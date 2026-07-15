# CROM — Fantasy Baseball Rankings
# Sources: (1) MLB Stats API + (2) FanGraphs, both via baseballr | (3) Fantrax (next)
# Scoring: 5x5 roto z-score composite | Window: Full Season/L15/L30/L60 (toggle, default Full Season)
# Tables: DT | Data layer: R/fangraphs.R, R/rankings.R

suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(dplyr)
})

# Explicitly source the data layer (also auto-sourced by Shiny from R/).
# R/utils.R defines the `%||%` operator used throughout.
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

# ---- UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(
    div(
      img(
        src = "crom.png",
        style = "height: 2.5em; margin-right: 10px; vertical-align: middle;"
      ),
      "CROM | Calculated Relativeishness of the Mean",
      div(
        "...it's a perfectly cromulent metric",
        style = "font-size: 14px; font-weight: normal; color: #888;"
      )
    ),
    windowTitle = "CROM"
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      radioButtons(
        "player_type",
        "Player type",
        choices = c("Hitters" = "hitting", "Pitchers" = "pitching"),
        selected = "hitting"
      ),
      radioButtons(
        "window",
        "Stat window",
        choices = c(
          "Full Season" = "season",
          "L15" = "15",
          "L30" = "30",
          "L60" = "60"
        ),
        selected = "season",
        inline = TRUE
      ),
      numericInput(
        "min_qual",
        "Min PA (hitters) / IP (pitchers) \u2014 0 = all who played",
        value = 0,
        min = 0,
        step = 5
      ),
      radioButtons(
        "weighting",
        "Rate stats (OBP/SLG, ERA/WHIP)",
        choices = c("Adjusted (per PA/IP)" = "adjusted", "Raw" = "raw"),
        selected = "adjusted",
        inline = TRUE
      ),
      helpText(textOutput("status", inline = TRUE)),
      tags$hr(),
      actionButton(
        "refresh_ownership",
        "Refresh Fantrax ownership",
        icon = icon("rotate"),
        class = "btn-sm btn-block"
      ),
      helpText(textOutput("ownership_status", inline = TRUE))
    ),
    mainPanel(
      width = 9,
      br(),
      tabsetPanel(
        tabPanel(
          "Players",
          glossary_panel(),
          shinycssloaders::withSpinner(
            DT::DTOutput("rankings_tbl"),
            type = 8,
            color = PADRES_GOLD
          )
        ),
        tabPanel(
          "Trade CROMalyzer",
          br(),
          helpText(
            "Pick the players each side sends. CROM (and the other stats) are ",
            "intrinsic to each player, so nothing is re-scored \u2014 the tables ",
            "below show the stats changing hands and your net swing per stat ",
            "(green = you gain, red = you lose)."
          ),
          fluidRow(
            column(
              6,
              selectizeInput(
                "trade_a",
                "You Send Out...",
                choices = NULL,
                multiple = TRUE,
                width = "100%"
              )
            ),
            column(
              6,
              selectizeInput(
                "trade_b",
                "They Send Back...",
                choices = NULL,
                multiple = TRUE,
                width = "100%"
              )
            )
          ),
          actionButton(
            "clear_trade",
            "Clear trade",
            icon = icon("xmark"),
            class = "btn-sm"
          ),
          tags$h5(
            "Trade evaluation (your perspective)",
            style = "font-weight: 600; margin-top: 18px;"
          ),
          DT::DTOutput("trade_eval"),
          tags$h5(
            "Players in the trade",
            style = "font-weight: 600; margin-top: 18px;"
          ),
          shinycssloaders::withSpinner(
            DT::DTOutput("trade_tbl"),
            type = 8,
            color = PADRES_GOLD
          )
        )
      )
    )
  )
)

# ---- Server ------------------------------------------------------------------
server <- function(input, output, session) {
  # Season-to-date leaderboard for the selected player type / window, read
  # from the static daily snapshot bundled with the app (data/). The snapshot
  # is produced offline by data_refresh.R (cron, 7am ET); the app never
  # fetches live. Switching player type or window just reads a different
  # bundled file. min_qual is applied downstream.
  raw_data <- reactive({
    read_fg_leaders(input$player_type, window = input$window)
  })

  # Preseason projections (3-system average), from the bundled snapshot.
  proj_data <- reactive({
    read_fg_projections(input$player_type)
  })

  # Rest-of-season projections (3-system average), from the bundled snapshot.
  ros_data <- reactive({
    read_fg_ros_projections(input$player_type)
  })

  ranked <- reactive({
    df <- raw_data()
    validate(need(
      !is.null(df) && nrow(df) > 0,
      paste("Could not load FanGraphs data.", attr(df, "err", exact = TRUE))
    ))
    # Baseline pool is everyone who has actually appeared (hitters PA >= 1,
    # pitchers IP > 0) -- no innings qualifier. Impact weighting (use_impact)
    # scales the rate stats by opportunity, so small-sample lines can't
    # distort the z-scores. min_qual is an optional extra floor (0 = none).
    df <- if (input$player_type == "hitting") {
      dplyr::filter(df, .data$PA >= 1)
    } else {
      dplyr::filter(df, .data$IP > 0)
    }
    build_rankings(
      df,
      input$player_type,
      input$min_qual %||% 0,
      use_impact = (input$weighting %||% "adjusted") == "adjusted",
      proj_df = proj_data(),
      ros_proj_df = ros_data()
    )
  })

  # A live ownership pull held in memory. The app normally reads the bundled
  # daily snapshot, but the "Refresh Fantrax ownership" button fetches the
  # current rosters straight from Fantrax and stashes them here (no filesystem
  # write, which is unreliable on deploy). When set, it takes precedence.
  own_override <- reactiveVal(NULL)
  own_refreshed_at <- reactiveVal(NULL)

  observeEvent(input$refresh_ownership, {
    res <- withProgress(
      message = "Refreshing Fantrax ownership...",
      value = 0.5,
      tryCatch(fetch_fantrax_ownership(), error = function(e) e)
    )
    ok <- !inherits(res, "error") && !is.null(res) && nrow(res) > 0
    if (ok) {
      own_override(res)
      own_refreshed_at(Sys.time())
      showNotification(
        sprintf(
          "Fantrax ownership refreshed (%d rostered players).",
          nrow(res)
        ),
        type = "message"
      )
    } else {
      msg <- if (inherits(res, "error")) {
        conditionMessage(res)
      } else {
        "no data returned"
      }
      showNotification(paste("Ownership refresh failed:", msg), type = "error")
    }
  })

  output$ownership_status <- renderText({
    t <- own_refreshed_at()
    if (is.null(t)) {
      "Ownership: bundled daily snapshot"
    } else {
      paste("Ownership refreshed at", format(t, "%H:%M:%S"))
    }
  })

  # Fantrax ownership lookup, keyed on MLBAM id (CROM's Headshot/xMLBAMID).
  # Uses the live override when present, else the bundled snapshot
  # (data/fantrax_ownership.rds). Yields one row per rostered player mapping
  # mlbam_id -> fantasy_team; players absent are free agents ("FA") downstream.
  ownership_map <- reactive({
    own <- own_override() %||% read_fantrax_ownership()
    if (is.null(own) || nrow(own) == 0) {
      return(tibble(
        Headshot = integer(),
        Fantrax = character(),
        Pos = character(),
        IL = character()
      ))
    }
    own |>
      filter(!is.na(mlbam_id)) |>
      distinct(mlbam_id, .keep_all = TRUE) |>
      transmute(
        Headshot = mlbam_id,
        # Some teams name themselves with a bare emoji, which breaks the DT
        # widget's client-side JSON payload; substitute plain-text labels.
        Fantrax = fantasy_team |>
          gsub("\U0001F95E", "Pancakes", x = _) |>
          gsub("\U0001FA84", "DISNEYBALL", x = _),
        # Collapse the corner-outfield positions into a single "OF".
        Pos = dplyr::recode(mlb_pos, LF = "OF", RF = "OF"),
        # Injured-reserve flag; drives the dark-red styling of the player's
        # name (kept as a hidden column, not shown directly).
        IL = ifelse(status == "INJURED_RESERVE", "IL", "")
      )
  })

  output$status <- renderText({
    df <- ranked()
    window_label <- if (input$window == "season") {
      "full season"
    } else {
      paste0("L", input$window, " days")
    }
    # Snapshot generation time (set by data_refresh.R), shown in ET.
    meta <- read_snapshot_meta()
    updated_label <- if (!is.null(meta) && !is.null(meta$generated_at)) {
      format(meta$generated_at, "%b %d %I:%M %p ET", tz = "America/New_York")
    } else {
      "unknown"
    }
    sprintf(
      "%d players | %s | data as of %s",
      nrow(df),
      window_label,
      updated_label
    )
  })

  output$rankings_tbl <- DT::renderDT({
    # Column toggles were removed; always show all projection groups.
    visible_groups <- c("actual", "proj", "diff")
    score_cols <- filter_score_cols(
      core_cols_with_proj(input$player_type),
      visible_groups
    )

    validate(need(
      nrow(ranked()) > 0,
      "No players meet the current qualifier. Lower the Min PA/IP threshold."
    ))

    df <- ranked() |>
      left_join(ownership_map(), by = "Headshot") |>
      mutate(
        Fantrax = tidyr::replace_na(Fantrax, "FA"),
        Pos = tidyr::replace_na(Pos, "FA"),
        IL = tidyr::replace_na(IL, "")
      ) |>
      select(
        Team,
        Player,
        Fantrax,
        Pos,
        Rank,
        IL,
        G,
        CROM,
        CRON,
        xCROM,
        xCROM_ROS,
        diffCROM,
        all_of(score_cols)
      ) |>
      # Factors give the column filters clean dropdowns of distinct values
      # instead of free-text search boxes.
      mutate(across(c(Fantrax, Pos, Team), as.factor))

    score_datatable(
      df,
      c(score_cols, "CROM", "xCROM", "xCROM_ROS", "diffCROM", "CRON"),
      order_col = "CROM",
      # Full-season actuals line up with the (full-season) projections, so
      # don't gray the projection columns; only mute them for rolling windows.
      mute_proj = input$window != "season"
    )
  })

  # ---- Trade CROMalyzer ----
  # Full player pool for the pickers (server-side, since the list is ~600 long).
  # Values are Headshot ids so duplicate names stay unambiguous; labels show
  # "Player (TEAM)".
  all_choices <- reactive({
    df <- ranked()
    stats::setNames(df$Headshot, sprintf("%s (%s)", df$Player, df$Team))
  })

  # Keep the two sides mutually exclusive: a player picked on one side is
  # dropped from the other side's options, and selectize already blocks picking
  # the same player twice within a side. Each observer preserves its own current
  # selection on update, so the cross-refresh can't change an input's value and
  # therefore can't loop.
  observe({
    avail <- all_choices()
    avail <- avail[!(avail %in% as.integer(input$trade_b))]
    updateSelectizeInput(
      session,
      "trade_a",
      choices = avail,
      selected = input$trade_a,
      server = TRUE
    )
  })
  observe({
    avail <- all_choices()
    avail <- avail[!(avail %in% as.integer(input$trade_a))]
    updateSelectizeInput(
      session,
      "trade_b",
      choices = avail,
      selected = input$trade_b,
      server = TRUE
    )
  })

  # Reset both sides at once. Clearing the selections lets the exclusivity
  # observers above restore each picker's full option list.
  observeEvent(input$clear_trade, {
    updateSelectizeInput(session, "trade_a", selected = character(0))
    updateSelectizeInput(session, "trade_b", selected = character(0))
  })

  # Tidy frame of every player in the hypothetical trade: Side, Player, Team,
  # and all glossary composite stats ("You" rows then "They" rows).
  trade_rows <- reactive({
    df <- ranked()
    stat_cols <- names(TRADE_EVAL_STATS)
    pick <- function(ids, side) {
      if (length(ids) == 0) {
        return(NULL)
      }
      df |>
        filter(Headshot %in% as.integer(ids)) |>
        select(Player, Team, all_of(stat_cols)) |>
        mutate(Side = side, .before = 1)
    }
    bind_rows(pick(input$trade_a, "You"), pick(input$trade_b, "They"))
  })

  # Single-row net swing for every glossary stat, from your perspective: you
  # receive what They send back and give up what You send out, so
  # net = sum(They) - sum(You) for each stat.
  output$trade_eval <- DT::renderDT({
    rows <- trade_rows()
    validate(need(
      !is.null(rows) && nrow(rows) > 0,
      "Select players on each side to evaluate a trade."
    ))
    stat_cols <- names(TRADE_EVAL_STATS)
    side_sum <- function(side) {
      sub <- rows[rows$Side == side, stat_cols, drop = FALSE]
      vapply(sub, function(v) sum(v, na.rm = TRUE), numeric(1))
    }
    nets <- side_sum("They") - side_sum("You")
    trade_eval_table(nets)
  })

  output$trade_tbl <- DT::renderDT({
    rows <- trade_rows()
    validate(need(
      !is.null(rows) && nrow(rows) > 0,
      "Select players on each side to evaluate a trade."
    ))
    trade_table(rows)
  })
}

shinyApp(ui, server)
