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
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

# ---- UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(div(
    img(src = "crom.png", height = "40px", style = "margin-right: 10px;"),
    "CROM | Calculated Relativeishness of the Mean",
    div(
      "...it's a perfectly cromulent metric",
      style = "font-size: 14px; font-weight: normal; color: #888;"
    )
  )),
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
        choices = c("Full Season" = "season", "L15" = "15", "L30" = "30", "L60" = "60"),
        selected = "season",
        inline = TRUE
      ),
      numericInput(
        "min_qual",
        "Min PA (hitters) / IP (pitchers)",
        value = 50,
        min = 0,
        step = 5
      ),
      actionButton("refresh", "Refresh data", icon = icon("rotate")),
      helpText(textOutput("status", inline = TRUE)),
      tags$hr(),
      strong("Players tab columns"),
      radioButtons("show_actual", "Actual", choices = c("Show", "Hide"), selected = "Show", inline = TRUE),
      radioButtons("show_proj", "x (projected avg)", choices = c("Show", "Hide"), selected = "Show", inline = TRUE),
      radioButtons("show_diff", "\u0394 (actual - proj)", choices = c("Show", "Hide"), selected = "Show", inline = TRUE),
      radioButtons("show_pct", "\u0394% (diff / proj)", choices = c("Show", "Hide"), selected = "Show", inline = TRUE)
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Players", br(), DT::DTOutput("rankings_tbl")),
        tabPanel("Advanced Stats", br(), DT::DTOutput("advanced_tbl")),
        tabPanel("Roster", br(), p("Coming soon.")),
        tabPanel("Standings", br(), p("Coming soon."))
      )
    )
  )
)

# Sensible default qualifier thresholds, scaled to each stat window (PA/IP
# accumulate roughly linearly with days played) -- L15's values match the
# app's original single hitting/pitching defaults (50 PA / 20 IP); "season"
# is set high enough to clear early-season noise without being a true
# full-season qualifying bar (502 PA / 162 IP), since this is season-to-date.
MIN_QUAL_DEFAULTS <- list(
  hitting  = c(season = 300, `15` = 50, `30` = 100, `60` = 200),
  pitching = c(season = 100, `15` = 20, `30` = 40,  `60` = 80)
)

# ---- Server ------------------------------------------------------------------
server <- function(input, output, session) {

  # Sensible default qualifier threshold when player type or stat window
  # changes.
  observeEvent(list(input$player_type, input$window), {
    default <- MIN_QUAL_DEFAULTS[[input$player_type]][[input$window]]
    updateNumericInput(session, "min_qual", value = default)
  })

  # Raw FanGraphs pull (Full Season or a rolling L15/L30/L60 window),
  # disk-cached per player_type per window per day (see
  # fetch_fg_leaders_cached). Only the Refresh button forces a re-pull;
  # switching player type or window just reads today's cached file for that
  # combination if one exists. min_qual is applied downstream so it doesn't
  # re-scrape.
  last_refresh <- reactiveVal(0)

  raw_data <- reactive({
    cur <- input$refresh
    force <- cur > isolate(last_refresh())
    isolate(last_refresh(cur))
    tryCatch(
      fetch_fg_leaders_cached(input$player_type, window = input$window, force_refresh = force),
      error = function(e) structure(NULL, err = conditionMessage(e))
    )
  })

  # Preseason projections, averaged across 3 FanGraphs systems (Steamer,
  # ZiPS, THE BAT), same disk-cache-per-day-per-player_type story as above.
  last_refresh_proj <- reactiveVal(0)

  proj_data <- reactive({
    cur <- input$refresh
    force <- cur > isolate(last_refresh_proj())
    isolate(last_refresh_proj(cur))
    tryCatch(
      fetch_fg_projections_cached(input$player_type, force_refresh = force),
      error = function(e) NULL
    )
  })

  # Rest-of-season projections, same 3-system-average / disk-cache story as
  # proj_data above, just sourced from each system's ROS line instead of its
  # preseason line (see fetch_fg_ros_projections).
  last_refresh_ros <- reactiveVal(0)

  ros_data <- reactive({
    cur <- input$refresh
    force <- cur > isolate(last_refresh_ros())
    isolate(last_refresh_ros(cur))
    tryCatch(
      fetch_fg_ros_projections_cached(input$player_type, force_refresh = force),
      error = function(e) NULL
    )
  })

  ranked <- reactive({
    df <- raw_data()
    validate(need(!is.null(df) && nrow(df) > 0,
                  paste("Could not load FanGraphs data.",
                        attr(df, "err", exact = TRUE))))
    build_rankings(df, input$player_type, input$min_qual %||% 0,
                   use_impact = TRUE, proj_df = proj_data(), ros_proj_df = ros_data())
  })

  # Placeholder "Bly" column, to eventually hold Fantrax roster/team data.
  # Randomly labels 340 players 1-17 with each number used exactly 20 times;
  # tied to raw_data (not min_qual) so labels don't reshuffle as the
  # qualifier slider changes.
  bly_map <- reactive({
    df <- raw_data()
    validate(need(!is.null(df) && nrow(df) > 0, ""))
    n <- min(nrow(df), 340)
    tibble(
      Player = sample(df$PlayerName, n),
      Bly = sample(rep(1:17, each = 20)[seq_len(n)])
    )
  })

  output$status <- renderText({
    df <- ranked()
    window_label <- if (input$window == "season") "full season" else paste0("L", input$window, " days")
    sprintf("%d players | %s | updated %s",
            nrow(df), window_label, format(Sys.time(), "%b %d %H:%M"))
  })

  output$rankings_tbl <- DT::renderDT({
    visible_groups <- c("actual", "proj", "diff", "pct")[
      c(input$show_actual, input$show_proj, input$show_diff, input$show_pct) == "Show"
    ]
    score_cols <- filter_score_cols(core_cols_with_proj(input$player_type), visible_groups)

    df <- ranked() |>
      left_join(bly_map(), by = "Player") |>
      select(Bly, Rank, Headshot, Player, Team, CROM, xCROM, xCROM_ROS, diffCROM, CRON, all_of(score_cols))
    df$Team <- team_logo_html(df$Team)
    df$Headshot <- player_headshot_html(df$Headshot)

    score_datatable(df, c(score_cols, "CROM", "xCROM", "xCROM_ROS", "diffCROM", "CRON"), order_col = "CROM")
  })

  # Supplemental sabermetric context (not part of CROM) on its own tab.
  output$advanced_tbl <- DT::renderDT({
    supp_cols <- SUPP_Z_COLS[[input$player_type]]
    df <- ranked() |>
      select(Rank, Headshot, Player, Team, all_of(supp_cols))
    df$Team <- team_logo_html(df$Team)
    df$Headshot <- player_headshot_html(df$Headshot)

    score_datatable(df, supp_cols)
  })
}

shinyApp(ui, server)
