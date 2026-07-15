# Table styling ---------------------------------------------------------------
# Each column's heatmap is a diverging low -> mid -> high scale, anchored so
# a score of 0 (the pool's midpoint on the -5..5 scale) always lands on the
# neutral `mid` color, regardless of where 0 falls in that column's actual
# observed min/max:
#   - CRON keeps the original Padres-inspired brown/white/gold.
#   - Every other score column is colored by its stat category (see
#     score_col_category()) so a category's whole column group (actual,
#     x, Diff) shares one 70s-inspired hue -- CATEGORY_COLORS below.

PADRES_BROWN <- "#4A3226"
PADRES_GOLD <- "#F2B705"
LIGHT_GRAY <- "#D9D9D9"
NEUTRAL_MID <- "#FFFFFF"
N_SHADES <- 9
MUTED_GRAY <- "#E0E0E0"
MUTED_TEXT <- "#999999"

# Preseason-projection-sourced columns: the per-category "x" group
# (z_<cat>_x, e.g. xAB) plus the xCROM / xCROM_ROS composite scores. These
# are grayed out instead of heatmapped only when the actual stats they'd
# compare against are a rolling L15/L30/L60 window -- against those, a
# full-season projection is not directly comparable. When the Full Season
# window is selected the projections line up, so `mute_proj` is FALSE and
# they heatmap like any other column.
is_muted_col <- function(col, mute_proj = TRUE) {
  mute_proj &&
    (score_col_group(col) == "proj" || col %in% c("xCROM", "xCROM_ROS"))
}

# One 70s-inspired hue per category -- shared by every column in that
# category's group (e.g. AB, xAB, ΔAB all use the harvest gold
# gradient). Composite scores (CROM, xCROM, diffCROM) and the
# supplemental sabermetric stats each get their own hue too; CRON is
# handled separately in score_datatable() and isn't in this map.
CATEGORY_COLORS <- c(
  # Hitting core
  AB = "#D4A017",
  H = "#CC5500",
  R = "#6B8E23",
  RBI = "#B7410E",
  SBN = "#1D5C63",
  OBP = "#E1AD01",
  SLG = "#CB6D51",
  # Pitching core
  IP = "#D4A017",
  ERA = "#CC5500",
  WHIP = "#6B8E23",
  K = "#B7410E",
  W = "#1D5C63",
  SVH = "#E1AD01",
  # Composite scores
  CROM = "#6B6B3A",
  xCROM = "#6B3A4B",
  xCROM_ROS = "#3A5C6B",
  diffCROM = "#2F6F6A",
  # Supplemental sabermetrics (hitting + pitching)
  xwOBA = "#B87333",
  wRCplus = "#DAA520",
  WAR = "#A0522D",
  xAVG = "#C08081",
  xSLG = "#8A9A5B",
  FIP = "#B87333",
  xFIP = "#A0522D",
  xERA = "#C08081",
  SIERA = "#8A9A5B"
)

# Boosts a hex color's HSV brightness (and saturation slightly) so the top
# of a category's gradient reads as a vivid pop of color rather than a
# muted 70s tone -- used only at the high end, the base hue in
# CATEGORY_COLORS is left as-is.
brighten_color <- function(hex, amount = 0.18) {
  rgb <- grDevices::col2rgb(hex)[, 1]
  hsv <- grDevices::rgb2hsv(rgb[1], rgb[2], rgb[3])[, 1]
  grDevices::hsv(hsv["h"], min(1, hsv["s"] * 1.1), min(1, hsv["v"] + amount))
}

# Heatmap anchor colors (low/high, diverging through NEUTRAL_MID at 0) for a
# score column: CRON keeps the Padres brown/gold; diffCROM diverges red (under)
# to green (over); every other column is light gray -> its category's brightened
# hue, falling back to harvest gold if unmapped. Single source of truth shared
# by the table heatmap (score_datatable) and the glossary swatches.
score_col_colors <- function(col) {
  if (col == "CRON") {
    c(low = PADRES_BROWN, high = PADRES_GOLD)
  } else if (col == "diffCROM") {
    c(low = "#D32F2F", high = "#2E7D32")
  } else {
    c(
      low = LIGHT_GRAY,
      high = brighten_color(
        CATEGORY_COLORS[[score_col_category(col)]] %||% PADRES_GOLD
      )
    )
  }
}

# Diverging low -> mid -> high color for a single value `v`, where `mid`
# is pinned to v == 0 -- not to the midpoint of `rng`. Values on the
# negative side interpolate low->mid across [rng[1], 0]; positive-side
# values interpolate mid->high across [0, rng[2]]. If the whole range
# falls on one side of zero, only that side's ramp is used.
diverging_color_at <- function(v, rng, low, mid, high) {
  if (v <= 0) {
    if (rng[1] >= 0) {
      return(mid)
    }
    frac <- (v - rng[1]) / (0 - rng[1])
    grDevices::colorRampPalette(c(low, mid))(101)[[round(frac * 100) + 1]]
  } else {
    if (rng[2] <= 0) {
      return(mid)
    }
    frac <- v / rng[2]
    grDevices::colorRampPalette(c(mid, high))(101)[[round(frac * 100) + 1]]
  }
}

# n evenly-spaced representative colors across x's range, one per
# DT::styleInterval bin, via diverging_color_at().
diverging_colors <- function(rng, low, mid, high, n = N_SHADES) {
  cuts <- seq(rng[1], rng[2], length.out = n + 1)
  bin_mids <- (cuts[-1] + cuts[-(n + 1)]) / 2
  vapply(
    bin_mids,
    diverging_color_at,
    character(1),
    rng = rng,
    low = low,
    mid = mid,
    high = high
  )
}

# Returns a DT::styleInterval() background-color scale sized to x's range.
zscore_style <- function(
  x,
  low = LIGHT_GRAY,
  high = PADRES_GOLD,
  mid = NEUTRAL_MID,
  n = N_SHADES
) {
  # suppressWarnings: see scale_score()'s comment -- an all-NA column is an
  # anticipated case, handled by the is.finite() check right below.
  rng <- suppressWarnings(range(x, na.rm = TRUE))
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
    return(rep(mid, length(x)))
  }
  colors <- diverging_colors(rng, low, mid, high, n)
  cuts <- seq(rng[1], rng[2], length.out = n + 1)[2:n]
  DT::styleInterval(cuts, colors)
}

# Strips the z_ prefix and any _x/_Diff suffix, leaving the shared
# base category name (e.g. "z_AB_Diff" -> "AB"), used to detect where one
# stat category's column group ends and the next begins.
score_col_category <- function(col) {
  col |> sub("^z_", "", x = _) |> sub("_x$|_Diff$", "", x = _)
}

# First column of each contiguous same-category run within `cols`, in
# order -- these get a bold vertical rule to visually separate categories.
category_boundary_cols <- function(cols) {
  cats <- score_col_category(cols)
  cols[c(TRUE, cats[-1] != cats[-length(cats)])]
}

# Definitions for the composite score columns, shown in the glossary rendered
# under the table (see glossary_panel()). Keys are the raw column names; the
# glossary pairs each with a color swatch from score_col_colors().
COLUMN_TOOLTIPS <- c(
  CROM = "Based on stats that have actually occurred in games this season: sum of per-category z-scores, rescaled -5 (worst in pool) to +5 (best in pool).",
  CRON = "How similar you are to Jake Cronenworth, rescaled -5 (least) to +5 (most).",
  xCROM = "Same as CROM, but computed from preseason projections (avg of Steamer/ZiPS/THE BAT) instead of actual in-game stats.",
  xCROM_ROS = "Same as CROM, but computed from each system's rest-of-season projection (avg of Steamer/ZiPS/THE BAT ROS lines) instead of actual in-game stats.",
  diffCROM = "How much actual performance differs from the preseason projection (CROM minus xCROM [Pre]); positive means outperforming, negative means underperforming."
)

# A static legend rendered under the table. Each entry pairs a composite
# score's definition (COLUMN_TOOLTIPS) with a gradient swatch that mirrors its
# heatmap (low -> white -> high via score_col_colors()), so the glossary
# color-matches the table. Labels mirror the table's header text.
glossary_panel <- function() {
  entries <- list(
    list(col = "CROM", label = "CROM"),
    list(col = "CRON", label = "CRON"),
    list(col = "xCROM", label = "xCROM [Pre]"),
    list(col = "xCROM_ROS", label = "xCROM [ROS]"),
    list(col = "diffCROM", label = "diffCROM")
  )
  rows <- lapply(entries, function(e) {
    pair <- score_col_colors(e$col)
    swatch <- sprintf(
      "linear-gradient(to right, %s, %s, %s)",
      pair[["low"]],
      NEUTRAL_MID,
      pair[["high"]]
    )
    htmltools::tags$div(
      style = "display: flex; align-items: flex-start; gap: 8px; margin-bottom: 6px;",
      htmltools::tags$span(
        style = sprintf(
          paste(
            "flex: 0 0 auto; width: 46px; height: 22px; margin-top: 3px;",
            "border: 1px solid #bbb; border-radius: 2px; background: %s;"
          ),
          swatch
        )
      ),
      htmltools::tags$span(
        htmltools::tags$strong(e$label),
        " \u2014 ",
        COLUMN_TOOLTIPS[[e$col]]
      )
    )
  })
  # Always-visible glossary rendered above the table.
  htmltools::tags$div(
    class = "crom-glossary",
    style = "margin: 8px 0 16px; max-width: 860px; font-size: 14px; color: #444;",
    htmltools::tags$h5(
      "Glossary",
      style = "font-size: 17px; font-weight: 600; margin-bottom: 10px;"
    ),
    rows
  )
}

# Full category names for the top row of the grouped header (see
# grouped_header_sketch()). Keys are the short category codes returned by
# score_col_category() -- unique across hitting and pitching, so one flat
# lookup covers both.
CATEGORY_FULL_NAMES <- c(
  AB = "At Bats",
  H = "Hits",
  R = "Runs",
  RBI = "Runs Batted In",
  SBN = "Net Steals (SB-CS)",
  OBP = "On-Base Pct",
  SLG = "Slugging Pct",
  IP = "Innings Pitched",
  ERA = "Earned Run Avg",
  WHIP = "Walks+Hits/IP",
  K = "Strikeouts",
  W = "Wins",
  SVH = "Saves+Holds"
)

# Builds a 2-row <thead> for DT::datatable(container = ...): row 1 groups
# each stat category's columns (AB/xAB/ΔAB, ...) under one spanning
# header showing its full name (CATEGORY_FULL_NAMES); columns that aren't
# part of a multi-column category (Rank, Player, CROM, ...) just span
# both rows with their existing short header, unchanged. `tooltips`, if
# supplied, is a named vector (names = entries in `cols`) whose values are
# set as the native `title` attr on that column's single-column header.
grouped_header_sketch <- function(cols, headers, tooltips = NULL) {
  cats <- score_col_category(cols)
  runs <- rle(cats)

  top_row <- list()
  bottom_row <- list()
  i <- 1
  for (j in seq_along(runs$lengths)) {
    len <- runs$lengths[j]
    idx <- i:(i + len - 1)
    if (len > 1) {
      full_name <- CATEGORY_FULL_NAMES[[runs$values[j]]] %||% runs$values[j]
      top_row[[length(top_row) + 1]] <- htmltools::tags$th(
        colspan = len,
        full_name
      )
      for (k in idx) {
        bottom_row[[length(bottom_row) + 1]] <- htmltools::tags$th(headers[k])
      }
    } else {
      tip <- if (is.null(tooltips)) {
        NA_character_
      } else {
        unname(tooltips[cols[idx]])
      }
      th_args <- list(rowspan = 2, headers[idx])
      if (!is.na(tip)) {
        th_args$title <- tip
      }
      top_row[[length(top_row) + 1]] <- do.call(htmltools::tags$th, th_args)
    }
    i <- i + len
  }

  htmltools::withTags(table(
    class = "display",
    thead(
      tr(top_row),
      tr(bottom_row)
    )
  ))
}

# Builds a centered DT::datatable with the per-category heatmap applied to
# `score_cols` (rounded to 1 decimal) and "z_" stripped from headers, with
# "_Diff" shown as the delta symbol (e.g. z_AB_Diff -> "ABΔ"). A bold
# vertical rule marks the start of each stat category's column group.
# Used to style the Players table.
score_datatable <- function(
  df,
  score_cols,
  order_col = NULL,
  mute_proj = TRUE
) {
  opts <- list(
    pageLength = 25,
    dom = "tip",
    scrollX = TRUE,
    columnDefs = list(list(className = "dt-center", targets = "_all"))
  )
  if (!is.null(order_col)) {
    opts$order <- list(list(which(names(df) == order_col) - 1, "desc"))
  }

  # Render the Team column's abbreviation as a logo for display while keeping
  # the raw abbreviation as the underlying value, so column sorting and the
  # (optional) filter operate on "NYY" rather than an <img> HTML string.
  if ("Team" %in% names(df)) {
    logo_map <- jsonlite::toJSON(as.list(TEAM_LOGO_ID), auto_unbox = TRUE)
    team_render <- DT::JS(sprintf(
      "function(data, type, row) {
         if (type !== 'display' || data == null) return data;
         var m = %s, id = m[data];
         if (!id) return data;
         return '<img src=\"https://www.mlbstatic.com/team-logos/' + id +
           '.svg\" height=\"24\" alt=\"' + data + '\" title=\"' + data + '\">';
       }",
      logo_map
    ))
    opts$columnDefs <- c(
      opts$columnDefs,
      list(list(targets = which(names(df) == "Team") - 1, render = team_render))
    )
  }

  # The "IL" flag is data-only: hidden from view, it just drives the dark-red
  # coloring of injured players' names (see formatStyle below).
  if ("IL" %in% names(df)) {
    opts$columnDefs <- c(
      opts$columnDefs,
      list(list(visible = FALSE, targets = which(names(df) == "IL") - 1))
    )
  }

  # Every column gets a per-column filter box (filter = "top" below) except
  # the Headshot image column: its cell holds an <img> tag that can't be
  # meaningfully searched or sorted, so disable both there.
  if ("Headshot" %in% names(df)) {
    opts$columnDefs <- c(
      opts$columnDefs,
      list(list(
        searchable = FALSE,
        orderable = FALSE,
        targets = which(names(df) == "Headshot") - 1
      ))
    )
  }

  headers <- names(df) |>
    sub("^z_", "", x = _) |>
    sub("^(.*)_Diff$", "\u0394\\1", x = _) |>
    sub("^(.*)_x$", "x\\1", x = _) |>
    sub("^xCROM_ROS$", "xCROM [ROS]", x = _) |>
    sub("^xCROM$", "xCROM [Pre]", x = _) |>
    sub("^Fantrax$", "Bly", x = _)

  # Drop the "Headshot" label so the headshot reads as part of the adjacent
  # Player column rather than its own titled column.
  headers[names(df) == "Headshot"] <- ""

  dt <- DT::datatable(
    df,
    rownames = FALSE,
    filter = "top",
    colnames = headers,
    # Team is a plain abbreviation rendered to a logo client-side (see
    # team_render), so its data is safe to escape; only the pre-built Headshot
    # <img> HTML (when present) must skip escaping.
    escape = if ("Headshot" %in% names(df)) {
      -which(names(df) == "Headshot")
    } else {
      TRUE
    },
    options = opts,
    class = "compact stripe hover"
  ) |>
    DT::formatRound(score_cols, 1)

  # Left-justify the headshot image; every other column stays centered via the
  # dt-center columnDef applied to all targets above.
  if ("Headshot" %in% names(df)) {
    dt <- dt |>
      DT::formatStyle("Headshot", textAlign = "left")
  }

  # Slightly bold player names to set them apart from the numeric columns.
  if ("Player" %in% names(df)) {
    dt <- dt |>
      DT::formatStyle("Player", fontWeight = 500)
  }

  # Color the name of injured-reserve players dark red, driven by the hidden
  # "IL" flag column (see the visible = FALSE columnDef above).
  if (all(c("Player", "IL") %in% names(df))) {
    dt <- dt |>
      DT::formatStyle(
        "Player",
        valueColumns = "IL",
        color = DT::styleEqual("IL", "#8B0000", default = NULL)
      )
  }

  # Boundaries only among the z_* stat columns -- CROM/CRON/xCROM
  # are single composite scores, not part of a same-category column group.
  boundary_cols <- category_boundary_cols(grep("^z_", score_cols, value = TRUE))
  if (length(boundary_cols) > 0) {
    dt <- dt |>
      DT::formatStyle(boundary_cols, `border-left` = "3px solid #333333")
  }

  for (col in score_cols) {
    if (is_muted_col(col, mute_proj)) {
      dt <- dt |>
        DT::formatStyle(
          col,
          backgroundColor = MUTED_GRAY,
          color = MUTED_TEXT,
          fontWeight = "normal"
        )
      next
    }

    # Anchor colors come from score_col_colors() so the table heatmap and the
    # glossary swatches stay in lockstep. All diverge through NEUTRAL_MID
    # (white) at a score of 0.
    pair <- score_col_colors(col)
    low <- pair[["low"]]
    high <- pair[["high"]]

    dt <- dt |>
      DT::formatStyle(
        col,
        backgroundColor = zscore_style(df[[col]], low, high),
        color = zscore_text_color(df[[col]], low, high),
        fontWeight = "bold"
      )
  }

  # DT emits a filter <input> for every column even when searchable = FALSE, so
  # the image column would still show a dead search box. Blank just that one
  # cell (matched positionally among the row's <td> cells) while leaving every
  # other column's filter -- and the surrounding <tr> -- intact.
  if ("Headshot" %in% names(df) && !is.null(dt$x$filterHTML)) {
    k <- which(names(df) == "Headshot")
    m <- gregexpr("(?s)<td\\b.*?</td>", dt$x$filterHTML, perl = TRUE)[[1]]
    if (m[1] != -1 && length(m) == ncol(df)) {
      st <- m[k]
      en <- m[k] + attr(m, "match.length")[k] - 1
      dt$x$filterHTML <- paste0(
        substr(dt$x$filterHTML, 1, st - 1),
        "<td></td>",
        substr(dt$x$filterHTML, en + 1, nchar(dt$x$filterHTML))
      )
    }
  }

  dt
}

# Matching text color (black/white) chosen for contrast against each shade.
zscore_text_color <- function(
  x,
  low = LIGHT_GRAY,
  high = PADRES_GOLD,
  mid = NEUTRAL_MID,
  n = N_SHADES
) {
  # suppressWarnings: see scale_score()'s comment -- an all-NA column is an
  # anticipated case, handled by the is.finite() check right below.
  rng <- suppressWarnings(range(x, na.rm = TRUE))
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
    return(rep("#000000", length(x)))
  }
  colors <- diverging_colors(rng, low, mid, high, n)
  rgb <- grDevices::col2rgb(colors)
  luminance <- 0.299 * rgb[1, ] + 0.587 * rgb[2, ] + 0.114 * rgb[3, ]
  text_colors <- ifelse(luminance > 140, "#000000", "#FFFFFF")
  cuts <- seq(rng[1], rng[2], length.out = n + 1)[2:n]
  DT::styleInterval(cuts, text_colors)
}
