# Table styling ---------------------------------------------------------------
# Each column's heatmap is a diverging low -> mid -> high scale, anchored so
# a score of 0 (the pool's midpoint on the -5..5 scale) always lands on the
# neutral `mid` color, regardless of where 0 falls in that column's actual
# observed min/max:
#   - CRON keeps the original Padres-inspired brown/white/gold.
#   - Every other score column is colored by its stat category (see
#     score_col_category()) so a category's whole column group (actual,
#     x, Diff, Pct) shares one 70s-inspired hue -- CATEGORY_COLORS below.

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
# category's group (e.g. AB, xAB, ΔAB, Δ%AB all use the harvest gold
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

# Strips the z_ prefix and any _x/_Diff/_Pct suffix, leaving the shared
# base category name (e.g. "z_AB_Diff" -> "AB"), used to detect where one
# stat category's column group ends and the next begins.
score_col_category <- function(col) {
  col |> sub("^z_", "", x = _) |> sub("_x$|_Diff$|_Pct$", "", x = _)
}

# First column of each contiguous same-category run within `cols`, in
# order -- these get a bold vertical rule to visually separate categories.
category_boundary_cols <- function(cols) {
  cats <- score_col_category(cols)
  cols[c(TRUE, cats[-1] != cats[-length(cats)])]
}

# Header tooltips (native title attr) for the 4 composite score columns --
# the first "real data" columns after the identity columns (Rank, Headshot,
# Player, Team). Per-category z-score columns aren't tooltipped.
COLUMN_TOOLTIPS <- c(
  CROM = "Based on stats that have actually occurred in games this season: sum of per-category z-scores, rescaled -5 (worst in pool) to +5 (best in pool).",
  xCROM = "Same as CROM, but computed from preseason projections (avg of Steamer/ZiPS/THE BAT) instead of actual in-game stats.",
  xCROM_ROS = "Same as CROM, but computed from each system's rest-of-season projection (avg of Steamer/ZiPS/THE BAT ROS lines) instead of actual in-game stats."
)

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
# each stat category's columns (AB/xAB/ΔAB/Δ%AB, ...) under one spanning
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
# Shared by the Players and Advanced Stats tabs so both style identically.
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

  headers <- names(df) |>
    sub("^z_", "", x = _) |>
    sub("^(.*)_Diff$", "\u0394\\1", x = _) |>
    sub("^(.*)_x$", "x\\1", x = _) |>
    sub("^(.*)_Pct$", "\u0394%\\1", x = _) |>
    sub("^xCROM_ROS$", "xCROM [ROS]", x = _) |>
    sub("^xCROM$", "xCROM [Pre]", x = _)

  dt <- DT::datatable(
    df,
    rownames = FALSE,
    container = grouped_header_sketch(
      names(df),
      headers,
      tooltips = COLUMN_TOOLTIPS
    ),
    escape = -which(names(df) %in% c("Team", "Headshot")),
    options = opts,
    class = "compact stripe hover"
  ) |>
    DT::formatRound(score_cols, 1)

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

    # CRON keeps the original Padres brown/gold as its low/high anchors;
    # every other column is light gray/its category's hue (CATEGORY_COLORS),
    # falling back to harvest gold if a category is somehow unmapped. Both
    # diverge through NEUTRAL_MID (white) at a score of 0.
    if (col == "CRON") {
      low <- PADRES_BROWN
      high <- PADRES_GOLD
    } else {
      low <- LIGHT_GRAY
      high <- brighten_color(
        CATEGORY_COLORS[[score_col_category(col)]] %||% PADRES_GOLD
      )
    }

    dt <- dt |>
      DT::formatStyle(
        col,
        backgroundColor = zscore_style(df[[col]], low, high),
        color = zscore_text_color(df[[col]], low, high),
        fontWeight = "bold"
      )
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
