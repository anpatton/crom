# Trade CROMalyzer -------------------------------------------------------------
# A hypothetical trade is just two baskets of players. CROM is intrinsic to
# each player (a pool-relative z-score sum), so a trade never re-scores anyone;
# the only thing that changes is each side's CROM total. These helpers build
# the result table and net-swing summary for the Trade CROMalyzer tab.

# Traditional evaluation palette: red = bad (low), green = good (high),
# diverging through white at 0. Used for ALL trade evaluation columns so a
# glance reads value the intuitive way, independent of the Players-tab hues.
TRADE_BAD <- "#D32F2F"
TRADE_GOOD <- "#2E7D32"

# Text color (black/white) with adequate contrast against a solid background.
contrast_text <- function(hex) {
  rgb <- grDevices::col2rgb(hex)
  luminance <- 0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]
  if (luminance > 140) "#000000" else "#FFFFFF"
}

# Detail table: one row per player in the trade, showing every glossary stat.
# Each stat column is filled with a single static color -- its identifying hue
# from the Players tab (score_col_colors()'s "high" anchor) -- so the columns
# read as the same color they do in the main table, without the value-based
# heatmap. `df` columns: Side, Player, Team, then the TRADE_EVAL_STATS columns.
trade_table <- function(df) {
  stat_cols <- intersect(names(TRADE_EVAL_STATS), names(df))

  # Header labels: identity columns keep their names; stats use display labels.
  disp <- names(df)
  disp[match(stat_cols, names(df))] <- TRADE_EVAL_STATS[stat_cols]

  dt <- DT::datatable(
    df,
    rownames = FALSE,
    colnames = disp,
    options = list(
      dom = "t",
      paging = FALSE,
      ordering = FALSE,
      columnDefs = list(list(className = "dt-center", targets = "_all"))
    ),
    class = "compact stripe hover"
  ) |>
    DT::formatRound(stat_cols, 1)

  for (col in stat_cols) {
    # diffCROM is a diverging (red<->green) column in the main table, so it has
    # no single identity hue -- give it a neutral gray fill instead.
    bg <- if (col == "diffCROM") LIGHT_GRAY else score_col_colors(col)[["high"]]
    dt <- dt |>
      DT::formatStyle(
        col,
        backgroundColor = bg,
        color = contrast_text(bg),
        fontWeight = "bold"
      )
  }
  dt
}

# The glossary composite stats evaluated in a trade, in display order. Names are
# the raw ranked() columns; values are the labels mirroring the Players-tab
# headers.
TRADE_EVAL_STATS <- c(
  CROM = "CROM",
  CRON = "CRON",
  xCROM = "xCROM [Pre]",
  xCROM_ROS = "xCROM [ROS]",
  diffCROM = "diffCROM"
)

# Single-row evaluation table: the net swing (your perspective) for every
# glossary stat. `nets` is a named numeric vector keyed by the raw stat columns
# (positive = you gain). Cells are colored red (you lose) -> green (you gain),
# diverging through white at 0 and self-scaled symmetrically to the row so the
# relative magnitude across stats reads at a glance.
trade_eval_table <- function(nets) {
  df <- as.data.frame(as.list(nets), check.names = FALSE)
  names(df) <- TRADE_EVAL_STATS[names(nets)]

  m <- suppressWarnings(max(abs(nets), na.rm = TRUE))
  anchor <- if (is.finite(m) && m > 0) c(-m, m) else c(-1, 1)

  DT::datatable(
    df,
    rownames = FALSE,
    options = list(
      dom = "t",
      paging = FALSE,
      ordering = FALSE,
      columnDefs = list(list(className = "dt-center", targets = "_all"))
    ),
    class = "compact stripe hover"
  ) |>
    DT::formatRound(names(df), 1) |>
    DT::formatStyle(
      names(df),
      backgroundColor = zscore_style(
        anchor,
        low = TRADE_BAD,
        high = TRADE_GOOD
      ),
      color = zscore_text_color(anchor, low = TRADE_BAD, high = TRADE_GOOD),
      fontWeight = "bold"
    )
}
