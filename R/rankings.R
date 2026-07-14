# Roto z-score engine --------------------------------------------------------
# Internally: CROM = sum of the per-category z-scores for the core roto
# categories (xCROM is the same sum computed from preseason projections
# instead of actuals). Every z_* column, CROM, and xCROM are then
# rescaled (scale_score) to a -5 (worst in the qualified pool) - +5 (best in
# the pool) display scale.
#
#   Hitters : AB, H, R, RBI (counting, higher better) + SBN = SB-CS (counting)
#             + OBP, SLG (rate)
#   Pitchers: IP, K, W, SVH = SV+HLD (counting, higher better) + ERA, WHIP
#             (rate, LOWER better -> flipped so higher z is always better)
#
# use_impact = TRUE volume-weights the rate stats before z-scoring (so a .400
# OBP in 10 PA, or a 0.00 ERA in 3 IP, doesn't dominate the pool) by
# converting each rate into "units better than league" scaled by opportunity,
# then z-scoring that impact. When FALSE, the raw rate stat is z-scored as-is.
#
# Supplemental sabermetric scores (xwOBA, wRC+, WAR, xAVG, xSLG for hitters;
# FIP, WAR, xFIP, xERA, SIERA for pitchers) are shown alongside CROM for
# context but are NOT included in the CROM total.

suppressPackageStartupMessages({
  library(dplyr)
})

# z-score helper: returns 0s for a degenerate (zero-variance) column.
z <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - m) / s
}

# "CRON" similarity score ----------------------------------------------------
# Euclidean distance, in the pool's core z-score space, from each player to
# Jake Cronenworth -- then flipped and rescaled to -5 (least similar) - +5
# (most similar), same display convention as the z-score columns.
CRON_PLAYER <- "Jake Cronenworth"

add_cron_similarity <- function(df, cols, target_name = CRON_PLAYER, out_col = "CRON") {
  idx <- which(df$PlayerName == target_name)
  if (length(idx) == 0) {
    df[[out_col]] <- NA_real_
    return(df)
  }
  target_vec <- as.numeric(df[idx[1], cols, drop = TRUE])
  mat <- as.matrix(df[, cols])
  dist <- sqrt(rowSums(sweep(mat, 2, target_vec, "-")^2))

  rng <- range(dist, na.rm = TRUE)
  df[[out_col]] <- if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
    rep(5, length(dist))
  } else {
    5 - (dist - rng[1]) / (rng[2] - rng[1]) * 10
  }
  df
}

# Per-category actual-vs-projection definitions --------------------------------
# One entry per core scoring category: how to pull the actual value and the
# averaged projected value (see fetch_fg_projections) out of the joined df,
# and which direction is "better" (flip = -1 for ERA/WHIP, where lower is
# better, matching the sign convention already used for z_ERA/z_WHIP).
# Rate categories always compare the raw rate (not the use_impact-weighted
# input) since a preseason projection has no current-PA context to
# volume-weight against.
CATEGORY_DEFS <- list(
  hitting = list(
    AB  = list(actual = function(df) df$AB,                        proj = function(df) df$proj_AB,                        flip = 1),
    H   = list(actual = function(df) df$H,                         proj = function(df) df$proj_H,                         flip = 1),
    R   = list(actual = function(df) df$R,                         proj = function(df) df$proj_R,                         flip = 1),
    RBI = list(actual = function(df) df$RBI,                       proj = function(df) df$proj_RBI,                       flip = 1),
    SBN = list(actual = function(df) df$SB - coalesce(df$CS, 0),   proj = function(df) df$proj_SB - coalesce(df$proj_CS, 0), flip = 1),
    OBP = list(actual = function(df) df$OBP,                       proj = function(df) df$proj_OBP,                       flip = 1),
    SLG = list(actual = function(df) df$SLG,                       proj = function(df) df$proj_SLG,                       flip = 1)
  ),
  pitching = list(
    IP   = list(actual = function(df) df$IP,                             proj = function(df) df$proj_IP,                             flip = 1),
    ERA  = list(actual = function(df) df$ERA,                            proj = function(df) df$proj_ERA,                            flip = -1),
    WHIP = list(actual = function(df) df$WHIP,                           proj = function(df) df$proj_WHIP,                           flip = -1),
    K    = list(actual = function(df) df$SO,                             proj = function(df) df$proj_SO,                             flip = 1),
    W    = list(actual = function(df) df$W,                              proj = function(df) df$proj_W,                              flip = 1),
    SVH  = list(actual = function(df) df$SV + coalesce(df$HLD, 0),       proj = function(df) df$proj_SV + coalesce(df$proj_HLD, 0), flip = 1)
  )
)

# For each core category z_<cat>, the 3 column names of its projection
# comparison siblings, in display order: projected avg ("x", matching the
# app's expected/projected naming convention -- CRON, xCROM, xwOBA...),
# actual-minus-projected diff, and that diff as a fraction of the projection.
category_proj_cols <- function(cat) paste0(cat, c("_x", "_Diff", "_Pct"))

# Adds z_<cat>_x / _Diff / _Pct for every core category (raw, not yet
# -5..5 scaled -- build_rankings()'s scale_score call picks them up via its
# "^z_" grep same as the existing z_* columns).
add_category_projection_cols <- function(df, player_type) {
  for (cat in names(CATEGORY_DEFS[[player_type]])) {
    def <- CATEGORY_DEFS[[player_type]][[cat]]
    actual <- def$actual(df) * def$flip
    proj   <- def$proj(df) * def$flip
    cols <- paste0("z_", category_proj_cols(cat))
    df[[cols[1]]] <- proj
    df[[cols[2]]] <- actual - proj
    df[[cols[3]]] <- ifelse(proj != 0, (actual - proj) / abs(proj), NA_real_)
  }
  df
}

# "xCROM" preseason score ---------------------------------------------------
# Joins the 3-system-averaged preseason projection (see fetch_fg_projections)
# onto the currently qualified pool by MLBAM id, z-scores the averaged line
# using the same categories/formula as CROM, and returns a raw (not yet
# -5..5 scaled) "xCROM" column alongside it, plus the per-category
# _x/_Diff/_Pct columns from add_category_projection_cols(). Players with
# no projection match get NA throughout, same as CRON's not-found case.
add_projection_score <- function(df, proj_df, player_type) {
  df <- df |> left_join(select(proj_df, -any_of("PlayerName")), by = "xMLBAMID")
  df <- add_category_projection_cols(df, player_type)

  # "diffCROM" -- sum of each core category's z-scored actual-minus-projected
  # diff (z_<cat>_Diff, added above), rescaled -5..5 alongside CROM and
  # xCROM by build_rankings(). A player beating their projections across
  # the board scores high; falling short across the board scores low.
  diff_cols <- paste0("z_", names(CATEGORY_DEFS[[player_type]]), "_Diff")
  df$diffCROM <- Reduce(`+`, lapply(df[diff_cols], z))

  df$xCROM <- projection_z_sum(df, player_type, "proj_")

  df |> select(-starts_with("proj_"))
}

# Sum of z-scored per-category values pulled from columns named
# `<prefix><CATEGORY>` (e.g. prefix "proj_" -> proj_AB, proj_H, ...), using
# the same core categories/sign conventions as CROM (ERA/WHIP flipped so
# higher z is always better). Shared by xCROM (prefix "proj_", preseason)
# and xCROM_ROS (prefix "rosproj_", rest-of-season) since both are "sum
# of z-scored projected stats," just sourced from a different FanGraphs
# projection pull.
projection_z_sum <- function(df, player_type, prefix) {
  col <- function(name) df[[paste0(prefix, name)]]
  if (player_type == "hitting") {
    sbn <- col("SB") - coalesce(col("CS"), 0)
    z(col("AB")) + z(col("H")) + z(col("R")) + z(col("RBI")) +
      z(sbn) + z(col("OBP")) + z(col("SLG"))
  } else {
    svh <- col("SV") + coalesce(col("HLD"), 0)
    z(col("IP")) - z(col("ERA")) - z(col("WHIP")) +
      z(col("SO")) + z(col("W")) + z(svh)
  }
}

# "xCROM_ROS" rest-of-season score -------------------------------------------
# Joins the 3-system-averaged rest-of-season projection (see
# fetch_fg_ros_projections) onto the currently qualified pool by MLBAM id
# and z-scores it the same way as xCROM, just from each system's
# rest-of-season line instead of its preseason line. Players with no ROS
# projection match get NA, same as xCROM/CRON's not-found case.
add_ros_projection_score <- function(df, ros_proj_df, player_type) {
  df <- df |> left_join(select(ros_proj_df, -any_of("PlayerName")), by = "xMLBAMID")
  df$xCROM_ROS <- projection_z_sum(df, player_type, "rosproj_")
  df |> select(-starts_with("rosproj_"))
}

# Rank hitters. `df` is a raw fetch_fg_leaders("hitting") tibble.
rank_hitters <- function(df, min_pa = 50, use_impact = FALSE) {
  df <- df |> filter(.data$PA >= min_pa)
  if (nrow(df) == 0) return(df)

  sbn <- df$SB - coalesce(df$CS, 0)

  if (use_impact) {
    lg_obp <- stats::weighted.mean(df$OBP, df$PA, na.rm = TRUE)
    lg_slg <- stats::weighted.mean(df$SLG, df$AB, na.rm = TRUE)
    obp_input <- df$PA * (df$OBP - lg_obp)
    slg_input <- df$AB * (df$SLG - lg_slg)
  } else {
    obp_input <- df$OBP
    slg_input <- df$SLG
  }

  df <- df |>
    mutate(
      z_AB  = z(.data$AB),
      z_H   = z(.data$H),
      z_R   = z(.data$R),
      z_RBI = z(.data$RBI),
      z_SBN = z(sbn),
      z_OBP = z(obp_input),
      z_SLG = z(slg_input),
      CROM = z_AB + z_H + z_R + z_RBI + z_SBN + z_OBP + z_SLG
    )
  df <- add_cron_similarity(df, c("z_AB", "z_H", "z_R", "z_RBI", "z_SBN", "z_OBP", "z_SLG"))

  df |>
    mutate(
      z_xwOBA   = z(.data$xwOBA),
      z_wRCplus = z(.data$wRC_plus),
      z_WAR     = z(.data$WAR),
      z_xAVG    = z(.data$xAVG),
      z_xSLG    = z(.data$xSLG)
    ) |>
    arrange(desc(.data$CROM))
}

# Rank pitchers. `df` is a raw fetch_fg_leaders("pitching") tibble.
rank_pitchers <- function(df, min_ip = 20, use_impact = FALSE) {
  df <- df |> filter(.data$IP >= min_ip)
  if (nrow(df) == 0) return(df)

  svh <- df$SV + coalesce(df$HLD, 0)

  if (use_impact) {
    lg_era  <- 9 * sum(df$ER, na.rm = TRUE) / sum(df$IP, na.rm = TRUE)
    lg_whip <- sum(df$H + df$BB, na.rm = TRUE) / sum(df$IP, na.rm = TRUE)
    era_input  <- df$IP * (lg_era  - df$ERA) / 9   # runs prevented vs league
    whip_input <- df$IP * (lg_whip - df$WHIP)      # baserunners prevented vs league
  } else {
    era_input  <- -df$ERA   # lower ERA is better -> flip sign
    whip_input <- -df$WHIP  # lower WHIP is better -> flip sign
  }

  df <- df |>
    mutate(
      z_IP   = z(.data$IP),
      z_ERA  = z(era_input),
      z_WHIP = z(whip_input),
      z_K    = z(.data$SO),
      z_W    = z(.data$W),
      z_SVH  = z(svh),
      CROM = z_IP + z_ERA + z_WHIP + z_K + z_W + z_SVH
    )
  df <- add_cron_similarity(df, c("z_IP", "z_ERA", "z_WHIP", "z_K", "z_W", "z_SVH"))

  df |>
    mutate(
      z_FIP   = -z(.data$FIP),
      z_WAR   = z(.data$WAR),
      z_xFIP  = -z(.data$xFIP),
      z_xERA  = -z(.data$xERA),
      z_SIERA = -z(.data$SIERA)
    ) |>
    arrange(desc(.data$CROM))
}

# Core scoring categories (feed into CROM) vs. supplemental sabermetric
# columns (context only, shown on their own tab). Keyed by player_type so
# app.R can split the display without hardcoding the category lists.
CORE_Z_COLS <- list(
  hitting = c("z_AB", "z_H", "z_R", "z_RBI", "z_SBN", "z_OBP", "z_SLG"),
  pitching = c("z_IP", "z_ERA", "z_WHIP", "z_K", "z_W", "z_SVH")
)
SUPP_Z_COLS <- list(
  hitting = c("z_xwOBA", "z_wRCplus", "z_WAR", "z_xAVG", "z_xSLG"),
  pitching = c("z_FIP", "z_WAR", "z_xFIP", "z_xERA", "z_SIERA")
)

# CORE_Z_COLS, expanded to also include each category's _x/_Diff/_Pct
# siblings, grouped together (actual, then its 3 projection comparisons).
core_cols_with_proj <- function(player_type) {
  unlist(lapply(CORE_Z_COLS[[player_type]], function(base) {
    c(base, paste0(base, c("_x", "_Diff", "_Pct")))
  }))
}

# Which of the 4 toggleable groups a core_cols_with_proj() column belongs to.
score_col_group <- function(col) {
  dplyr::case_when(
    grepl("_x$", col)    ~ "proj",
    grepl("_Diff$", col) ~ "diff",
    grepl("_Pct$", col)  ~ "pct",
    TRUE ~ "actual"
  )
}

# Keeps only the columns whose group is in `visible_groups` (e.g. the subset
# of c("actual","proj","diff","pct") toggled to "Show" in the UI).
filter_score_cols <- function(cols, visible_groups) {
  cols[score_col_group(cols) %in% visible_groups]
}

# Rescales each z-score-derived column to a -5 (worst in pool) - +5 (best in
# pool) scale, min-max per column. Degenerate (zero-range) columns map to a
# flat 0 rather than dividing by zero.
scale_score <- function(df, cols) {
  for (col in cols) {
    x <- df[[col]]
    # suppressWarnings: an all-NA column (e.g. xCROM when proj_df fetch
    # failed) makes range() warn "no non-missing arguments" -- already
    # handled below via the is.finite() check, so the warning is just noise.
    rng <- suppressWarnings(range(x, na.rm = TRUE))
    df[[col]] <- if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
      rep(0, length(x))
    } else {
      -5 + (x - rng[1]) / (rng[2] - rng[1]) * 10
    }
  }
  df
}

# Dispatch + tidy display frame (Rank, Player, Team, -5..5 scores, CROM).
# proj_df, if supplied, is a fetch_fg_projections() result used to add the
# "xCROM" preseason score column.
build_rankings <- function(df, player_type, min_qual, use_impact = FALSE, proj_df = NULL, ros_proj_df = NULL) {
  ranked <- if (player_type == "hitting") {
    rank_hitters(df, min_pa = min_qual, use_impact = use_impact)
  } else {
    rank_pitchers(df, min_ip = min_qual, use_impact = use_impact)
  }
  if (nrow(ranked) == 0) return(ranked)

  if (!is.null(proj_df)) {
    ranked <- add_projection_score(ranked, proj_df, player_type)
  } else {
    ranked$xCROM <- NA_real_
    ranked$diffCROM <- NA_real_
    for (col in core_cols_with_proj(player_type)) {
      if (!col %in% names(ranked)) ranked[[col]] <- NA_real_
    }
  }

  if (!is.null(ros_proj_df)) {
    ranked <- add_ros_projection_score(ranked, ros_proj_df, player_type)
  } else {
    ranked$xCROM_ROS <- NA_real_
  }

  ranked <- ranked |>
    mutate(Rank = row_number(), Player = .data$PlayerName, Team = .data$team_name,
           Headshot = .data$xMLBAMID) |>
    select(Rank, Headshot, Player, Team, CROM, xCROM, xCROM_ROS, diffCROM, CRON, starts_with("z_"))

  scale_score(ranked, c(grep("^z_", names(ranked), value = TRUE), "CROM", "xCROM", "xCROM_ROS", "diffCROM"))
}
