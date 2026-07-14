# FanGraphs preseason projections ---------------------------------------------
# Source 2b: FanGraphs' undocumented /api/projections endpoint (same one that
# powers fangraphs.com/projections). Pulls full-season, start-of-season
# projections from three independent systems and averages them per player;
# the ranking layer (see add_projection_score in rankings.R) then z-scores
# that averaged line the same way it z-scores the season-to-date stats.

suppressPackageStartupMessages({
  library(dplyr)
})
# httr and jsonlite are used via `::` (not attached) -- jsonlite::validate()
# masks shiny::validate() if attached, which broke every validate(need(...))
# call in app.R whenever data loaded successfully.

PROJECTION_SYSTEMS <- c("steamer", "zips", "thebat")

# Raw field names to pull from FanGraphs per player_type (before averaging).
FG_PROJECTION_STAT_COLS <- list(
  hitting = c("AB", "H", "R", "RBI", "SB", "CS", "OBP", "SLG"),
  pitching = c("IP", "ERA", "WHIP", "SO", "W", "SV", "HLD")
)

# Pulls one system's full-season projection leaderboard for one player_type.
fetch_fg_projections_one <- function(
  system,
  player_type = c("hitting", "pitching")
) {
  player_type <- match.arg(player_type)
  stat_cols <- FG_PROJECTION_STAT_COLS[[player_type]]

  url <- httr::modify_url(
    "https://www.fangraphs.com/api/projections",
    query = list(
      type = system,
      stats = if (player_type == "hitting") "bat" else "pit",
      pos = "all",
      team = "0",
      players = "0",
      lg = "all"
    )
  )
  resp <- httr::RETRY("GET", url, times = 3, quiet = TRUE)
  raw <- jsonlite::fromJSON(rawToChar(resp$content), flatten = TRUE)
  as_tibble(raw)[, intersect(
    c("PlayerName", "xMLBAMID", "playerid", stat_cols),
    names(raw)
  )]
}

# Pulls all PROJECTION_SYSTEMS for a player_type and averages each stat
# category across systems, joined by MLBAM id. A player missing from a
# system just contributes fewer values to that category's mean(na.rm=TRUE).
fetch_fg_projections <- function(player_type = c("hitting", "pitching")) {
  player_type <- match.arg(player_type)
  stat_cols <- FG_PROJECTION_STAT_COLS[[player_type]]

  combined <- PROJECTION_SYSTEMS |>
    lapply(fetch_fg_projections_one, player_type = player_type) |>
    bind_rows() |>
    filter(!is.na(.data$xMLBAMID))

  combined |>
    group_by(.data$xMLBAMID) |>
    summarise(
      PlayerName = dplyr::first(.data$PlayerName),
      across(all_of(stat_cols), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    rename_with(~ paste0("proj_", .x), all_of(stat_cols))
}

# Reader for the bundled preseason-projection snapshot (see DATA_DIR /
# data_refresh.R). Returns NULL if the file is absent -- the ranking layer
# treats a NULL proj_df as "no projections available".
read_fg_projections <- function(player_type, data_dir = DATA_DIR) {
  f <- file.path(data_dir, sprintf("proj_%s.rds", player_type))
  if (!file.exists(f)) {
    return(NULL)
  }
  readRDS(f)
}

# FanGraphs rest-of-season projections ------------------------------------------
# Same 3 systems as the preseason pull, but FanGraphs exposes each system's
# rest-of-season line under its own (inconsistently named) `type` code rather
# than a shared suffix/prefix convention -- confirmed against the live API:
# Steamer's ROS line is "steamerr", ZiPS' is "rzips", THE BAT's is "rthebat".
PROJECTION_SYSTEMS_ROS <- c(
  steamer = "steamerr",
  zips = "rzips",
  thebat = "rthebat"
)

fetch_fg_ros_projections_one <- function(
  system_type,
  player_type = c("hitting", "pitching")
) {
  player_type <- match.arg(player_type)
  stat_cols <- FG_PROJECTION_STAT_COLS[[player_type]]

  url <- httr::modify_url(
    "https://www.fangraphs.com/api/projections",
    query = list(
      type = system_type,
      stats = if (player_type == "hitting") "bat" else "pit",
      pos = "all",
      team = "0",
      players = "0",
      lg = "all"
    )
  )
  resp <- httr::RETRY("GET", url, times = 3, quiet = TRUE)
  raw <- jsonlite::fromJSON(rawToChar(resp$content), flatten = TRUE)
  as_tibble(raw)[, intersect(
    c("PlayerName", "xMLBAMID", "playerid", stat_cols),
    names(raw)
  )]
}

# Pulls all 3 systems' ROS lines for a player_type and averages each stat
# category across systems, joined by MLBAM id -- mirrors fetch_fg_projections
# but prefixes averaged columns "rosproj_" (distinct from "proj_") so both
# can be joined onto the same ranked df at once.
fetch_fg_ros_projections <- function(player_type = c("hitting", "pitching")) {
  player_type <- match.arg(player_type)
  stat_cols <- FG_PROJECTION_STAT_COLS[[player_type]]

  combined <- PROJECTION_SYSTEMS_ROS |>
    lapply(fetch_fg_ros_projections_one, player_type = player_type) |>
    bind_rows() |>
    filter(!is.na(.data$xMLBAMID))

  combined |>
    group_by(.data$xMLBAMID) |>
    summarise(
      PlayerName = dplyr::first(.data$PlayerName),
      across(all_of(stat_cols), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    rename_with(~ paste0("rosproj_", .x), all_of(stat_cols))
}

# Reader for the bundled rest-of-season-projection snapshot. Returns NULL if
# the file is absent (ranking layer treats a NULL ros_proj_df as "none").
read_fg_ros_projections <- function(player_type, data_dir = DATA_DIR) {
  f <- file.path(data_dir, sprintf("rosproj_%s.rds", player_type))
  if (!file.exists(f)) {
    return(NULL)
  }
  readRDS(f)
}
