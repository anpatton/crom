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
  hitting  = c("AB", "H", "R", "RBI", "SB", "CS", "OBP", "SLG"),
  pitching = c("IP", "ERA", "WHIP", "SO", "W", "SV", "HLD")
)

# Pulls one system's full-season projection leaderboard for one player_type.
fetch_fg_projections_one <- function(system, player_type = c("hitting", "pitching")) {
  player_type <- match.arg(player_type)
  stat_cols <- FG_PROJECTION_STAT_COLS[[player_type]]

  url <- httr::modify_url(
    "https://www.fangraphs.com/api/projections",
    query = list(type = system, stats = if (player_type == "hitting") "bat" else "pit",
                 pos = "all", team = "0", players = "0", lg = "all")
  )
  resp <- httr::RETRY("GET", url, times = 3, quiet = TRUE)
  raw <- jsonlite::fromJSON(rawToChar(resp$content), flatten = TRUE)
  as_tibble(raw)[, intersect(c("PlayerName", "xMLBAMID", "playerid", stat_cols), names(raw))]
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

# Disk cache wrapper, mirroring fetch_fg_leaders_cached -- one file per
# player_type per calendar day (preseason projections barely move day to
# day, but this keeps the caching story consistent with the season stats).
FG_PROJ_CACHE_DIR <- "cache"

fetch_fg_projections_cached <- function(player_type, force_refresh = FALSE,
                                         cache_dir = FG_PROJ_CACHE_DIR) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  cache_file <- file.path(cache_dir, sprintf("proj_%s_%s.rds", player_type, Sys.Date()))

  if (!force_refresh && file.exists(cache_file)) {
    return(readRDS(cache_file))
  }

  df <- fetch_fg_projections(player_type)
  saveRDS(df, cache_file)
  df
}

# FanGraphs rest-of-season projections ------------------------------------------
# Same 3 systems as the preseason pull, but FanGraphs exposes each system's
# rest-of-season line under its own (inconsistently named) `type` code rather
# than a shared suffix/prefix convention -- confirmed against the live API:
# Steamer's ROS line is "steamerr", ZiPS' is "rzips", THE BAT's is "rthebat".
PROJECTION_SYSTEMS_ROS <- c(steamer = "steamerr", zips = "rzips", thebat = "rthebat")

fetch_fg_ros_projections_one <- function(system_type, player_type = c("hitting", "pitching")) {
  player_type <- match.arg(player_type)
  stat_cols <- FG_PROJECTION_STAT_COLS[[player_type]]

  url <- httr::modify_url(
    "https://www.fangraphs.com/api/projections",
    query = list(type = system_type, stats = if (player_type == "hitting") "bat" else "pit",
                 pos = "all", team = "0", players = "0", lg = "all")
  )
  resp <- httr::RETRY("GET", url, times = 3, quiet = TRUE)
  raw <- jsonlite::fromJSON(rawToChar(resp$content), flatten = TRUE)
  as_tibble(raw)[, intersect(c("PlayerName", "xMLBAMID", "playerid", stat_cols), names(raw))]
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

# Disk cache wrapper, mirroring fetch_fg_projections_cached -- ROS lines move
# daily (unlike preseason projections) as games are played, so this still
# only saves repeat pulls within the same calendar day.
FG_ROS_PROJ_CACHE_DIR <- "cache"

fetch_fg_ros_projections_cached <- function(player_type, force_refresh = FALSE,
                                             cache_dir = FG_ROS_PROJ_CACHE_DIR) {
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  cache_file <- file.path(cache_dir, sprintf("rosproj_%s_%s.rds", player_type, Sys.Date()))

  if (!force_refresh && file.exists(cache_file)) {
    return(readRDS(cache_file))
  }

  df <- fetch_fg_ros_projections(player_type)
  saveRDS(df, cache_file)
  df
}
