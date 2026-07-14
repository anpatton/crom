# FanGraphs data pulls (via baseballr) --------------------------------------
# Source 2 of 3. baseballr also wraps the MLB Stats API (mlb_* fns) which we
# use later for roster / position eligibility; FanGraphs leaderboards are the
# cleaner source for the counting + rate stats that drive the 5x5 z-scores.
#
# Key quirk learned the hard way: to pull a *rolling* window you must pass
# month = "1000" (FanGraphs' "custom date range" flag) alongside
# startdate/enddate, otherwise FanGraphs silently returns full-season totals.

suppressPackageStartupMessages({
  library(baseballr)
  library(dplyr)
})
# httr and jsonlite are used via `::` (not attached) -- see projections.R's
# note on jsonlite::validate() masking shiny::validate().

# Pitching + custom date range hits a live FanGraphs API quirk: for that one
# combination (stats = "pit" with startdate/enddate set) the response's
# season column comes back lowercase "season" instead of "Season", and
# baseballr::fg_pitcher_leaders() hardcodes dplyr::select("Season", ...),
# so it throws "object 'leaders' not found" instead of returning data.
# Hitting and non-rolling pitching pulls are unaffected -- this bypasses
# baseballr for just the broken combination, hitting the same endpoint
# directly and keeping only the columns rank_pitchers()/add_projection_score
# actually use.
fetch_fg_pitcher_leaders_rolling <- function(season, startdate, enddate) {
  params <- list(
    age = "",
    pos = "all",
    stats = "pit",
    lg = "all",
    qual = "0",
    season = season,
    season1 = season,
    startdate = startdate,
    enddate = enddate,
    month = "1000",
    hand = "",
    team = "0",
    pageitems = "10000",
    pagenum = "1",
    ind = "0",
    rost = "0",
    players = "",
    type = "8",
    postseason = "",
    sortdir = "default",
    sortstat = "WAR"
  )
  url <- httr::modify_url(
    "https://www.fangraphs.com/api/leaders/major-league/data",
    query = params
  )
  resp <- httr::RETRY("GET", url, times = 3, quiet = TRUE)
  raw <- jsonlite::fromJSON(rawToChar(resp$content), flatten = TRUE)

  needed <- c(
    "PlayerName",
    "xMLBAMID",
    "TeamName",
    "IP",
    "ER",
    "H",
    "BB",
    "ERA",
    "WHIP",
    "SO",
    "W",
    "SV",
    "HLD",
    "FIP",
    "WAR",
    "xFIP",
    "xERA",
    "SIERA"
  )
  as_tibble(raw$data)[, intersect(needed, names(raw$data))] |>
    rename(team_name = "TeamName")
}

# Pull a hitter or pitcher leaderboard for a season or a rolling last-N days.
#
#   player_type : "hitting" | "pitching"
#   window      : "season"  | "rolling"
#   last_n      : number of days back for the rolling window
#   as_of       : end date of the rolling window (defaults to today)
#
# Returns the raw baseballr tibble (qual = "0" -> everyone; filtering by
# playing time happens in the ranking step).
fetch_fg_leaders <- function(
  player_type = c("hitting", "pitching"),
  season = as.integer(format(Sys.Date(), "%Y")),
  window = c("season", "rolling"),
  last_n = 15,
  as_of = Sys.Date()
) {
  player_type <- match.arg(player_type)
  window <- match.arg(window)

  if (player_type == "pitching" && window == "rolling") {
    return(fetch_fg_pitcher_leaders_rolling(
      season,
      as.character(as_of - last_n),
      as.character(as_of)
    ))
  }

  args <- list(startseason = season, endseason = season, qual = "0")
  if (window == "rolling") {
    args$startdate <- as.character(as_of - last_n)
    args$enddate <- as.character(as_of)
    args$month <- "1000" # custom date-range flag
  }

  fn <- if (player_type == "hitting") fg_batter_leaders else fg_pitcher_leaders
  do.call(fn, args)
}

# Static daily snapshot layer -------------------------------------------------
# Data is no longer pulled live from the app. An offline job (data_refresh.R,
# run by cron at 7am ET) fetches every combination and writes it to DATA_DIR
# under a stable, date-free filename that ships bundled with the app. The app
# only ever reads these files.
DATA_DIR <- "data"

# Filename tag for a leaderboard window: "season", "15" -> "L15", etc.
leaders_tag <- function(window) {
  if (window == "season") "season" else paste0("L", window)
}

leaders_file <- function(player_type, window, data_dir = DATA_DIR) {
  file.path(data_dir, sprintf("%s_%s.rds", player_type, leaders_tag(window)))
}

# Reader used by the app. Returns the bundled snapshot for one player_type /
# window, or NULL (with an `err` attribute) if the file is missing.
read_fg_leaders <- function(
  player_type,
  window = "season",
  data_dir = DATA_DIR
) {
  f <- leaders_file(player_type, window, data_dir)
  if (!file.exists(f)) {
    return(structure(NULL, err = sprintf("Data file not found: %s", f)))
  }
  readRDS(f)
}

# Snapshot metadata (list with `generated_at`), written by data_refresh.R.
read_snapshot_meta <- function(data_dir = DATA_DIR) {
  f <- file.path(data_dir, "meta.rds")
  if (!file.exists(f)) {
    return(NULL)
  }
  readRDS(f)
}
