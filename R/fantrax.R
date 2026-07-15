# Fantrax league integration --------------------------------------------------
# Pulls standings and roster/ownership data from the Fantrax Beta API (the
# endpoints documented at fantrax.com/developer). These are plain GET requests
# returning JSON; a public league needs no authentication. Like the FanGraphs
# layer, the fetch_* functions here are called offline by data_refresh.R and
# the results are written to data/ for the app to read.

# Read from the FANTRAX_LEAGUE_ID environment variable (set in the gitignored
# .Renviron, loaded at R startup) so the league ID never lives in version
# control. See .Renviron.example for the expected format.
FANTRAX_LEAGUE_ID <- Sys.getenv("FANTRAX_LEAGUE_ID")
FANTRAX_BASE <- "https://www.fantrax.com/fxea/general/"

# Smart Fantasy Baseball's public Player ID Map (stable CSV export). Used to
# translate Fantrax player IDs to MLBAM IDs, which the CROM tables already
# carry (xMLBAMID / the Headshot column) -- an ID join avoids the accent and
# suffix mismatches that name-based joins hit (e.g. José Ramírez).
SFBB_IDMAP_URL <- "https://www.smartfantasybaseball.com/PLAYERIDMAPCSV"

# Thin GET wrapper: returns the parsed JSON body or errors on non-200.
fantrax_get <- function(endpoint, query = list()) {
  resp <- httr::GET(paste0(FANTRAX_BASE, endpoint), query = query)
  if (httr::status_code(resp) != 200) {
    stop(sprintf(
      "Fantrax %s returned HTTP %d",
      endpoint,
      httr::status_code(resp)
    ))
  }
  httr::content(resp, as = "parsed", type = "application/json")
}

# Fantrax stores player names as "Last, First" (e.g. "Henderson, Gunnar").
# FanGraphs / the CROM tables use "First Last", so flip for name-based joins.
# Handles single-token names and extra commas defensively.
fantrax_flip_name <- function(x) {
  vapply(
    x,
    function(nm) {
      parts <- strsplit(nm, ",\\s*")[[1]]
      if (length(parts) < 2) {
        return(nm)
      }
      paste(paste(parts[-1], collapse = ", "), parts[1])
    },
    character(1),
    USE.NAMES = FALSE
  )
}

# League standings as a tidy tibble, ordered by rank.
fetch_fantrax_standings <- function(league_id = FANTRAX_LEAGUE_ID) {
  raw <- fantrax_get("getStandings", list(leagueId = league_id))
  dplyr::bind_rows(lapply(raw, function(t) {
    tibble::tibble(
      rank = as.integer(t$rank),
      team = t$teamName %||% NA_character_,
      team_id = t$teamId %||% NA_character_,
      points = suppressWarnings(as.numeric(t$points)),
      games_back = suppressWarnings(as.numeric(t$gamesBack)),
      win_pct = suppressWarnings(as.numeric(t$winPercentage))
    )
  })) |>
    dplyr::arrange(rank)
}

# Fantrax player-ID reference for a sport: fantrax_id -> name/team/position.
# Names are flipped to "First Last" for joining to the CROM tables.
fetch_fantrax_player_ids <- function(sport = "MLB") {
  raw <- fantrax_get("getPlayerIds", list(sport = sport))
  dplyr::bind_rows(lapply(raw, function(p) {
    tibble::tibble(
      fantrax_id = p$fantraxId %||% NA_character_,
      player = fantrax_flip_name(p$name %||% NA_character_),
      mlb_team = p$team %||% NA_character_,
      mlb_pos = p$position %||% NA_character_
    )
  }))
}

# Every rostered player across all teams: one row per (fantasy team, player).
# `period` is the scoring period the roster reflects (echoed by the API).
fetch_fantrax_rosters <- function(league_id = FANTRAX_LEAGUE_ID) {
  raw <- fantrax_get("getTeamRosters", list(leagueId = league_id))
  rows <- lapply(names(raw$rosters), function(tid) {
    team <- raw$rosters[[tid]]
    dplyr::bind_rows(lapply(team$rosterItems, function(item) {
      tibble::tibble(
        team_id = tid,
        fantasy_team = team$teamName %||% NA_character_,
        fantrax_id = item$id %||% NA_character_,
        roster_position = item$position %||% NA_character_,
        status = item$status %||% NA_character_
      )
    }))
  })
  out <- dplyr::bind_rows(rows)
  attr(out, "period") <- raw$period
  out
}

# Fantrax ID -> MLBAM ID crosswalk from the SFBB Player ID Map. SFBB wraps
# the Fantrax ID in asterisks (e.g. "*01u75*"), so strip them to match the
# raw ids returned by getPlayerIds.
fetch_sfbb_idmap <- function(url = SFBB_IDMAP_URL) {
  map <- readr::read_csv(url, show_col_types = FALSE)
  tibble::tibble(
    fantrax_id = gsub("\\*", "", map$FANTRAXID),
    mlbam_id = suppressWarnings(as.integer(map$MLBID)),
    fangraphs_id = as.character(map$IDFANGRAPHS)
  ) |>
    dplyr::filter(!is.na(fantrax_id), fantrax_id != "", !is.na(mlbam_id))
}

# Ownership table: one row per rostered player, ready to left_join onto the
# CROM tables. `mlbam_id` (via the SFBB crosswalk) is the preferred join key
# against CROM's Headshot/xMLBAMID; `player` supports a name fallback. Free
# agents (unrostered players) simply won't appear here.
fetch_fantrax_ownership <- function(
  league_id = FANTRAX_LEAGUE_ID,
  sport = "MLB"
) {
  rosters <- fetch_fantrax_rosters(league_id)
  ids <- fetch_fantrax_player_ids(sport)
  xwalk <- fetch_sfbb_idmap()
  rosters |>
    dplyr::left_join(ids, by = "fantrax_id") |>
    dplyr::left_join(
      dplyr::select(xwalk, fantrax_id, mlbam_id),
      by = "fantrax_id"
    ) |>
    dplyr::select(
      player,
      mlbam_id,
      mlb_team,
      mlb_pos,
      fantasy_team,
      roster_position,
      status,
      fantrax_id
    )
}

# --- Readers for the bundled Fantrax snapshot (written by data_refresh.R) ---

read_fantrax_standings <- function(data_dir = DATA_DIR) {
  f <- file.path(data_dir, "fantrax_standings.rds")
  if (!file.exists(f)) {
    return(NULL)
  }
  readRDS(f)
}

read_fantrax_ownership <- function(data_dir = DATA_DIR) {
  f <- file.path(data_dir, "fantrax_ownership.rds")
  if (!file.exists(f)) {
    return(NULL)
  }
  readRDS(f)
}
