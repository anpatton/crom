# CROM daily data refresh ------------------------------------------------------
# Offline job, run by cron at 7am ET (see crontab note in README/plan). Pulls
# every FanGraphs combination the app needs and writes it to data/ under a
# stable, date-free filename that ships bundled with the app. The app reads
# these files only; it never fetches live.
#
# Usage:  Rscript data_refresh.R   (from the project root)
#
# On a fetch failure the previous file is left in place (not overwritten with a
# broken/empty result) and the script exits non-zero so cron logs surface it,
# while the app keeps serving the last good snapshot.

suppressPackageStartupMessages({
  library(baseballr)
  library(dplyr)
})

# Source the fetch layer (fetch_fg_leaders, fetch_fg_projections, etc.).
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)

failures <- character(0)

# Write `fetcher()`'s result to `path`, but only if it returns a non-empty
# data frame. Otherwise record a failure and leave any existing file untouched.
save_pull <- function(path, fetcher) {
  ok <- tryCatch(
    {
      df <- fetcher()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        stop("empty or non-data-frame result")
      }
      saveRDS(df, path)
      message(sprintf("  wrote %s (%d rows)", path, nrow(df)))
      TRUE
    },
    error = function(e) {
      message(sprintf("  FAILED %s: %s", path, conditionMessage(e)))
      FALSE
    }
  )
  if (!ok) {
    failures[[length(failures) + 1L]] <<- path
  }
  invisible(ok)
}

player_types <- c("hitting", "pitching")
windows <- c("season", "15", "30", "60")

message("Pulling leaderboards...")
for (pt in player_types) {
  for (w in windows) {
    save_pull(leaders_file(pt, w), function() {
      if (w == "season") {
        fetch_fg_leaders(player_type = pt, window = "season")
      } else {
        fetch_fg_leaders(
          player_type = pt,
          window = "rolling",
          last_n = as.integer(w)
        )
      }
    })
  }
}

message("Pulling preseason projections...")
for (pt in player_types) {
  save_pull(file.path(DATA_DIR, sprintf("proj_%s.rds", pt)), function() {
    fetch_fg_projections(pt)
  })
}

message("Pulling rest-of-season projections...")
for (pt in player_types) {
  save_pull(file.path(DATA_DIR, sprintf("rosproj_%s.rds", pt)), function() {
    fetch_fg_ros_projections(pt)
  })
}

message("Pulling Fantrax league data...")
save_pull(file.path(DATA_DIR, "fantrax_standings.rds"), function() {
  fetch_fantrax_standings()
})
save_pull(file.path(DATA_DIR, "fantrax_ownership.rds"), function() {
  fetch_fantrax_ownership()
})

# Snapshot metadata: the app displays generated_at as the "updated" time.
saveRDS(list(generated_at = Sys.time()), file.path(DATA_DIR, "meta.rds"))

if (length(failures) > 0) {
  message(sprintf("Refresh completed with %d failure(s):", length(failures)))
  for (p in failures) {
    message("  - ", p)
  }
  quit(status = 1L)
}

message("Refresh completed successfully.")
