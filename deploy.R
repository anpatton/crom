# Deploy CROM
# (directly callable)
# Deployment messages (and any error) are teed to logs/deploy.log with a
# run-start timestamp so the scheduled 7am ET deploy is diagnosable even when
# run headless by cron.
library(rsconnect)

dir.create("/root/crom/logs", showWarnings = FALSE, recursive = TRUE)
log_con <- file("/root/crom/logs/deploy.log", open = "at")

# Tee stdout to the log (split = TRUE keeps console output too) and redirect
# messages/warnings there as well.
sink(log_con, split = TRUE)
sink(log_con, type = "message")

on.exit(
  {
    sink(type = "message")
    sink()
    close(log_con)
  },
  add = TRUE
)

message(sprintf(
  "\n===== deploy run: %s =====",
  format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
))

app_dir <- "/root/crom"

# Explicit allowlist of everything the running app needs, so the bundle
# excludes the offline scripts (deploy.R / data_refresh.R -- their absolute
# paths otherwise trip an rsconnect warning), logs/, rsconnect/, .git, and
# the unused www/*.png art (~9.6 MB). Team logos and headshots load from
# img.mlbstatic.com, so crom.png is the only local image required.
app_files <- c(
  "app.R",
  file.path("R", list.files(file.path(app_dir, "R"), pattern = "\\.R$")),
  file.path("data", list.files(file.path(app_dir, "data"))),
  "www/crom.png"
)

tryCatch(
  {
    deployApp(
      appDir = app_dir,
      appFiles = app_files,
      appName = "CROM",
      account = "apanalytics",
      server = "shinyapps.io",
      forceUpdate = TRUE
    )
    message(sprintf(
      "===== deploy succeeded: %s =====",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    ))
  },
  error = function(e) {
    message(sprintf("===== deploy FAILED: %s =====", conditionMessage(e)))
    # Restore sinks before signalling so cron still sees a non-zero exit.
    sink(type = "message")
    sink()
    quit(status = 1L)
  }
)
