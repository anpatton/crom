# Deploy the Fantasy Baseball Rankings app to shinyapps.io
# Usage:  Rscript deploy.R
library(rsconnect)

deployApp(
  appDir      = "/Users/andrewpatton/crom",
  appName     = "CROM",
  account     = "apanalytics",
  server      = "shinyapps.io",
  forceUpdate = TRUE
)
