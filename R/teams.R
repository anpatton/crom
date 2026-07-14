# Team logo lookup -----------------------------------------------------------
# Maps FanGraphs team abbreviations to MLB's numeric team ids, used to build
# https://www.mlbstatic.com/team-logos/{id}.svg image URLs for the table.

TEAM_LOGO_ID <- c(
  ARI = "109", ATH = "133", ATL = "144", BAL = "110", BOS = "111",
  CHC = "112", CHW = "145", CIN = "113", CLE = "114", COL = "115",
  DET = "116", HOU = "117", KCR = "118", LAA = "108", LAD = "119",
  MIA = "146", MIL = "158", MIN = "142", NYM = "121", NYY = "147",
  PHI = "143", PIT = "134", SDP = "135", SEA = "136", SFG = "137",
  STL = "138", TBR = "139", TEX = "140", TOR = "141", WSN = "120"
)

# Renders team abbreviations as <img> logo tags; unknown abbreviations
# (e.g. "- - -" for players who changed teams) fall back to the raw text.
team_logo_html <- function(abbrev) {
  id <- TEAM_LOGO_ID[abbrev]
  ifelse(
    is.na(id),
    abbrev,
    sprintf('<img src="https://www.mlbstatic.com/team-logos/%s.svg" height="24" alt="%s" title="%s">',
            id, abbrev, abbrev)
  )
}

# Renders MLBAM player ids as <img> headshot tags, sourced from MLB's
# static headshot CDN; missing ids fall back to an empty cell.
player_headshot_html <- function(mlbam_id) {
  ifelse(
    is.na(mlbam_id),
    "",
    sprintf('<img src="https://img.mlbstatic.com/mlb-photos/image/upload/w_60,q_100/v1/people/%s/headshot/67/current.png" height="40">',
            mlbam_id)
  )
}
