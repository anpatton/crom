# Shared utilities -------------------------------------------------------------

# Null-coalescing operator: returns `x` unless it is NULL, else `y`. Base R
# only ships `%||%` since 4.4.0 and no attached package here exports it, so we
# define it in the sourced data layer where both app.R and R/style.R rely on
# it (defining it only in app.R breaks sourcing R/ on its own).
`%||%` <- function(x, y) if (is.null(x)) y else x
