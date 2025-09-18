# R/zzz.R
# Silence NSE notes and set GIF backend on load.

utils::globalVariables(c(
  # data.table columns
  "PG", "Va", "Va_log", ".SD", ":=",
  # selection helper
  "..xs",
  # ggplot/tidyr columns
  "type", "value", "resid", "hi95", "lo95"
))

.onLoad <- function(libname, pkgname) {
  # Decide GIF backend (no hard dependency)
  if (requireNamespace("gifski", quietly = TRUE)) {
    assign("GIF_BACKEND", "gifski", envir = parent.env(environment()))
  } else if (requireNamespace("magick", quietly = TRUE)) {
    assign("GIF_BACKEND", "magick", envir = parent.env(environment()))
  } else {
    assign("GIF_BACKEND", "none",   envir = parent.env(environment()))
  }
}
