#' Use local QuantLib installation
#'
#' @export
qlg_use_quantlib <- function(
  path = "C:/AnalyticFin/Projects/QuantLib_tidy/library/windows"
) {
  .libPaths(c(path, .libPaths()))
  invisible(.libPaths())
}

#' Test QuantLibGauss
#'
#' @export
qlg_hello <- function() {
  "QuantLibGauss is ready."
}