#' QuantLib bond example
#'
#' @export
qlg_bond_example <- function() {
  qlg_use_quantlib()

  source(
    system.file(
      "examples/raw_scripts/bonds.R",
      package = "QuantLibGauss"
    ),
    local = TRUE
  )

  list(
    summary = result_tbl,
    yield = yld,
    clean_price = clnPrc,
    yield_check = yld2
  )
}