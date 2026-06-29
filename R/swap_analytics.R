# swap_analytics.R

#' Swap NPV
#'
#' @param swap A QuantLib Swap object.
#'
#' @export
qlg_swap_npv <- function(swap) {
  swap$NPV()
}

#' Swap leg NPV
#'
#' @param swap A QuantLib Swap object.
#' @param leg_no Leg number. QuantLib uses 0-based leg indexing.
#'
#' @export
qlg_swap_leg_npv <- function(swap, leg_no) {
  swap$legNPV(as.integer(leg_no))
}

#' Swap fixed leg NPV
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @export
qlg_swap_fixed_leg_npv <- function(swap) {
  qlg_swap_leg_npv(swap, 0L)
}

#' Swap floating leg NPV
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @export
qlg_swap_floating_leg_npv <- function(swap) {
  qlg_swap_leg_npv(swap, 1L)
}

#' Swap fair fixed rate
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @export
qlg_swap_fair_rate <- function(swap) {
  tryCatch(
    swap$fairRate(),
    error = function(e) NA_real_
  )
}

#' Swap fair spread
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @export
qlg_swap_fair_spread <- function(swap) {
  tryCatch(
    swap$fairSpread(),
    error = function(e) NA_real_
  )
}

#' Swap fixed leg cashflow table
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @export
qlg_swap_fixed_leg_table <- function(swap) {
  qlg_leg_to_cashflow_tbl(swap$fixedLeg())
}

#' Swap floating leg cashflow table
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @export
qlg_swap_floating_leg_table <- function(swap) {
  qlg_leg_to_cashflow_tbl(swap$floatingLeg())
}

#' Swap summary
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @export
qlg_swap_summary <- function(swap) {
  qlg_use_quantlib()
  requireNamespace("tibble", quietly = TRUE)

  tibble::tibble(
    npv = qlg_swap_npv(swap),
    fixed_leg_npv = qlg_swap_fixed_leg_npv(swap),
    floating_leg_npv = qlg_swap_floating_leg_npv(swap),
    fair_rate = qlg_swap_fair_rate(swap),
    fair_spread = qlg_swap_fair_spread(swap)
  )
}
