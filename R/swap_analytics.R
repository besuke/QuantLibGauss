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


#' OIS overnight leg cashflow table
#'
#' @param swap A QuantLib OIS-like object.
#'
#' @export
qlg_ois_overnight_leg_table <- function(swap) {
  qlg_leg_to_cashflow_tbl(swap$overnightLeg())
}

#' Run OIS cashflow example
#'
#' @export
qlg_ois_cashflow_example <- function() {
  qlg_use_quantlib()
  requireNamespace("tibble", quietly = TRUE)

  qlg_eval_date("2020-10-19")

  calendar <- QuantLib::TARGET()

  today <- qlg_date("2020-10-19")

  settlement_date <- QuantLib::Calendar_advance(
    calendar,
    today,
    3,
    "Days"
  )

  curve_input_tbl <- tibble::tibble(
    date_chr = c(
      "2020-10-19", "2020-11-19", "2021-01-19", "2021-04-19",
      "2021-10-19", "2022-04-19", "2022-10-19", "2023-10-19",
      "2025-10-19", "2030-10-19", "2035-10-19", "2040-10-19"
    ),
    rate = c(
      -0.004, -0.002, 0.001, 0.005,
      0.009, 0.010, 0.010, 0.012,
      0.017, 0.019, 0.028, 0.032
    )
  )

  dates <- qlg_make_date_vector(curve_input_tbl$date_chr)

  forecast_curve <- QuantLib::ZeroCurve(
    dates,
    curve_input_tbl$rate,
    QuantLib::Actual365Fixed()
  )

  forecast_handle <- QuantLib::YieldTermStructureHandle(forecast_curve)

  swap_builder <- QuantLib::MakeOIS(
    swapTenor = QuantLib::Period(5, "Years"),
    overnightIndex = QuantLib::Eonia(forecast_handle),
    fixedRate = 0.002
  )

  swap <- QuantLib::MakeOIS_makeOIS(swap_builder)

  list(
    today = QuantLib::Date_ISO(today),
    settlement_date = QuantLib::Date_ISO(settlement_date),
    fixed_leg = qlg_swap_fixed_leg_table(swap),
    overnight_leg = qlg_ois_overnight_leg_table(swap)
  )
}
