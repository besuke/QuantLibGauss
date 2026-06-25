#' Convert ISO string to QuantLib Date
#'
#' @export
qlg_to_ql_date <- function(x) {
  QuantLib::DateParser_parseISO(as.character(x))
}

#' Build QuantLib DateVector
#'
#' @export
qlg_build_date_vector <- function(date_list) {
  dv <- QuantLib::DateVector()
  purrr::walk(date_list, function(d) {
    QuantLib::DateVector_append(dv, d)
  })
  dv
}

#' Convert QuantLib leg to cashflow table
#'
#' @export
qlg_leg_to_cashflow_tbl <- function(leg) {
  tibble::tibble(idx = seq_len(leg$size())) |>
    dplyr::mutate(
      cashflow = purrr::map(idx, function(i) leg[i][[1]]),
      date = purrr::map_chr(cashflow, function(cf) QuantLib::Date_ISO(QuantLib::CashFlow_date(cf))),
      amount = purrr::map_dbl(cashflow, function(cf) QuantLib::CashFlow_amount(cf))
    ) |>
    dplyr::select(date, amount)
}

#' Run QuantLib cashflow example
#'
#' @export
qlg_cashflow_example <- function() {
  qlg_use_quantlib()
  requireNamespace("QuantLib", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)

  calendar <- QuantLib::TARGET()

  todaysDate <- qlg_to_ql_date("2020-10-19")
  invisible(QuantLib::Settings_instance()$setEvaluationDate(d = todaysDate))

  settlementDays <- 3
  settlementDate <- QuantLib::Calendar_advance(
    calendar, todaysDate, settlementDays, "Days"
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
  ) |>
    dplyr::mutate(
      ql_date = purrr::map(date_chr, qlg_to_ql_date)
    )

  dates <- qlg_build_date_vector(curve_input_tbl$ql_date)

  forecast_curve <- QuantLib::ZeroCurve(
    dates,
    curve_input_tbl$rate,
    QuantLib::Actual365Fixed()
  )

  forecast_handle <- QuantLib::YieldTermStructureHandle(forecast_curve)

  swapBuilder <- QuantLib::MakeOIS(
    swapTenor = QuantLib::Period(5, "Years"),
    overnightIndex = QuantLib::Eonia(forecast_handle),
    fixedRate = 0.002
  )

  swap <- QuantLib::MakeOIS_makeOIS(swapBuilder)

  fixed_leg <- swap$fixedLeg()
  floating_leg <- swap$overnightLeg()

  list(
    today = todaysDate$`__str__`(),
    settlement_date = settlementDate$`__str__`(),
    fixed_leg_maturity = QuantLib::Date_ISO(
      QuantLib::CashFlows_maturityDate(fixed_leg)
    ),
    fixed_leg_cashflows = qlg_leg_to_cashflow_tbl(fixed_leg),
    floating_leg_cashflows = qlg_leg_to_cashflow_tbl(floating_leg)
  )
}
