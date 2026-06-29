
#' Convert ISO date to QuantLib Date
#'
#' @export
qlg_date <- function(x) {
  if (inherits(x, "Date")) {
    x <- format(x, "%Y-%m-%d")
  }
  stopifnot(is.character(x), length(x) == 1)
  QuantLib::DateParser_parseISO(x)
}

#' Convert QuantLib Date to ISO string
#'
#' @export
qlg_iso <- function(x) {
  tryCatch(
    QuantLib::Date_ISO(x),
    error = function(e) as.character(x)
  )
}


qlg_make_date_vector <- function(dates) {
  dv <- QuantLib::DateVector()
  for (d in dates) {
    QuantLib::DateVector_append(dv, qlg_date(d))
  }
  dv
}

qlg_day_counter <- function(day_counter = "Actual365Fixed") {
  switch(
    day_counter,
    Actual365Fixed = QuantLib::Actual365Fixed(),
    Actual360 = QuantLib::Actual360(),
    Thirty360 = QuantLib::Thirty360(),
    stop("Unsupported day counter: ", day_counter)
  )
}

#' Build a QuantLib discount curve from zero-rate nodes
#'
#' @export
qlg_build_discount_curve <- function(nodes, day_counter = "Actual365Fixed") {
  stopifnot(all(c("date", "zero_rate") %in% names(nodes)))

  dates_chr <- nodes$date
  dates_ql  <- qlg_make_date_vector(dates_chr)
  dc <- qlg_day_counter(day_counter)

  origin <- as.Date(dates_chr[[1]])
  times <- as.numeric(as.Date(dates_chr) - origin) / 365
  times[1] <- 0

  dfs <- exp(-nodes$zero_rate * times)
  dfs[1] <- 1.0

  QuantLib::DiscountCurve(dates_ql, dfs, dc)
}

#' Extract tidy discount table from zero-rate nodes
#'
#' @export
qlg_discount_table <- function(nodes) {
  first_date <- as.Date(nodes$date[[1]])

  tibble::tibble(
    date = nodes$date,
    zero_rate_input = nodes$zero_rate
  ) |>
    dplyr::mutate(
      year_frac = as.numeric(as.Date(date) - first_date) / 365,
      discount = dplyr::if_else(
        year_frac > 0,
        exp(-zero_rate_input * year_frac),
        1
      ),
      implied_zero = dplyr::if_else(
        year_frac > 0,
        -log(discount) / year_frac,
        0
      )
    )
}

#' Make a tidy OIS curve object
#'
#' @export
qlg_ois_curve <- function(nodes_tbl, day_counter = "Actual365Fixed") {
  stopifnot(is.data.frame(nodes_tbl))
  stopifnot(all(c("date", "zero_rate") %in% names(nodes_tbl)))

  qlg_set_eval_date(nodes_tbl$date[[1]])

  curve <- qlg_build_discount_curve(
    nodes = nodes_tbl,
    day_counter = day_counter
  )

  out_tbl <- qlg_discount_table(nodes_tbl)

  list(
    curve = curve,
    table = out_tbl
  )
}

#' Example OIS curve nodes
#'
#' @export
qlg_example_ois_nodes <- function() {
  tibble::tribble(
    ~date,         ~zero_rate,
    "2026-04-12",  0.0030,
    "2026-05-12",  0.0032,
    "2026-07-12",  0.0034,
    "2026-10-12",  0.0038,
    "2027-04-12",  0.0045,
    "2028-04-12",  0.0065,
    "2029-04-12",  0.0080,
    "2031-04-12",  0.0105
  )
}

#' Run OIS curve example
#'
#' @export
qlg_ois_curve_example <- function() {
  nodes <- qlg_example_ois_nodes()
  result <- qlg_ois_curve(nodes)
  result$table
}


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
#' Leg to Cashflow Table
#'
#' @export
qlg_leg_to_cashflow_tbl <- function(leg) {
  tibble::tibble(
    idx = seq_len(leg$size())
  ) |>
    dplyr::mutate(
      cashflow = purrr::map(
        idx,
        function(i) leg[i][[1]]
      ),
      date = purrr::map_chr(
        cashflow,
        function(cf) Date_ISO(CashFlow_date(cf))
      ),
      amount = purrr::map_dbl(
        cashflow,
        function(cf) CashFlow_amount(cf)
      )
    ) |>
    dplyr::select(date, amount)
}
#' Create a QuantLib QuoteHandle
#'
#' @export
qlg_quote_handle <- function(x) {
  QuantLib::QuoteHandle(QuantLib::SimpleQuote(x))
}

#' Push rate helpers into a QuantLib RateHelperVector
#'
#' @export
qlg_push_rate_helpers <- function(x) {
  vec <- QuantLib::RateHelperVector()
  purrr::walk(x, ~ QuantLib::RateHelperVector_push_back(vec, .x))
  vec
}

#' Build a bond discount curve
#'
#' @export
qlg_build_bond_discount_curve <- function(
    settlement_date = "2008-09-18",
    fixing_days = 3,
    settlement_days = 3
) {
  qlg_use_quantlib()

  calendar <- QuantLib::UnitedStates("GovernmentBond")

  settlementDate <- qlg_date(settlement_date)
  settlementDate <- QuantLib::Calendar_adjust(calendar, settlementDate)

  zc_quotes <- tibble::tibble(
    rate = c(0.0096, 0.0145, 0.0194),
    tenor_n = c(3, 6, 1),
    tenor_unit = c("Months", "Months", "Years")
  ) |>
    dplyr::mutate(
      tenor = purrr::map2(tenor_n, tenor_unit, QuantLib::Period)
    )

  zcBondsDayCounter <- QuantLib::Actual365Fixed()

  deposit_helpers_bond_curve <- purrr::pmap(
    list(zc_quotes$rate, zc_quotes$tenor),
    function(rate, tenor) {
      QuantLib::DepositRateHelper(
        QuantLib::QuoteHandle(QuantLib::SimpleQuote(rate)),
        tenor,
        fixing_days,
        calendar,
        "ModifiedFollowing",
        TRUE,
        zcBondsDayCounter
      )
    }
  )

  bond_quotes <- tibble::tibble(
    issue_date   = c("2005-03-15", "2005-06-15", "2006-06-30", "2002-11-15", "1987-05-15"),
    maturity     = c("2010-08-31", "2011-08-31", "2013-08-31", "2018-08-15", "2038-05-15"),
    coupon_rate  = c(0.02375, 0.04625, 0.03125, 0.04000, 0.04500),
    market_quote = c(100.390625, 106.21875, 100.59375, 101.6875, 102.140625)
  ) |>
    dplyr::mutate(
      issue_qldate    = purrr::map(issue_date, qlg_date),
      maturity_qldate = purrr::map(maturity, qlg_date)
    )

  redemption <- 100.0

  bond_helpers <- purrr::pmap(
    list(
      bond_quotes$issue_qldate,
      bond_quotes$maturity_qldate,
      bond_quotes$coupon_rate,
      bond_quotes$market_quote
    ),
    function(issueDate, maturityDate, couponRate, marketQuote) {
      schedule <- QuantLib::Schedule(
        issueDate,
        maturityDate,
        QuantLib::Period("Semiannual"),
        QuantLib::UnitedStates("GovernmentBond"),
        "Unadjusted",
        "Unadjusted",
        QuantLib::copyToR(QuantLib::DateGeneration(), "Backward"),
        FALSE
      )

      QuantLib::FixedRateBondHelper(
        QuantLib::QuoteHandle(QuantLib::SimpleQuote(marketQuote)),
        settlement_days,
        100.0,
        schedule,
        couponRate,
        QuantLib::ActualActual("Bond"),
        "Unadjusted",
        redemption,
        issueDate
      )
    }
  )

  bondInstruments <- qlg_push_rate_helpers(
    c(deposit_helpers_bond_curve, bond_helpers)
  )

  termStructureDayCounter <- QuantLib::ActualActual("ISDA")

  QuantLib::PiecewiseFlatForward(
    settlementDate,
    bondInstruments,
    termStructureDayCounter
  )
}

#' Build a deposit-swap forecasting curve
#'
#' @export
qlg_build_swap_curve <- function(
    settlement_date = "2008-09-18",
    fixing_days = 3
) {
  qlg_use_quantlib()

  calendar <- QuantLib::UnitedStates("GovernmentBond")

  settlementDate <- qlg_date(settlement_date)
  settlementDate <- QuantLib::Calendar_adjust(calendar, settlementDate)

  dQuotes <- tibble::tibble(
    rate = c(0.043375, 0.031875, 0.0320375, 0.03385, 0.0338125, 0.0335125),
    tenor_n = c(1, 1, 3, 6, 9, 1),
    tenor_unit = c("Weeks", "Months", "Months", "Months", "Months", "Years")
  ) |>
    dplyr::mutate(
      tenor = purrr::map2(tenor_n, tenor_unit, QuantLib::Period)
    )

  sQuotes <- tibble::tibble(
    rate = c(0.0295, 0.0323, 0.0359, 0.0412, 0.0433),
    tenor_n = c(2, 3, 5, 10, 15),
    tenor_unit = c("Years", "Years", "Years", "Years", "Years")
  ) |>
    dplyr::mutate(
      tenor = purrr::map2(tenor_n, tenor_unit, QuantLib::Period)
    )

  depositDayCounter <- QuantLib::Actual360()

  depo_helpers_swap_curve <- purrr::pmap(
    list(dQuotes$rate, dQuotes$tenor),
    function(rate, tenor) {
      QuantLib::DepositRateHelper(
        QuantLib::QuoteHandle(QuantLib::SimpleQuote(rate)),
        tenor,
        fixing_days,
        calendar,
        "ModifiedFollowing",
        TRUE,
        depositDayCounter
      )
    }
  )

  swFixedLegFrequency   <- "Annual"
  swFixedLegConvention  <- "Unadjusted"
  swFixedLegDayCounter  <- QuantLib::Thirty360("European")
  swFloatingLegIndex    <- QuantLib::Euribor6M()
  forwardStart          <- QuantLib::Period(1, "Days")

  swap_helpers <- purrr::pmap(
    list(sQuotes$rate, sQuotes$tenor),
    function(rate, tenor) {
      QuantLib::SwapRateHelper(
        QuantLib::QuoteHandle(QuantLib::SimpleQuote(rate)),
        tenor,
        calendar,
        swFixedLegFrequency,
        swFixedLegConvention,
        swFixedLegDayCounter,
        swFloatingLegIndex,
        QuantLib::QuoteHandle(),
        forwardStart
      )
    }
  )

  depoSwapInstruments <- qlg_push_rate_helpers(
    c(depo_helpers_swap_curve, swap_helpers)
  )

  termStructureDayCounter <- QuantLib::ActualActual("ISDA")

  QuantLib::PiecewiseFlatForward(
    settlementDate,
    depoSwapInstruments,
    termStructureDayCounter
  )
}
