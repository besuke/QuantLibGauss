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