#' Make a QuantLib interest-rate cap
#'
#' @export
qlg_make_cap <- function(
    notional,
    start_date,
    maturity_date,
    cap_rate,
    valuation_date = qlg_eval_date_get(),
    tenor_months = 6L,
    discount_rate = 0.03,
    forecast_rate = discount_rate,
    volatility = 0.20,
    day_counter = QuantLib::Actual365Fixed(),
    calendar = QuantLib::TARGET(),
    convention = QuantLib::BusinessDayConvention_ModifiedFollowing_get(),
    pricing_engine = NULL
) {
  qlg_use_quantlib()
  requireNamespace("QuantLib", quietly = TRUE)

  parts <- .qlg_interest_option_parts(
    notional = notional,
    start_date = start_date,
    maturity_date = maturity_date,
    valuation_date = valuation_date,
    tenor_months = tenor_months,
    discount_rate = discount_rate,
    forecast_rate = forecast_rate,
    volatility = volatility,
    day_counter = day_counter,
    calendar = calendar,
    convention = convention,
    pricing_engine = pricing_engine
  )

  cap <- QuantLib::Cap(
    parts$leg,
    c(as.numeric(cap_rate))
  )

  QuantLib::Instrument_setPricingEngine(
    cap,
    parts$engine
  )

  cap
}

#' Make a QuantLib interest-rate floor
#'
#' @export
qlg_make_floor <- function(
    notional,
    start_date,
    maturity_date,
    floor_rate,
    valuation_date = qlg_eval_date_get(),
    tenor_months = 6L,
    discount_rate = 0.03,
    forecast_rate = discount_rate,
    volatility = 0.20,
    day_counter = QuantLib::Actual365Fixed(),
    calendar = QuantLib::TARGET(),
    convention = QuantLib::BusinessDayConvention_ModifiedFollowing_get(),
    pricing_engine = NULL
) {
  qlg_use_quantlib()
  requireNamespace("QuantLib", quietly = TRUE)

  parts <- .qlg_interest_option_parts(
    notional = notional,
    start_date = start_date,
    maturity_date = maturity_date,
    valuation_date = valuation_date,
    tenor_months = tenor_months,
    discount_rate = discount_rate,
    forecast_rate = forecast_rate,
    volatility = volatility,
    day_counter = day_counter,
    calendar = calendar,
    convention = convention,
    pricing_engine = pricing_engine
  )

  floor <- QuantLib::Floor(
    parts$leg,
    c(as.numeric(floor_rate))
  )

  QuantLib::Instrument_setPricingEngine(
    floor,
    parts$engine
  )

  floor
}

#' Calculate cap/floor NPV
#'
#' @export
qlg_cap_floor_npv <- function(cap_floor) {
  .qlg_cap_floor_value(
    cap_floor,
    "Instrument_NPV"
  )
}

#' Calculate cap/floor vega
#'
#' @export
qlg_cap_floor_vega <- function(cap_floor) {
  .qlg_cap_floor_value(
    cap_floor,
    "CapFloor_vega"
  )
}

#' Extract cap/floor optionlet prices
#'
#' @export
qlg_cap_floor_optionlet_prices <- function(cap_floor) {
  requireNamespace("tibble", quietly = TRUE)

  prices <- tryCatch(
    QuantLib::CapFloor_optionletsPrice(cap_floor),
    error = function(e) numeric()
  )

  tibble::tibble(
    optionlet = seq_along(prices),
    price = as.numeric(prices)
  )
}

#' Summarise a cap/floor
#'
#' @export
qlg_cap_floor_summary <- function(cap_floor) {
  requireNamespace("tibble", quietly = TRUE)

  tibble::tibble(
    metric = c("npv", "vega"),
    value = c(
      qlg_cap_floor_npv(cap_floor),
      qlg_cap_floor_vega(cap_floor)
    )
  )
}

.qlg_interest_option_parts <- function(
    notional,
    start_date,
    maturity_date,
    valuation_date,
    tenor_months,
    discount_rate,
    forecast_rate,
    volatility,
    day_counter,
    calendar,
    convention,
    pricing_engine = NULL
) {
  valuation_date <- as.character(as.Date(valuation_date))
  qlg_eval_date(valuation_date)

  eval_date <- qlg_date(valuation_date)
  start_date <- qlg_date(as.character(as.Date(start_date)))
  maturity_date <- qlg_date(as.character(as.Date(maturity_date)))

  tenor <- QuantLib::Period(
    as.integer(tenor_months),
    QuantLib::TimeUnit_Months_get()
  )

  schedule <- QuantLib::Schedule(
    start_date,
    maturity_date,
    tenor,
    calendar,
    convention,
    convention,
    QuantLib::DateGeneration_Forward_get(),
    FALSE
  )

  discount_curve <- QuantLib::YieldTermStructureHandle(
    QuantLib::FlatForward(
      eval_date,
      as.numeric(discount_rate),
      day_counter
    )
  )

  forecast_curve <- QuantLib::YieldTermStructureHandle(
    QuantLib::FlatForward(
      eval_date,
      as.numeric(forecast_rate),
      day_counter
    )
  )

  index <- QuantLib::Euribor__SWIG_0(
    tenor,
    forecast_curve
  )

  leg <- QuantLib::IborLeg__SWIG_14(
    c(as.numeric(notional)),
    schedule,
    index,
    day_counter
  )

  if (is.null(pricing_engine)) {
    vol_quote <- QuantLib::QuoteHandle(
      QuantLib::SimpleQuote(as.numeric(volatility))
    )

    pricing_engine <- QuantLib::BlackCapFloorEngine__SWIG_1(
      discount_curve,
      vol_quote,
      day_counter
    )
  }

  list(
    leg = leg,
    engine = pricing_engine
  )
}

.qlg_cap_floor_value <- function(cap_floor, fun_name) {
  out <- tryCatch(
    .qlg_quantlib_fun(fun_name)(cap_floor),
    error = function(e) NA_real_
  )

  as.numeric(out)
}
#' Make a cap from trade data
#'
#' @param trade A one-row data frame containing cap trade fields.
#' @param pricing_engine Optional QuantLib pricing engine.
#'
#' @return QuantLib Cap object.
#' @export
qlg_make_cap_from_trade <- function(
    trade,
    pricing_engine = NULL
) {
  qlg_use_quantlib()

  if (!is.data.frame(trade) || nrow(trade) != 1) {
    stop("trade must be a one-row data frame.", call. = FALSE)
  }

  notional <- .qlg_interest_option_required_field(
    trade = trade,
    names = c("notional", "nominal"),
    label = "notional"
  )

  start_date <- .qlg_interest_option_required_field(
    trade = trade,
    names = c("start_date", "effective_date"),
    label = "start_date or effective_date"
  )

  maturity_date <- .qlg_interest_option_required_field(
    trade = trade,
    names = c("maturity_date", "termination_date", "end_date", "expiry_date"),
    label = "maturity_date, termination_date, end_date, or expiry_date"
  )

  cap_rate <- .qlg_interest_option_required_field(
    trade = trade,
    names = c("cap_rate", "strike", "rate"),
    label = "cap_rate, strike, or rate"
  )

  qlg_make_cap(
    notional = notional,
    start_date = start_date,
    maturity_date = maturity_date,
    cap_rate = cap_rate,
    valuation_date = .qlg_interest_option_trade_field(
      trade = trade,
      names = c("valuation_date", "eval_date"),
      default = qlg_eval_date_get()
    ),
    tenor_months = .qlg_interest_option_trade_field(
      trade = trade,
      names = c("tenor_months", "ibor_tenor_months"),
      default = 6L
    ),
    discount_rate = .qlg_interest_option_trade_field(
      trade = trade,
      names = c("discount_rate", "risk_free_rate"),
      default = 0.03
    ),
    forecast_rate = .qlg_interest_option_trade_field(
      trade = trade,
      names = c("forecast_rate", "forward_rate"),
      default = .qlg_interest_option_trade_field(
        trade = trade,
        names = c("discount_rate", "risk_free_rate"),
        default = 0.03
      )
    ),
    volatility = .qlg_interest_option_trade_field(
      trade = trade,
      names = c("volatility", "vol"),
      default = 0.20
    ),
    pricing_engine = pricing_engine
  )
}

#' Make a floor from trade data
#'
#' @param trade A one-row data frame containing floor trade fields.
#' @param pricing_engine Optional QuantLib pricing engine.
#'
#' @return QuantLib Floor object.
#' @export
qlg_make_floor_from_trade <- function(
    trade,
    pricing_engine = NULL
) {
  qlg_use_quantlib()

  if (!is.data.frame(trade) || nrow(trade) != 1) {
    stop("trade must be a one-row data frame.", call. = FALSE)
  }

  notional <- .qlg_interest_option_required_field(
    trade = trade,
    names = c("notional", "nominal"),
    label = "notional"
  )

  start_date <- .qlg_interest_option_required_field(
    trade = trade,
    names = c("start_date", "effective_date"),
    label = "start_date or effective_date"
  )

  maturity_date <- .qlg_interest_option_required_field(
    trade = trade,
    names = c("maturity_date", "termination_date", "end_date", "expiry_date"),
    label = "maturity_date, termination_date, end_date, or expiry_date"
  )

  floor_rate <- .qlg_interest_option_required_field(
    trade = trade,
    names = c("floor_rate", "strike", "rate"),
    label = "floor_rate, strike, or rate"
  )

  qlg_make_floor(
    notional = notional,
    start_date = start_date,
    maturity_date = maturity_date,
    floor_rate = floor_rate,
    valuation_date = .qlg_interest_option_trade_field(
      trade = trade,
      names = c("valuation_date", "eval_date"),
      default = qlg_eval_date_get()
    ),
    tenor_months = .qlg_interest_option_trade_field(
      trade = trade,
      names = c("tenor_months", "ibor_tenor_months"),
      default = 6L
    ),
    discount_rate = .qlg_interest_option_trade_field(
      trade = trade,
      names = c("discount_rate", "risk_free_rate"),
      default = 0.03
    ),
    forecast_rate = .qlg_interest_option_trade_field(
      trade = trade,
      names = c("forecast_rate", "forward_rate"),
      default = .qlg_interest_option_trade_field(
        trade = trade,
        names = c("discount_rate", "risk_free_rate"),
        default = 0.03
      )
    ),
    volatility = .qlg_interest_option_trade_field(
      trade = trade,
      names = c("volatility", "vol"),
      default = 0.20
    ),
    pricing_engine = pricing_engine
  )
}

#' Make an interest-rate option from trade data
#'
#' This dispatches to cap or floor creation depending on the trade fields.
#'
#' @param trade A one-row data frame containing interest-rate option trade fields.
#' @param pricing_engine Optional QuantLib pricing engine.
#'
#' @return QuantLib Cap or Floor object.
#' @export
qlg_make_interest_option_from_trade <- function(
    trade,
    pricing_engine = NULL
) {
  qlg_use_quantlib()

  if (!is.data.frame(trade) || nrow(trade) != 1) {
    stop("trade must be a one-row data frame.", call. = FALSE)
  }

  product <- .qlg_interest_option_trade_field(
    trade = trade,
    names = c("product", "product_type", "instrument_type", "trade_type", "interest_option_type"),
    default = ""
  )

  product <- .qlg_interest_option_token(product)

  has_cap_rate <- !is.null(
    .qlg_interest_option_trade_field(
      trade = trade,
      names = c("cap_rate"),
      default = NULL
    )
  )

  has_floor_rate <- !is.null(
    .qlg_interest_option_trade_field(
      trade = trade,
      names = c("floor_rate"),
      default = NULL
    )
  )

  if (product %in% c("cap", "ircap", "interestratecap") || has_cap_rate) {
    return(
      qlg_make_cap_from_trade(
        trade = trade,
        pricing_engine = pricing_engine
      )
    )
  }

  if (product %in% c("floor", "irfloor", "interestratefloor") || has_floor_rate) {
    return(
      qlg_make_floor_from_trade(
        trade = trade,
        pricing_engine = pricing_engine
      )
    )
  }

  stop(
    "Unable to determine interest option type. ",
    "Use product = 'cap' or 'floor', or provide cap_rate / floor_rate.",
    call. = FALSE
  )
}

.qlg_interest_option_trade_field <- function(
    trade,
    names,
    default = NULL
) {
  for (nm in names) {
    value <- qlg_trade_value(
      trade = trade,
      name = nm,
      default = NULL
    )

    if (!is.null(value) && length(value) > 0 && !is.na(value[[1]])) {
      return(value[[1]])
    }
  }

  default
}

.qlg_interest_option_required_field <- function(
    trade,
    names,
    label = names[[1]]
) {
  value <- .qlg_interest_option_trade_field(
    trade = trade,
    names = names,
    default = NULL
  )

  if (is.null(value)) {
    stop(
      "trade must contain ",
      label,
      ".",
      call. = FALSE
    )
  }

  value
}

.qlg_interest_option_token <- function(x) {
  tolower(gsub("[^A-Za-z0-9]", "", as.character(x[[1]])))
}