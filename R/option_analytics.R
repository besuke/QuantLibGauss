# option_analytics.R

#' Make a QuantLib European vanilla option
#'
#' @param spot Spot price.
#' @param strike Strike price.
#' @param maturity_date Option maturity date.
#' @param option_type Option type. Use "call" or "put".
#' @param valuation_date Evaluation date.
#' @param risk_free_rate Flat risk-free rate.
#' @param dividend_yield Flat dividend yield.
#' @param volatility Flat Black volatility.
#' @param day_counter QuantLib day counter.
#' @param calendar QuantLib calendar.
#' @param pricing_engine Optional QuantLib pricing engine.
#'
#' @return QuantLib VanillaOption object.
#' @export
qlg_make_european_option <- function(
    spot,
    strike,
    maturity_date,
    option_type = "call",
    valuation_date = qlg_eval_date_get(),
    risk_free_rate = 0.03,
    dividend_yield = 0,
    volatility = 0.20,
    day_counter = QuantLib::Actual365Fixed(),
    calendar = QuantLib::TARGET(),
    pricing_engine = NULL
) {
  qlg_use_quantlib()
  requireNamespace("QuantLib", quietly = TRUE)

  valuation_date <- as.character(as.Date(valuation_date))
  qlg_eval_date(valuation_date)

  eval_date <- qlg_date(valuation_date)
  maturity_date <- .qlg_option_date(maturity_date)
  option_type <- .qlg_option_type(option_type)

  spot_handle <- QuantLib::QuoteHandle(
    QuantLib::SimpleQuote(as.numeric(spot))
  )

  risk_free_curve <- QuantLib::YieldTermStructureHandle(
    QuantLib::FlatForward(
      eval_date,
      as.numeric(risk_free_rate),
      day_counter
    )
  )

  dividend_curve <- QuantLib::YieldTermStructureHandle(
    QuantLib::FlatForward(
      eval_date,
      as.numeric(dividend_yield),
      day_counter
    )
  )

  vol_curve <- QuantLib::BlackVolTermStructureHandle(
    QuantLib::BlackConstantVol(
      eval_date,
      calendar,
      as.numeric(volatility),
      day_counter
    )
  )

  process <- QuantLib::BlackScholesMertonProcess(
    spot_handle,
    dividend_curve,
    risk_free_curve,
    vol_curve
  )

  payoff <- QuantLib::PlainVanillaPayoff(
    option_type,
    as.numeric(strike)
  )

  exercise <- QuantLib::EuropeanExercise(maturity_date)

  option <- QuantLib::VanillaOption(
    payoff,
    exercise
  )

  if (is.null(pricing_engine)) {
    pricing_engine <- QuantLib::AnalyticEuropeanEngine(process)
  }

  QuantLib::Instrument_setPricingEngine(option, pricing_engine)

  option
}

#' Option NPV
#'
#' @param option QuantLib option object.
#'
#' @return Numeric NPV.
#' @export
qlg_option_npv <- function(option) {
  .qlg_option_value(option, "Instrument_NPV")
}

#' Option delta
#'
#' @param option QuantLib option object.
#'
#' @return Numeric delta.
#' @export
qlg_option_delta <- function(option) {
  .qlg_option_value(option, "OneAssetOption_delta")
}

#' Option gamma
#'
#' @param option QuantLib option object.
#'
#' @return Numeric gamma.
#' @export
qlg_option_gamma <- function(option) {
  .qlg_option_value(option, "OneAssetOption_gamma")
}

#' Option vega
#'
#' @param option QuantLib option object.
#'
#' @return Numeric vega.
#' @export
qlg_option_vega <- function(option) {
  .qlg_option_value(option, "OneAssetOption_vega")
}

#' Option theta
#'
#' @param option QuantLib option object.
#'
#' @return Numeric theta.
#' @export
qlg_option_theta <- function(option) {
  .qlg_option_value(option, "OneAssetOption_theta")
}

#' Option rho
#'
#' @param option QuantLib option object.
#'
#' @return Numeric rho.
#' @export
qlg_option_rho <- function(option) {
  .qlg_option_value(option, "OneAssetOption_rho")
}

#' Summarise an option
#'
#' @param option QuantLib option object.
#'
#' @return A tibble with option analytics.
#' @export
qlg_option_summary <- function(option) {
  requireNamespace("tibble", quietly = TRUE)

  tibble::tibble(
    metric = c(
      "npv",
      "delta",
      "gamma",
      "vega",
      "theta",
      "rho"
    ),
    value = c(
      qlg_option_npv(option),
      qlg_option_delta(option),
      qlg_option_gamma(option),
      qlg_option_vega(option),
      qlg_option_theta(option),
      qlg_option_rho(option)
    )
  )
}

.qlg_option_value <- function(option, fun_name) {
  out <- tryCatch(
    .qlg_quantlib_fun(fun_name)(option),
    error = function(e) NA_real_
  )

  as.numeric(out)
}
.qlg_option_date <- function(x) {
  if (is.character(x) || inherits(x, "Date")) {
    return(qlg_date(as.character(as.Date(x))))
  }

  x
}

.qlg_option_type <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  x <- tolower(trimws(x[[1]]))

  if (x %in% c("call", "c")) {
    return(QuantLib::Option_Call_get())
  }

  if (x %in% c("put", "p")) {
    return(QuantLib::Option_Put_get())
  }

  stop("Unsupported option_type: ", x, ". Use 'call' or 'put'.", call. = FALSE)
}
#' Make a European vanilla option from trade data
#'
#' @param trade A one-row data frame containing European option trade fields.
#' @param pricing_engine Optional QuantLib pricing engine.
#'
#' @return QuantLib VanillaOption object.
#' @export
qlg_make_european_option_from_trade <- function(
    trade,
    pricing_engine = NULL
) {
  qlg_use_quantlib()

  if (!is.data.frame(trade) || nrow(trade) != 1) {
    stop("trade must be a one-row data frame.", call. = FALSE)
  }

  maturity_date <- qlg_trade_value(
    trade = trade,
    name = "maturity_date",
    default = NULL
  )

  if (is.null(maturity_date)) {
    maturity_date <- qlg_trade_value(
      trade = trade,
      name = "expiry_date",
      default = NULL
    )
  }

  if (is.null(maturity_date)) {
    stop("trade must contain maturity_date or expiry_date.", call. = FALSE)
  }

  qlg_make_european_option(
    spot = qlg_trade_value(
      trade = trade,
      name = "spot",
      default = NULL
    ),
    strike = qlg_trade_value(
      trade = trade,
      name = "strike",
      default = NULL
    ),
    maturity_date = maturity_date,
    option_type = qlg_trade_value(
      trade = trade,
      name = "option_type",
      default = "call"
    ),
    valuation_date = qlg_trade_value(
      trade = trade,
      name = "valuation_date",
      default = qlg_eval_date_get()
    ),
    risk_free_rate = qlg_trade_value(
      trade = trade,
      name = "risk_free_rate",
      default = 0.03
    ),
    dividend_yield = qlg_trade_value(
      trade = trade,
      name = "dividend_yield",
      default = 0
    ),
    volatility = qlg_trade_value(
      trade = trade,
      name = "volatility",
      default = 0.20
    ),
    pricing_engine = pricing_engine
  )
}
#' Option implied volatility
#'
#' @param option QuantLib option object.
#' @param target_value Market option value.
#' @param spot Spot price.
#' @param valuation_date Evaluation date.
#' @param risk_free_rate Flat risk-free rate.
#' @param dividend_yield Flat dividend yield.
#' @param volatility Initial volatility used to build the process.
#' @param day_counter QuantLib day counter.
#' @param calendar QuantLib calendar.
#' @param accuracy Solver accuracy.
#' @param max_evaluations Maximum solver evaluations.
#' @param min_vol Minimum volatility.
#' @param max_vol Maximum volatility.
#'
#' @return Numeric implied volatility.
#' @export
qlg_option_implied_volatility <- function(
    option,
    target_value,
    spot,
    valuation_date = qlg_eval_date_get(),
    risk_free_rate = 0.03,
    dividend_yield = 0,
    volatility = 0.20,
    day_counter = QuantLib::Actual365Fixed(),
    calendar = QuantLib::TARGET(),
    accuracy = 1e-6,
    max_evaluations = 100,
    min_vol = 1e-7,
    max_vol = 4.0
) {
  qlg_use_quantlib()
  requireNamespace("QuantLib", quietly = TRUE)

  valuation_date <- as.character(as.Date(valuation_date))
  qlg_eval_date(valuation_date)

  eval_date <- qlg_date(valuation_date)

  spot_handle <- QuantLib::QuoteHandle(
    QuantLib::SimpleQuote(as.numeric(spot))
  )

  risk_free_curve <- QuantLib::YieldTermStructureHandle(
    QuantLib::FlatForward(
      eval_date,
      as.numeric(risk_free_rate),
      day_counter
    )
  )

  dividend_curve <- QuantLib::YieldTermStructureHandle(
    QuantLib::FlatForward(
      eval_date,
      as.numeric(dividend_yield),
      day_counter
    )
  )

  vol_curve <- QuantLib::BlackVolTermStructureHandle(
    QuantLib::BlackConstantVol(
      eval_date,
      calendar,
      as.numeric(volatility),
      day_counter
    )
  )

  process <- QuantLib::BlackScholesMertonProcess(
    spot_handle,
    dividend_curve,
    risk_free_curve,
    vol_curve
  )

  as.numeric(
    QuantLib::VanillaOption_impliedVolatility(
      option,
      as.numeric(target_value),
      process,
      as.numeric(accuracy),
      as.integer(max_evaluations),
      as.numeric(min_vol),
      as.numeric(max_vol)
    )
  )
}
#' Make a QuantLib American vanilla option
#'
#' @param spot Spot price.
#' @param strike Strike price.
#' @param maturity_date Option maturity date.
#' @param option_type Option type. Use "call" or "put".
#' @param valuation_date Evaluation date.
#' @param risk_free_rate Flat risk-free rate.
#' @param dividend_yield Flat dividend yield.
#' @param volatility Flat Black volatility.
#' @param day_counter QuantLib day counter.
#' @param calendar QuantLib calendar.
#' @param steps Number of binomial tree steps.
#' @param pricing_engine Optional QuantLib pricing engine.
#'
#' @return QuantLib VanillaOption object.
#' @export
qlg_make_american_option <- function(
    spot,
    strike,
    maturity_date,
    option_type = "put",
    valuation_date = qlg_eval_date_get(),
    risk_free_rate = 0.03,
    dividend_yield = 0,
    volatility = 0.20,
    day_counter = QuantLib::Actual365Fixed(),
    calendar = QuantLib::TARGET(),
    steps = 200L,
    pricing_engine = NULL
) {
  qlg_use_quantlib()
  requireNamespace("QuantLib", quietly = TRUE)

  valuation_date <- as.character(as.Date(valuation_date))
  qlg_eval_date(valuation_date)

  eval_date <- qlg_date(valuation_date)
  maturity_date <- .qlg_option_date(maturity_date)
  option_type <- .qlg_option_type(option_type)

  spot_handle <- QuantLib::QuoteHandle(
    QuantLib::SimpleQuote(as.numeric(spot))
  )

  risk_free_curve <- QuantLib::YieldTermStructureHandle(
    QuantLib::FlatForward(
      eval_date,
      as.numeric(risk_free_rate),
      day_counter
    )
  )

  dividend_curve <- QuantLib::YieldTermStructureHandle(
    QuantLib::FlatForward(
      eval_date,
      as.numeric(dividend_yield),
      day_counter
    )
  )

  vol_curve <- QuantLib::BlackVolTermStructureHandle(
    QuantLib::BlackConstantVol(
      eval_date,
      calendar,
      as.numeric(volatility),
      day_counter
    )
  )

  process <- QuantLib::BlackScholesMertonProcess(
    spot_handle,
    dividend_curve,
    risk_free_curve,
    vol_curve
  )

  payoff <- QuantLib::PlainVanillaPayoff(
    option_type,
    as.numeric(strike)
  )

  exercise <- QuantLib::AmericanExercise(
    eval_date,
    maturity_date
  )

  option <- QuantLib::VanillaOption(
    payoff,
    exercise
  )

  if (is.null(pricing_engine)) {
    pricing_engine <- QuantLib::BinomialCRRVanillaEngine(
      process,
      as.integer(steps)
    )
  }

  QuantLib::Instrument_setPricingEngine(option, pricing_engine)

  option
}
#' Make a QuantLib barrier option
#'
#' @param spot Spot price.
#' @param strike Strike price.
#' @param maturity_date Option maturity date.
#' @param barrier Barrier level.
#' @param barrier_type Barrier type. Use "down_out", "up_out", "down_in", or "up_in".
#' @param rebate Barrier rebate amount.
#' @param option_type Option type. Use "call" or "put".
#' @param valuation_date Evaluation date.
#' @param risk_free_rate Flat risk-free rate.
#' @param dividend_yield Flat dividend yield.
#' @param volatility Flat Black volatility.
#' @param day_counter QuantLib day counter.
#' @param calendar QuantLib calendar.
#' @param pricing_engine Optional QuantLib pricing engine.
#'
#' @return QuantLib BarrierOption object.
#' @export
qlg_make_barrier_option <- function(
    spot,
    strike,
    maturity_date,
    barrier,
    barrier_type = "down_out",
    rebate = 0,
    option_type = "call",
    valuation_date = qlg_eval_date_get(),
    risk_free_rate = 0.03,
    dividend_yield = 0,
    volatility = 0.20,
    day_counter = QuantLib::Actual365Fixed(),
    calendar = QuantLib::TARGET(),
    pricing_engine = NULL
) {
  qlg_use_quantlib()
  requireNamespace("QuantLib", quietly = TRUE)

  valuation_date <- as.character(as.Date(valuation_date))
  qlg_eval_date(valuation_date)

  eval_date <- qlg_date(valuation_date)
  maturity_date <- .qlg_option_date(maturity_date)
  option_type <- .qlg_option_type(option_type)
  barrier_type <- .qlg_barrier_type(barrier_type)

  spot_handle <- QuantLib::QuoteHandle(
    QuantLib::SimpleQuote(as.numeric(spot))
  )

  risk_free_curve <- QuantLib::YieldTermStructureHandle(
    QuantLib::FlatForward(
      eval_date,
      as.numeric(risk_free_rate),
      day_counter
    )
  )

  dividend_curve <- QuantLib::YieldTermStructureHandle(
    QuantLib::FlatForward(
      eval_date,
      as.numeric(dividend_yield),
      day_counter
    )
  )

  vol_curve <- QuantLib::BlackVolTermStructureHandle(
    QuantLib::BlackConstantVol(
      eval_date,
      calendar,
      as.numeric(volatility),
      day_counter
    )
  )

  process <- QuantLib::BlackScholesMertonProcess(
    spot_handle,
    dividend_curve,
    risk_free_curve,
    vol_curve
  )

  payoff <- QuantLib::PlainVanillaPayoff(
    option_type,
    as.numeric(strike)
  )

  exercise <- QuantLib::EuropeanExercise(maturity_date)

  option <- QuantLib::BarrierOption(
    barrier_type,
    as.numeric(barrier),
    as.numeric(rebate),
    payoff,
    exercise
  )

  if (is.null(pricing_engine)) {
    pricing_engine <- QuantLib::AnalyticBarrierEngine(process)
  }

  QuantLib::Instrument_setPricingEngine(option, pricing_engine)

  option
}

.qlg_barrier_type <- function(x) {
  if (!is.character(x)) {
    return(x)
  }

  x <- tolower(gsub("[^A-Za-z0-9]", "", trimws(x[[1]])))

  if (x %in% c("downout", "downandout", "do")) {
    return(QuantLib::Barrier_DownOut_get())
  }

  if (x %in% c("upout", "upandout", "uo")) {
    return(QuantLib::Barrier_UpOut_get())
  }

  if (x %in% c("downin", "downandin", "di")) {
    return(QuantLib::Barrier_DownIn_get())
  }

  if (x %in% c("upin", "upandin", "ui")) {
    return(QuantLib::Barrier_UpIn_get())
  }

  stop(
    "Unsupported barrier_type: ",
    x,
    ". Use 'down_out', 'up_out', 'down_in', or 'up_in'.",
    call. = FALSE
  )
}
#' Make a barrier option from trade data
#'
#' @param trade A one-row data frame containing barrier option trade fields.
#' @param pricing_engine Optional QuantLib pricing engine.
#'
#' @return QuantLib BarrierOption object.
#' @export
qlg_make_barrier_option_from_trade <- function(
    trade,
    pricing_engine = NULL
) {
  qlg_use_quantlib()

  if (!is.data.frame(trade) || nrow(trade) != 1) {
    stop("trade must be a one-row data frame.", call. = FALSE)
  }

  maturity_date <- qlg_trade_value(
    trade = trade,
    name = "maturity_date",
    default = NULL
  )

  if (is.null(maturity_date)) {
    maturity_date <- qlg_trade_value(
      trade = trade,
      name = "expiry_date",
      default = NULL
    )
  }

  if (is.null(maturity_date)) {
    stop("trade must contain maturity_date or expiry_date.", call. = FALSE)
  }

  spot <- qlg_trade_value(trade, "spot", default = NULL)
  strike <- qlg_trade_value(trade, "strike", default = NULL)
  barrier <- qlg_trade_value(trade, "barrier", default = NULL)

  if (is.null(spot)) {
    stop("trade must contain spot.", call. = FALSE)
  }

  if (is.null(strike)) {
    stop("trade must contain strike.", call. = FALSE)
  }

  if (is.null(barrier)) {
    stop("trade must contain barrier.", call. = FALSE)
  }

  qlg_make_barrier_option(
    spot = spot,
    strike = strike,
    maturity_date = maturity_date,
    barrier = barrier,
    barrier_type = qlg_trade_value(
      trade = trade,
      name = "barrier_type",
      default = "down_out"
    ),
    rebate = qlg_trade_value(
      trade = trade,
      name = "rebate",
      default = 0
    ),
    option_type = qlg_trade_value(
      trade = trade,
      name = "option_type",
      default = "call"
    ),
    valuation_date = qlg_trade_value(
      trade = trade,
      name = "valuation_date",
      default = qlg_eval_date_get()
    ),
    risk_free_rate = qlg_trade_value(
      trade = trade,
      name = "risk_free_rate",
      default = 0.03
    ),
    dividend_yield = qlg_trade_value(
      trade = trade,
      name = "dividend_yield",
      default = 0
    ),
    volatility = qlg_trade_value(
      trade = trade,
      name = "volatility",
      default = 0.20
    ),
    pricing_engine = pricing_engine
  )
}
#' Make an American vanilla option from trade data
#'
#' @param trade A one-row data frame containing American option trade fields.
#' @param pricing_engine Optional QuantLib pricing engine.
#'
#' @return QuantLib VanillaOption object.
#' @export
qlg_make_american_option_from_trade <- function(
    trade,
    pricing_engine = NULL
) {
  qlg_use_quantlib()

  if (!is.data.frame(trade) || nrow(trade) != 1) {
    stop("trade must be a one-row data frame.", call. = FALSE)
  }

  maturity_date <- qlg_trade_value(
    trade = trade,
    name = "maturity_date",
    default = NULL
  )

  if (is.null(maturity_date)) {
    maturity_date <- qlg_trade_value(
      trade = trade,
      name = "expiry_date",
      default = NULL
    )
  }

  if (is.null(maturity_date)) {
    stop("trade must contain maturity_date or expiry_date.", call. = FALSE)
  }

  spot <- qlg_trade_value(trade, "spot", default = NULL)
  strike <- qlg_trade_value(trade, "strike", default = NULL)

  if (is.null(spot)) {
    stop("trade must contain spot.", call. = FALSE)
  }

  if (is.null(strike)) {
    stop("trade must contain strike.", call. = FALSE)
  }

  qlg_make_american_option(
    spot = spot,
    strike = strike,
    maturity_date = maturity_date,
    option_type = qlg_trade_value(
      trade = trade,
      name = "option_type",
      default = "put"
    ),
    valuation_date = qlg_trade_value(
      trade = trade,
      name = "valuation_date",
      default = qlg_eval_date_get()
    ),
    risk_free_rate = qlg_trade_value(
      trade = trade,
      name = "risk_free_rate",
      default = 0.03
    ),
    dividend_yield = qlg_trade_value(
      trade = trade,
      name = "dividend_yield",
      default = 0
    ),
    volatility = qlg_trade_value(
      trade = trade,
      name = "volatility",
      default = 0.20
    ),
    steps = qlg_trade_value(
      trade = trade,
      name = "steps",
      default = 200L
    ),
    pricing_engine = pricing_engine
  )
}
#' Make an option from trade data
#'
#' This is a generic option trade dispatcher. It creates a European,
#' American, or barrier option depending on the trade fields.
#'
#' @param trade A one-row data frame containing option trade fields.
#' @param pricing_engine Optional QuantLib pricing engine.
#'
#' @return QuantLib option object.
#' @export
qlg_make_option_from_trade <- function(
    trade,
    pricing_engine = NULL
) {
  qlg_use_quantlib()

  if (!is.data.frame(trade) || nrow(trade) != 1) {
    stop("trade must be a one-row data frame.", call. = FALSE)
  }

  if (.qlg_option_trade_is_barrier(trade)) {
    return(
      qlg_make_barrier_option_from_trade(
        trade = trade,
        pricing_engine = pricing_engine
      )
    )
  }

  style <- .qlg_option_trade_field(
    trade = trade,
    names = c("exercise_style", "option_style", "style"),
    default = "european"
  )

  style <- .qlg_option_trade_token(style)

  if (style %in% c("american", "am")) {
    return(
      qlg_make_american_option_from_trade(
        trade = trade,
        pricing_engine = pricing_engine
      )
    )
  }

  if (style %in% c("european", "euro", "eu")) {
    return(
      qlg_make_european_option_from_trade(
        trade = trade,
        pricing_engine = pricing_engine
      )
    )
  }

  stop(
    "Unsupported option style: ",
    style,
    ". Use 'european' or 'american', or provide barrier fields.",
    call. = FALSE
  )
}

#' Summarise an option from trade data
#'
#' @param trade A one-row data frame containing option trade fields.
#' @param pricing_engine Optional QuantLib pricing engine.
#'
#' @return A tibble with option value and Greeks where available.
#' @export
qlg_option_summary_from_trade <- function(
    trade,
    pricing_engine = NULL
) {
  option <- qlg_make_option_from_trade(
    trade = trade,
    pricing_engine = pricing_engine
  )

  qlg_option_summary(option)
}

.qlg_option_trade_is_barrier <- function(trade) {
  barrier <- qlg_trade_value(
    trade = trade,
    name = "barrier",
    default = NULL
  )

  barrier_type <- qlg_trade_value(
    trade = trade,
    name = "barrier_type",
    default = NULL
  )

  product <- .qlg_option_trade_field(
    trade = trade,
    names = c("product", "product_type", "instrument_type", "trade_type"),
    default = ""
  )

  product <- .qlg_option_trade_token(product)

  !is.null(barrier) ||
    !is.null(barrier_type) ||
    grepl("barrier", product, fixed = TRUE)
}

.qlg_option_trade_field <- function(
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

    if (!is.null(value) && !is.na(value[[1]])) {
      value <- as.character(value[[1]])

      if (nzchar(trimws(value))) {
        return(value)
      }
    }
  }

  default
}

.qlg_option_trade_token <- function(x) {
  tolower(gsub("[^A-Za-z0-9]", "", as.character(x[[1]])))
}