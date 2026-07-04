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
#' Make a Hull-White one-factor short-rate model
#'
#' @param term_structure Optional QuantLib yield term structure handle.
#' @param valuation_date Evaluation date used when term_structure is NULL.
#' @param rate Flat rate used when term_structure is NULL.
#' @param a Hull-White mean reversion.
#' @param sigma Hull-White volatility.
#' @param day_counter QuantLib day counter.
#'
#' @return QuantLib HullWhite model object.
#' @export
qlg_hull_white_model <- function(
    term_structure = NULL,
    valuation_date = qlg_eval_date_get(),
    rate = 0.03,
    a = 0.03,
    sigma = 0.01,
    day_counter = QuantLib::Actual365Fixed()
) {
  qlg_use_quantlib()
  requireNamespace("QuantLib", quietly = TRUE)

  if (is.null(term_structure)) {
    term_structure <- .qlg_interest_option_flat_curve(
      valuation_date = valuation_date,
      rate = rate,
      day_counter = day_counter
    )
  }

  QuantLib::HullWhite__SWIG_0(
    term_structure,
    as.numeric(a),
    as.numeric(sigma)
  )
}

#' Make a Hull-White cap/floor pricing engine
#'
#' @param term_structure Optional QuantLib yield term structure handle.
#' @param valuation_date Evaluation date used when term_structure is NULL.
#' @param rate Flat rate used when term_structure is NULL.
#' @param a Hull-White mean reversion.
#' @param sigma Hull-White volatility.
#' @param method Pricing method. Use "analytic" or "tree".
#' @param time_steps Number of tree time steps when method = "tree".
#' @param day_counter QuantLib day counter.
#'
#' @return QuantLib cap/floor pricing engine.
#' @export
qlg_hull_white_cap_floor_engine <- function(
    term_structure = NULL,
    valuation_date = qlg_eval_date_get(),
    rate = 0.03,
    a = 0.03,
    sigma = 0.01,
    method = c("analytic", "tree"),
    time_steps = 60L,
    day_counter = QuantLib::Actual365Fixed()
) {
  qlg_use_quantlib()
  requireNamespace("QuantLib", quietly = TRUE)

  method <- match.arg(method)

  if (is.null(term_structure)) {
    term_structure <- .qlg_interest_option_flat_curve(
      valuation_date = valuation_date,
      rate = rate,
      day_counter = day_counter
    )
  }

  model <- qlg_hull_white_model(
    term_structure = term_structure,
    a = a,
    sigma = sigma
  )

  if (identical(method, "analytic")) {
    return(
      QuantLib::AnalyticCapFloorEngine__SWIG_0(
        model,
        term_structure
      )
    )
  }

  QuantLib::TreeCapFloorEngine__SWIG_0(
    model,
    as.integer(time_steps),
    term_structure
  )
}

.qlg_interest_option_flat_curve <- function(
    valuation_date,
    rate,
    day_counter = QuantLib::Actual365Fixed()
) {
  valuation_date <- as.character(as.Date(valuation_date))
  qlg_eval_date(valuation_date)

  eval_date <- qlg_date(valuation_date)

  QuantLib::YieldTermStructureHandle(
    QuantLib::FlatForward(
      eval_date,
      as.numeric(rate),
      day_counter
    )
  )
}
#' Make a Hull-White swaption pricing engine
#'
#' @param term_structure Optional QuantLib yield term structure handle.
#' @param valuation_date Evaluation date used when term_structure is NULL.
#' @param rate Flat rate used when term_structure is NULL.
#' @param a Hull-White mean reversion.
#' @param sigma Hull-White volatility.
#' @param method Pricing method. Use "jamshidian" or "tree".
#' @param time_steps Number of tree time steps when method = "tree".
#' @param day_counter QuantLib day counter.
#'
#' @return QuantLib swaption pricing engine.
#' @export
qlg_hull_white_swaption_engine <- function(
    term_structure = NULL,
    valuation_date = qlg_eval_date_get(),
    rate = 0.03,
    a = 0.03,
    sigma = 0.01,
    method = c("jamshidian", "tree"),
    time_steps = 60L,
    day_counter = QuantLib::Actual365Fixed()
) {
  qlg_use_quantlib()
  requireNamespace("QuantLib", quietly = TRUE)

  method <- match.arg(method)

  if (is.null(term_structure)) {
    term_structure <- .qlg_interest_option_flat_curve(
      valuation_date = valuation_date,
      rate = rate,
      day_counter = day_counter
    )
  }

  model <- qlg_hull_white_model(
    term_structure = term_structure,
    a = a,
    sigma = sigma
  )

  if (identical(method, "jamshidian")) {
    return(
      QuantLib::JamshidianSwaptionEngine__SWIG_0(
        model,
        term_structure
      )
    )
  }

  QuantLib::TreeSwaptionEngine__SWIG_0(
    model,
    as.integer(time_steps),
    term_structure
  )
}

#' Make a QuantLib swaption
#'
#' @param underlying_swap QuantLib VanillaSwap object.
#' @param exercise_date Swaption exercise date.
#' @param pricing_engine Optional QuantLib pricing engine.
#'
#' @return QuantLib Swaption object.
#' @export
qlg_make_swaption <- function(
    underlying_swap,
    exercise_date,
    pricing_engine = NULL
) {
  qlg_use_quantlib()
  requireNamespace("QuantLib", quietly = TRUE)

  exercise <- QuantLib::EuropeanExercise(
    qlg_date(as.character(as.Date(exercise_date)))
  )

  swaption <- QuantLib::Swaption__SWIG_2(
    underlying_swap,
    exercise
  )

  if (!is.null(pricing_engine)) {
    QuantLib::Instrument_setPricingEngine(
      swaption,
      pricing_engine
    )
  }

  swaption
}

#' Calculate swaption NPV
#'
#' @param swaption QuantLib Swaption object.
#'
#' @return Numeric NPV.
#' @export
qlg_swaption_npv <- function(swaption) {
  .qlg_swaption_value(
    swaption,
    "Instrument_NPV"
  )
}

#' Calculate swaption vega
#'
#' @param swaption QuantLib Swaption object.
#'
#' @return Numeric vega if available.
#' @export
qlg_swaption_vega <- function(swaption) {
  .qlg_swaption_value(
    swaption,
    "Swaption_vega"
  )
}

#' Calculate swaption annuity
#'
#' @param swaption QuantLib Swaption object.
#'
#' @return Numeric annuity if available.
#' @export
qlg_swaption_annuity <- function(swaption) {
  .qlg_swaption_value(
    swaption,
    "Swaption_annuity"
  )
}

#' Summarise a swaption
#'
#' @param swaption QuantLib Swaption object.
#'
#' @return A tibble with swaption analytics.
#' @export
qlg_swaption_summary <- function(swaption) {
  requireNamespace("tibble", quietly = TRUE)

  tibble::tibble(
    metric = c("npv", "vega", "annuity"),
    value = c(
      qlg_swaption_npv(swaption),
      qlg_swaption_vega(swaption),
      qlg_swaption_annuity(swaption)
    )
  )
}

.qlg_swaption_value <- function(swaption, fun_name) {
  out <- tryCatch(
    .qlg_quantlib_fun(fun_name)(swaption),
    error = function(e) NA_real_
  )

  as.numeric(out)
}
#' Make a Hull-White swaption from trade data
#'
#' @param trade A one-row data frame containing swaption trade fields.
#' @param forecast_handle Optional forecast curve handle.
#' @param discount_handle Optional discount curve handle.
#' @param pricing_engine Optional QuantLib swaption pricing engine.
#'
#' @return QuantLib Swaption object.
#' @export
qlg_make_swaption_from_trade <- function(
    trade,
    forecast_handle = NULL,
    discount_handle = NULL,
    pricing_engine = NULL
) {
  qlg_use_quantlib()

  if (!is.data.frame(trade) || nrow(trade) != 1) {
    stop("trade must be a one-row data frame.", call. = FALSE)
  }

  exercise_date <- .qlg_interest_option_required_field(
    trade = trade,
    names = c("exercise_date", "option_expiry_date", "expiry_date"),
    label = "exercise_date, option_expiry_date, or expiry_date"
  )

  valuation_date <- .qlg_interest_option_trade_field(
    trade = trade,
    names = c("valuation_date", "eval_date"),
    default = qlg_eval_date_get()
  )

  rate <- .qlg_interest_option_trade_field(
    trade = trade,
    names = c("discount_rate", "risk_free_rate", "rate"),
    default = 0.03
  )

  day_counter <- QuantLib::Actual365Fixed()

  term_structure <- discount_handle

  if (is.null(term_structure)) {
    term_structure <- .qlg_interest_option_flat_curve(
      valuation_date = valuation_date,
      rate = rate,
      day_counter = day_counter
    )
  }

  if (is.null(forecast_handle)) {
    forecast_handle <- term_structure
  }

  if (is.null(discount_handle)) {
    discount_handle <- term_structure
  }

  if (is.null(pricing_engine)) {
    pricing_engine <- qlg_hull_white_swaption_engine(
      term_structure = term_structure,
      a = .qlg_interest_option_trade_field(
        trade = trade,
        names = c("hw_a", "hull_white_a", "mean_reversion"),
        default = 0.03
      ),
      sigma = .qlg_interest_option_trade_field(
        trade = trade,
        names = c("hw_sigma", "hull_white_sigma", "short_rate_volatility"),
        default = 0.01
      ),
      method = .qlg_interest_option_trade_field(
        trade = trade,
        names = c("swaption_method", "engine_method", "method"),
        default = "jamshidian"
      ),
      time_steps = .qlg_interest_option_trade_field(
        trade = trade,
        names = c("time_steps", "tree_steps"),
        default = 60L
      )
    )
  }

  underlying_swap <- qlg_make_vanilla_swap_from_trade(
    trade = trade,
    forecast_handle = forecast_handle,
    discount_handle = discount_handle
  )

  qlg_make_swaption(
    underlying_swap = underlying_swap,
    exercise_date = exercise_date,
    pricing_engine = pricing_engine
  )
}
#' Make a QuantLib Bermudan swaption
#'
#' @param underlying_swap QuantLib VanillaSwap object.
#' @param exercise_dates Bermudan exercise dates.
#' @param pricing_engine Optional QuantLib pricing engine.
#'
#' @return QuantLib Swaption object.
#' @export
qlg_make_bermudan_swaption <- function(
    underlying_swap,
    exercise_dates,
    pricing_engine = NULL
) {
  qlg_use_quantlib()
  requireNamespace("QuantLib", quietly = TRUE)

  exercise <- QuantLib::BermudanExercise__SWIG_1(
    .qlg_bermudan_exercise_date_vector(exercise_dates)
  )

  swaption <- QuantLib::Swaption__SWIG_2(
    underlying_swap,
    exercise
  )

  if (!is.null(pricing_engine)) {
    QuantLib::Instrument_setPricingEngine(
      swaption,
      pricing_engine
    )
  }

  swaption
}

#' Make a Bermudan swaption from trade data
#'
#' @param trade A one-row data frame containing Bermudan swaption trade fields.
#' @param forecast_handle Optional forecast curve handle.
#' @param discount_handle Optional discount curve handle.
#' @param pricing_engine Optional QuantLib swaption pricing engine.
#'
#' @return QuantLib Swaption object.
#' @export
qlg_make_bermudan_swaption_from_trade <- function(
    trade,
    forecast_handle = NULL,
    discount_handle = NULL,
    pricing_engine = NULL
) {
  qlg_use_quantlib()

  if (!is.data.frame(trade) || nrow(trade) != 1) {
    stop("trade must be a one-row data frame.", call. = FALSE)
  }

  exercise_dates <- .qlg_interest_option_raw_field(
    trade = trade,
    names = c("exercise_dates", "bermudan_dates", "exercise_schedule"),
    default = NULL
  )

  if (is.null(exercise_dates)) {
    stop(
      "trade must contain exercise_dates, bermudan_dates, or exercise_schedule.",
      call. = FALSE
    )
  }

  valuation_date <- .qlg_interest_option_trade_field(
    trade = trade,
    names = c("valuation_date", "eval_date"),
    default = qlg_eval_date_get()
  )

  rate <- .qlg_interest_option_trade_field(
    trade = trade,
    names = c("discount_rate", "risk_free_rate", "rate"),
    default = 0.03
  )

  day_counter <- QuantLib::Actual365Fixed()

  term_structure <- discount_handle

  if (is.null(term_structure)) {
    term_structure <- .qlg_interest_option_flat_curve(
      valuation_date = valuation_date,
      rate = rate,
      day_counter = day_counter
    )
  }

  if (is.null(forecast_handle)) {
    forecast_handle <- term_structure
  }

  if (is.null(discount_handle)) {
    discount_handle <- term_structure
  }

  if (is.null(pricing_engine)) {
    method <- .qlg_interest_option_trade_field(
      trade = trade,
      names = c("swaption_method", "engine_method", "method"),
      default = "tree"
    )

    method <- .qlg_interest_option_token(method)

    if (!identical(method, "tree")) {
      stop(
        "Bermudan swaption currently supports method = 'tree'.",
        call. = FALSE
      )
    }

    pricing_engine <- qlg_hull_white_swaption_engine(
      term_structure = term_structure,
      a = .qlg_interest_option_trade_field(
        trade = trade,
        names = c("hw_a", "hull_white_a", "mean_reversion"),
        default = 0.03
      ),
      sigma = .qlg_interest_option_trade_field(
        trade = trade,
        names = c("hw_sigma", "hull_white_sigma", "short_rate_volatility"),
        default = 0.01
      ),
      method = "tree",
      time_steps = .qlg_interest_option_trade_field(
        trade = trade,
        names = c("time_steps", "tree_steps"),
        default = 60L
      )
    )
  }

  underlying_swap <- qlg_make_vanilla_swap_from_trade(
    trade = trade,
    forecast_handle = forecast_handle,
    discount_handle = discount_handle
  )

  qlg_make_bermudan_swaption(
    underlying_swap = underlying_swap,
    exercise_dates = exercise_dates,
    pricing_engine = pricing_engine
  )
}

.qlg_bermudan_exercise_date_vector <- function(exercise_dates) {
  dates <- .qlg_interest_option_parse_exercise_dates(exercise_dates)

  out <- QuantLib::DateVector()

  for (d in dates) {
    QuantLib::DateVector_append(
      out,
      qlg_date(d)
    )
  }

  out
}

.qlg_interest_option_parse_exercise_dates <- function(exercise_dates) {
  if (is.list(exercise_dates) && length(exercise_dates) == 1) {
    exercise_dates <- exercise_dates[[1]]
  }

  if (length(exercise_dates) == 1 && is.character(exercise_dates)) {
    exercise_dates <- unlist(
      strsplit(
        exercise_dates,
        split = "[,;]",
        perl = TRUE
      )
    )
  }

  dates <- trimws(as.character(unlist(exercise_dates)))
  dates <- dates[nzchar(dates)]

  if (!length(dates)) {
    stop("exercise_dates must contain at least one date.", call. = FALSE)
  }

  as.character(as.Date(dates))
}

.qlg_interest_option_raw_field <- function(
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

    if (!is.null(value)) {
      return(value)
    }
  }

  default
}