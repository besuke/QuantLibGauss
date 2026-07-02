# swap_factory.R

#' Create an OIS swap
#'
#' @param swap_tenor QuantLib Period object.
#' @param overnight_index QuantLib overnight index object.
#' @param fixed_rate Fixed rate.
#'
#' @return A QuantLib OIS swap object.
#' @export
qlg_make_ois <- function(
    swap_tenor,
    overnight_index,
    fixed_rate
) {
  qlg_use_quantlib()

  swap_builder <- QuantLib::MakeOIS(
    swapTenor = swap_tenor,
    overnightIndex = overnight_index,
    fixedRate = fixed_rate
  )

  QuantLib::MakeOIS_makeOIS(swap_builder)
}

#' Create an Eonia OIS swap
#'
#' @param swap_tenor QuantLib Period object.
#' @param forecast_handle QuantLib yield term structure handle.
#' @param fixed_rate Fixed rate.
#'
#' @return A QuantLib OIS swap object.
#' @export
qlg_make_eonia_ois <- function(
    swap_tenor,
    forecast_handle,
    fixed_rate
) {
  qlg_use_quantlib()

  overnight_index <- QuantLib::Eonia(forecast_handle)

  qlg_make_ois(
    swap_tenor = swap_tenor,
    overnight_index = overnight_index,
    fixed_rate = fixed_rate
  )
}

# R/swap_factory.R






# R/swap_factory.R





#' Make an OIS from trade data
#'
#' @param trade A one-row data frame containing OIS trade fields.
#' @param forecast_handle QuantLib forecast curve handle.
#'
#' @return A QuantLib OIS object.
#'
#' @export
qlg_make_ois_from_trade <- function(
    trade,
    forecast_handle
) {
  qlg_use_quantlib()

  stopifnot(is.data.frame(trade))
  stopifnot(nrow(trade) == 1)

  index <- toupper(
    as.character(
      qlg_trade_value(
        trade = trade,
        name = "index",
        default = "EONIA"
      )
    )
  )

  swap_tenor <- qlg_trade_period(
    trade = trade,
    n_col = "tenor_n",
    unit_col = "tenor_unit",
    default_n = 5,
    default_unit = "Years"
  )

  fixed_rate <- as.numeric(
    qlg_trade_value(
      trade = trade,
      name = "fixed_rate",
      default = 0
    )
  )

  if (index == "EONIA") {
    return(
      qlg_make_eonia_ois(
        swap_tenor = swap_tenor,
        forecast_handle = forecast_handle,
        fixed_rate = fixed_rate
      )
    )
  }

  stop(
    "Unsupported OIS index: ",
    index,
    ". Currently supported: EONIA."
  )
}

# R/swap_factory.R

qlg_vanilla_swap_type <- function(type = "payer") {
  type <- tolower(as.character(type))

  if (type %in% c("payer", "pay", "pay_fixed", "fixed_payer")) {
    return("Payer")
  }

  if (type %in% c("receiver", "receive", "receive_fixed", "fixed_receiver")) {
    return("Receiver")
  }

  stop(
    "Unsupported swap type: ",
    type,
    ". Use 'payer' or 'receiver'."
  )
}


qlg_make_ibor_index <- function(
    index = "Euribor6M",
    forecast_handle = NULL
) {
  qlg_use_quantlib()

  index <- toupper(as.character(index))

  if (index %in% c("EURIBOR6M", "EURIBOR_6M")) {
    if (is.null(forecast_handle)) {
      return(QuantLib::Euribor6M())
    }

    return(QuantLib::Euribor6M(forecast_handle))
  }

  if (index %in% c("EURIBOR3M", "EURIBOR_3M")) {
    tenor <- QuantLib::Period(3, "Months")

    if (is.null(forecast_handle)) {
      return(QuantLib::Euribor(tenor))
    }

    return(QuantLib::Euribor(tenor, forecast_handle))
  }

  if (index %in% c("USD_LIBOR_3M", "USDLIBOR3M", "LIBOR3M")) {
    tenor <- QuantLib::Period(3, "Months")

    if (is.null(forecast_handle)) {
      return(QuantLib::USDLibor(tenor))
    }

    return(QuantLib::USDLibor(tenor, forecast_handle))
  }

  if (index %in% c("USD_LIBOR_6M", "USDLIBOR6M", "LIBOR6M")) {
    tenor <- QuantLib::Period(6, "Months")

    if (is.null(forecast_handle)) {
      return(QuantLib::USDLibor(tenor))
    }

    return(QuantLib::USDLibor(tenor, forecast_handle))
  }

  stop(
    "Unsupported ibor index: ",
    index,
    ". Currently supported: Euribor6M, Euribor3M, USDLibor3M, USDLibor6M."
  )
}


qlg_make_schedule <- function(
    effective_date,
    maturity_date,
    tenor = QuantLib::Period(1, "Years"),
    calendar = QuantLib::TARGET(),
    convention = "ModifiedFollowing",
    termination_convention = "ModifiedFollowing",
    date_generation = "Forward",
    end_of_month = FALSE
) {
  qlg_use_quantlib()

  QuantLib::Schedule(
    qlg_date(effective_date),
    qlg_date(maturity_date),
    tenor,
    calendar,
    convention,
    termination_convention,
    QuantLib::copyToR(
      QuantLib::DateGeneration(),
      date_generation
    ),
    end_of_month
  )
}


#' Make a QuantLib VanillaSwap
#'
#' @param effective_date Effective date.
#' @param maturity_date Maturity date.
#' @param fixed_rate Fixed coupon rate.
#' @param notional Notional amount.
#' @param forecast_handle Forecast curve handle.
#' @param discount_handle Discount curve handle. If supplied, a DiscountingSwapEngine is attached.
#' @param swap_type Payer or receiver.
#' @param index Floating index name.
#' @param spread Floating spread.
#' @param fixed_tenor Fixed leg tenor.
#' @param floating_tenor Floating leg tenor.
#' @param fixed_day_counter Fixed leg day counter.
#' @param floating_day_counter Floating leg day counter.
#' @param calendar QuantLib calendar.
#' @param fixed_convention Fixed leg business day convention.
#' @param floating_convention Floating leg business day convention.
#'
#' @return A QuantLib VanillaSwap object.
#'
#' @export
qlg_make_vanilla_swap <- function(
    effective_date,
    maturity_date,
    fixed_rate,
    notional = 1,
    forecast_handle = NULL,
    discount_handle = NULL,
    swap_type = "payer",
    index = "Euribor6M",
    spread = 0,
    fixed_tenor = QuantLib::Period(1, "Years"),
    floating_tenor = QuantLib::Period(6, "Months"),
    fixed_day_counter = QuantLib::Thirty360("European"),
    floating_day_counter = QuantLib::Actual360(),
    calendar = QuantLib::TARGET(),
    fixed_convention = "ModifiedFollowing",
    floating_convention = "ModifiedFollowing"
) {
  qlg_use_quantlib()

  fixed_schedule <- qlg_make_schedule(
    effective_date = effective_date,
    maturity_date = maturity_date,
    tenor = fixed_tenor,
    calendar = calendar,
    convention = fixed_convention,
    termination_convention = fixed_convention
  )

  floating_schedule <- qlg_make_schedule(
    effective_date = effective_date,
    maturity_date = maturity_date,
    tenor = floating_tenor,
    calendar = calendar,
    convention = floating_convention,
    termination_convention = floating_convention
  )

  floating_index <- qlg_make_ibor_index(
    index = index,
    forecast_handle = forecast_handle
  )

  swap <- QuantLib::VanillaSwap(
    qlg_vanilla_swap_type(swap_type),
    as.numeric(notional),
    fixed_schedule,
    as.numeric(fixed_rate),
    fixed_day_counter,
    floating_schedule,
    floating_index,
    as.numeric(spread),
    floating_day_counter
  )

  if (!is.null(discount_handle)) {
    engine <- QuantLib::DiscountingSwapEngine(discount_handle)
    swap$setPricingEngine(engine)
  }

  swap
}


qlg_trade_value <- function(
    trade,
    name,
    default = NULL
) {
  stopifnot(is.data.frame(trade))

  if (!(name %in% names(trade))) {
    return(default)
  }

  value <- trade[[name]][[1]]

  if (length(value) == 0 || is.na(value)) {
    default
  } else {
    value
  }
}


qlg_trade_period <- function(
    trade,
    n_col,
    unit_col,
    default_n,
    default_unit
) {
  qlg_use_quantlib()

  n <- qlg_trade_value(
    trade = trade,
    name = n_col,
    default = default_n
  )

  unit <- qlg_trade_value(
    trade = trade,
    name = unit_col,
    default = default_unit
  )

  QuantLib::Period(
    as.integer(n),
    as.character(unit)
  )
}


#' Make a QuantLib VanillaSwap from trade data
#'
#' @param trade A one-row data frame containing VanillaSwap trade fields.
#' @param forecast_handle Forecast curve handle.
#' @param discount_handle Discount curve handle. If supplied, a DiscountingSwapEngine is attached.
#'
#' @return A QuantLib VanillaSwap object.
#'
#' @export
qlg_make_vanilla_swap_from_trade <- function(
    trade,
    forecast_handle = NULL,
    discount_handle = NULL
) {
  qlg_use_quantlib()

  stopifnot(is.data.frame(trade))
  stopifnot(nrow(trade) == 1)

  effective_date <- qlg_trade_value(
    trade = trade,
    name = "effective_date",
    default = NULL
  )

  maturity_date <- qlg_trade_value(
    trade = trade,
    name = "maturity_date",
    default = NULL
  )

  if (is.null(effective_date) || is.null(maturity_date)) {
    stop("trade must contain effective_date and maturity_date.")
  }

  fixed_tenor <- qlg_trade_period(
    trade = trade,
    n_col = "fixed_tenor_n",
    unit_col = "fixed_tenor_unit",
    default_n = 1,
    default_unit = "Years"
  )

  floating_tenor <- qlg_trade_period(
    trade = trade,
    n_col = "floating_tenor_n",
    unit_col = "floating_tenor_unit",
    default_n = 6,
    default_unit = "Months"
  )

  qlg_make_vanilla_swap(
    effective_date = as.character(effective_date),
    maturity_date = as.character(maturity_date),
    fixed_rate = as.numeric(
      qlg_trade_value(
        trade = trade,
        name = "fixed_rate",
        default = 0
      )
    ),
    notional = as.numeric(
      qlg_trade_value(
        trade = trade,
        name = "notional",
        default = 1
      )
    ),
    forecast_handle = forecast_handle,
    discount_handle = discount_handle,
    swap_type = qlg_trade_value(
      trade = trade,
      name = "swap_type",
      default = "payer"
    ),
    index = qlg_trade_value(
      trade = trade,
      name = "index",
      default = "Euribor6M"
    ),
    spread = as.numeric(
      qlg_trade_value(
        trade = trade,
        name = "spread",
        default = 0
      )
    ),
    fixed_tenor = fixed_tenor,
    floating_tenor = floating_tenor
  )
}


.qlg_quantlib_fun <- function(name) {
  ql <- asNamespace("QuantLib")

  if (!exists(name, envir = ql, inherits = FALSE)) {
    stop("QuantLib function is not available: ", name, call. = FALSE)
  }

  get(name, envir = ql, inherits = FALSE)
}

#' Build a QuantLib AssetSwap
#'
#' This is a thin wrapper around QuantLib AssetSwap.
#'
#' @param bond QuantLib bond object.
#' @param clean_price Bond clean price.
#' @param ibor_index QuantLib IborIndex object.
#' @param floating_schedule QuantLib Schedule object.
#' @param floating_day_counter QuantLib DayCounter object.
#' @param deal_maturity QuantLib Date object.
#' @param pay_bond_coupon Logical. TRUE means paying the bond coupon leg.
#' @param spread Floating leg spread.
#' @param par_asset_swap Logical. TRUE for par asset swap.
#' @param gearing Floating leg gearing.
#' @param non_par_repayment Non-par repayment amount.
#'
#' @return QuantLib AssetSwap object.
#' @export
qlg_make_asset_swap <- function(
  bond,
  clean_price,
  ibor_index,
  floating_schedule,
  floating_day_counter,
  deal_maturity,
  pay_bond_coupon = TRUE,
  spread = 0,
  par_asset_swap = TRUE,
  gearing = 1,
  non_par_repayment = 100
) {
  requireNamespace("QuantLib", quietly = TRUE)

  .qlg_quantlib_fun("AssetSwap__SWIG_0")(
    pay_bond_coupon,
    bond,
    clean_price,
    ibor_index,
    spread,
    floating_schedule,
    floating_day_counter,
    par_asset_swap,
    gearing,
    non_par_repayment,
    deal_maturity
  )
}

.qlg_asset_swap_set_pricing_engine <- function(asset_swap, engine) {
  .qlg_quantlib_fun("Instrument_setPricingEngine")(asset_swap, engine)
  invisible(asset_swap)
}

#' Attach a discounting engine to an AssetSwap
#'
#' @param asset_swap QuantLib AssetSwap object.
#' @param discount_curve_handle QuantLib YieldTermStructureHandle.
#'
#' @return The original AssetSwap object, invisibly.
#' @export
qlg_asset_swap_set_engine <- function(asset_swap, discount_curve_handle) {
  requireNamespace("QuantLib", quietly = TRUE)

  engine <- .qlg_quantlib_fun("DiscountingSwapEngine")(discount_curve_handle)
  .qlg_asset_swap_set_pricing_engine(asset_swap, engine)

  invisible(asset_swap)
}

.qlg_asset_swap_value <- function(asset_swap, fun_name) {
  out <- tryCatch(
    .qlg_quantlib_fun(fun_name)(asset_swap),
    error = function(e) NA_real_
  )

  as.numeric(out)
}

#' AssetSwap fair spread
#'
#' @param asset_swap QuantLib AssetSwap object.
#'
#' @return Numeric fair spread.
#' @export
qlg_asset_swap_fair_spread <- function(asset_swap) {
  .qlg_asset_swap_value(asset_swap, "AssetSwap_fairSpread")
}

#' AssetSwap fair clean price
#'
#' @param asset_swap QuantLib AssetSwap object.
#'
#' @return Numeric fair clean price.
#' @export
qlg_asset_swap_fair_clean_price <- function(asset_swap) {
  .qlg_asset_swap_value(asset_swap, "AssetSwap_fairCleanPrice")
}

#' Summarise an AssetSwap
#'
#' @param asset_swap QuantLib AssetSwap object.
#'
#' @return A tibble with AssetSwap analytics.
#' @export
qlg_asset_swap_summary <- function(asset_swap) {
  requireNamespace("tibble", quietly = TRUE)

  tibble::tibble(
    metric = c(
      "fair_spread",
      "fair_clean_price"
    ),
    value = c(
      qlg_asset_swap_fair_spread(asset_swap),
      qlg_asset_swap_fair_clean_price(asset_swap)
    )
  )
}
