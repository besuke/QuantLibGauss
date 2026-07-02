# bond_analytics.R


#' Create Fixed Rate Bond
#' @export
qlg_fixed_rate_bond <- function(
  issue_date = "2007-05-15",
  maturity_date = "2017-05-15",
  coupon_rate = 0.045,
  face_amount = 100,
  settlement_days = 3
) {
  schedule <- Schedule(
    qlg_date(issue_date),
    qlg_date(maturity_date),
    Period("Semiannual"),
    UnitedStates("GovernmentBond"),
    "Unadjusted",
    "Unadjusted",
    copyToR(DateGeneration(), "Backward"),
    FALSE
  )

  FixedRateBond(
    settlement_days,
    face_amount,
    schedule,
    coupon_rate,
    ActualActual("Bond"),
    "ModifiedFollowing",
    100.0,
    qlg_date(issue_date)
  )
}

#' Set Bond Pricing Engine
#'
#' @param bond QuantLib bond object.
#' @param curve QuantLib yield term structure.
#'
#' @return Bond with pricing engine.
#' @export
qlg_set_bond_pricing_engine <- function(bond, curve) {
  discounting_term_structure <- RelinkableYieldTermStructureHandle()

  invisible(
    RelinkableYieldTermStructureHandle_linkTo(
      discounting_term_structure,
      curve
    )
  )

  bond_engine <- DiscountingBondEngine(discounting_term_structure)

  invisible(
    Instrument_setPricingEngine(bond, bond_engine)
  )

  bond
}


#' Bond Yield
#' @export
qlg_bond_yield <- function(
  bond,
  day_counter = Actual365Fixed(),
  compounding = "Compounded",
  frequency = "Annual"
) {
  Bond_yield(
    bond,
    day_counter,
    compounding,
    frequency
  )
}

#' Bond Duration
#' @export
qlg_bond_duration <- function(
  bond,
  ytm = NULL,
  day_counter = Actual365Fixed(),
  compounding = "Compounded",
  frequency = "Annual"
) {
  if (is.null(ytm)) {
    ytm <- qlg_bond_yield(bond, day_counter, compounding, frequency)
  }

  BondFunctions_duration(
    bond,
    ytm,
    day_counter,
    compounding,
    frequency
  )
}

#' Bond Convexity
#' @export
qlg_bond_convexity <- function(
  bond,
  ytm = NULL,
  day_counter = Actual365Fixed(),
  compounding = "Compounded",
  frequency = "Annual"
) {
  if (is.null(ytm)) {
    ytm <- qlg_bond_yield(bond, day_counter, compounding, frequency)
  }

  BondFunctions_convexity(
    bond,
    ytm,
    day_counter,
    compounding,
    frequency
  )
}

#' Bond Accrued Amount
#' @export
qlg_bond_accrued <- function(bond) {
  Bond_accruedAmount(bond)
}


#' Bond Example
#' @export
qlg_bond_example <- function() {

  qlg_eval_date("2010-01-01")
  curve <- qlg_build_bond_discount_curve()
  bond <- qlg_zero_coupon_bond(curve)

  list(
    bond = bond,
    yield = qlg_bond_yield(bond),
    duration = qlg_bond_duration(bond),
    convexity = qlg_bond_convexity(bond),
    summary = qlg_bond_summary(bond)
  )
}

#' Bond Price-Yield Curve
#'
#' @export
qlg_bond_price_yield_curve <- function(
  bond,
  ytm_center = NULL,
  spread = 0.02,
  n = 41,
  day_counter = Actual360(),
  compounding = "Compounded",
  frequency = "Annual"
) {
  if (is.null(ytm_center)) {
    ytm_center <- qlg_bond_yield(
      bond,
      day_counter,
      compounding,
      frequency
    )
  }

  ytm <- seq(
    from = ytm_center - spread,
    to = ytm_center + spread,
    length.out = n
  )

  price <- purrr::map_dbl(
    ytm,
    ~ qlg_bond_price_from_yield(
      bond,
      .x,
      day_counter,
      compounding,
      frequency
    )
  )

  tibble::tibble(
    yield = ytm,
    clean_price = price
  )
}
#' Bond Price from Yield
#'
#' @export
qlg_bond_price_from_yield <- function(
  bond,
  ytm,
  day_counter = Actual360(),
  compounding = "Compounded",
  frequency = "Annual",
  settlement_date = NULL
) {
  if (is.null(settlement_date)) {
    Bond_cleanPrice(
      bond,
      ytm,
      day_counter,
      compounding,
      frequency
    )
  } else {
    Bond_cleanPrice(
      bond,
      ytm,
      day_counter,
      compounding,
      frequency,
      qlg_date(settlement_date)
    )
  }
}

#' Bond Yield from Clean Price
#'
#' @export
qlg_bond_yield_from_price <- function(
  bond,
  clean_price,
  day_counter = Actual360(),
  compounding = "Compounded",
  frequency = "Annual",
  settlement_date = NULL
) {
  price <- BondPrice(
    clean_price,
    BondPrice_Clean_get()
  )

  if (is.null(settlement_date)) {
    Bond_yield(
      bond,
      price,
      day_counter,
      compounding,
      frequency
    )
  } else {
    Bond_yield(
      bond,
      price,
      day_counter,
      compounding,
      frequency,
      qlg_date(settlement_date)
    )
  }
}
#' Bond DV01
#'
#' Dollar value of 1bp yield change.
#'
#' @export
qlg_bond_dv01 <- function(
  bond,
  ytm = NULL,
  bp = 1e-4,
  day_counter = Actual360(),
  compounding = "Compounded",
  frequency = "Annual"
) {
  if (is.null(ytm)) {
    ytm <- qlg_bond_yield(bond, day_counter, compounding, frequency)
  }

  price_down <- qlg_bond_price_from_yield(
    bond, ytm - bp, day_counter, compounding, frequency
  )

  price_up <- qlg_bond_price_from_yield(
    bond, ytm + bp, day_counter, compounding, frequency
  )

  (price_down - price_up) / 2
}

#' Bond PV01
#'
#' Present value change for 1bp yield change per face amount.
#'
#' @export
qlg_bond_pv01 <- function(
  bond,
  ytm = NULL,
  bp = 1e-4,
  day_counter = Actual360(),
  compounding = "Compounded",
  frequency = "Annual"
) {
  qlg_bond_dv01(
    bond,
    ytm,
    bp,
    day_counter,
    compounding,
    frequency
  )
}


#' Bond Cashflow Table
#'
#' @export
qlg_bond_cashflow_table <- function(bond) {
  qlg_leg_to_cashflow_tbl(Bond_cashflows(bond))
}

#' Create a zero coupon bond
#'
#' @export
qlg_zero_coupon_bond <- function(
    discount_curve,
    maturity = "2013-08-15",
    issue = "2008-08-15",
    face = 100
) {
  calendar <- QuantLib::UnitedStates("GovernmentBond")

  issueDate <- qlg_date(issue)

  maturityDate <- qlg_date(maturity)

  settlementDays <- 3

  redemption <- 100

  bond <- QuantLib::ZeroCouponBond(
    settlementDays,
    calendar,
    face,
    maturityDate,
    "Following",
    redemption,
    issueDate
  )

  engine <- QuantLib::DiscountingBondEngine(
    QuantLib::YieldTermStructureHandle(
      discount_curve
    )
  )

  QuantLib::Instrument_setPricingEngine(
    bond,
    engine
  )

  bond
}

#' Get bond coupon information
#'
#' @param bond A QuantLib bond object.
#' @param as_of Evaluation date. Default is  qlg_eval_date_get().
#'
#' @return A tibble with coupon cashflow information.
#'
#' @export
qlg_bond_coupon_info <- function(bond, as_of = qlg_eval_date_get()) {
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tibble", quietly = TRUE)

  cf <- qlg_bond_cashflow_table(bond)

  if (!"date" %in% names(cf)) {
    stop("qlg_bond_cashflow_table() must return a column named 'date'.")
  }

  if (!"amount" %in% names(cf)) {
    stop("qlg_bond_cashflow_table() must return a column named 'amount'.")
  }

  as_of <- as.Date(as_of)

  cf <- tibble::as_tibble(cf) |>
    dplyr::mutate(
      date = as.Date(.data$date),
      amount = as.numeric(.data$amount),
      is_future = .data$date > as_of
    )

  # Redemption/principal を除き、coupon cashflow だけを推定する
  if ("type" %in% names(cf)) {
    coupons <- cf |>
      dplyr::filter(
        !grepl("redemption|principal|notional", .data$type, ignore.case = TRUE)
      )
  } else {
    # type 列がない場合は、最大金額を元本償還とみなして除外する簡易版
    max_amount <- max(abs(cf$amount), na.rm = TRUE)

    coupons <- cf |>
      dplyr::filter(abs(.data$amount) < max_amount)
  }

  coupons <- coupons |>
    dplyr::arrange(.data$date)

  previous_candidates <- coupons$date[coupons$date <= as_of]
  next_candidates <- coupons$date[coupons$date > as_of]

  previous_coupon_date <- if (length(previous_candidates) > 0) {
    max(previous_candidates)
  } else {
    as.Date(NA)
  }

  next_coupon_date <- if (length(next_candidates) > 0) {
    min(next_candidates)
  } else {
    as.Date(NA)
  }

  coupons |>
    dplyr::mutate(
      coupon_no = dplyr::row_number(),
      is_previous_coupon = !is.na(previous_coupon_date) &
        .data$date == previous_coupon_date,
      is_next_coupon = !is.na(next_coupon_date) &
        .data$date == next_coupon_date
    )
}
#' Get bond settlement information
#'
#' @export
#'
qlg_bond_settlement_info <- function(bond, as_of = qlg_eval_date_get()) {
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)

  as_of <- as.Date(as_of)

  coupon_info <- qlg_bond_coupon_info(bond, as_of = as_of)

  previous_coupon <- coupon_info |>
    dplyr::filter(.data$is_previous_coupon)

  next_coupon <- coupon_info |>
    dplyr::filter(.data$is_next_coupon)

  previous_coupon_date <- if (nrow(previous_coupon) > 0) {
    previous_coupon$date[[1]]
  } else {
    as.Date(NA)
  }

  next_coupon_date <- if (nrow(next_coupon) > 0) {
    next_coupon$date[[1]]
  } else {
    as.Date(NA)
  }

  days_since_previous_coupon <- if (!is.na(previous_coupon_date)) {
    as.integer(as_of - previous_coupon_date)
  } else {
    NA_integer_
  }

  days_to_next_coupon <- if (!is.na(next_coupon_date)) {
    as.integer(next_coupon_date - as_of)
  } else {
    NA_integer_
  }

  accrued_amount <- tryCatch(
    Bond_accruedAmount(bond),
    error = function(e) {
      tryCatch(
        bond$accruedAmount(),
        error = function(e2) NA_real_
      )
    }
  )

  settlement_date <- tryCatch(
    qlg_ql_date_to_r_date(Bond_settlementDate(bond)),
    error = function(e) {
      tryCatch(
        qlg_ql_date_to_r_date(bond$settlementDate()),
        error = function(e2) as.Date(NA)
      )
    }
  )

  settlement_days <- tryCatch(
    Bond_settlementDays(bond),
    error = function(e) {
      tryCatch(
        bond$settlementDays(),
        error = function(e2) NA_integer_
      )
    }
  )

  tibble::tibble(
    evaluation_date = as_of,
    settlement_date = settlement_date,
    settlement_days = as.integer(settlement_days),
    previous_coupon_date = previous_coupon_date,
    next_coupon_date = next_coupon_date,
    days_since_previous_coupon = days_since_previous_coupon,
    days_to_next_coupon = days_to_next_coupon,
    accrued_amount = as.numeric(accrued_amount)
  )
}

# Internal helper: convert QuantLib Date to R Date
qlg_ql_date_to_r_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  tryCatch(
    as.Date(Date_ISO(x)),
    error = function(e1) {
      tryCatch(
        as.Date(x$ISO()),
        error = function(e2) as.Date(NA)
      )
    }
  )
}

#' Bond sensitivity table
#'
#' @param bond A QuantLib bond object.
#' @param ytm Base yield. If NULL, calculated from bond.
#' @param shifts_bp Yield shifts in basis points.
#' @param day_counter QuantLib day counter.
#' @param compounding Compounding convention.
#' @param frequency Payment frequency.
#'
#' @return A tibble with price sensitivity to yield shifts.
#'
#' @export
qlg_bond_sensitivity_table <- function(
  bond,
  ytm = NULL,
  shifts_bp = c(-100, -50, -25, -10, -1, 0, 1, 10, 25, 50, 100),
  day_counter = Actual365Fixed(),
  compounding = "Compounded",
  frequency = "Annual"
) {
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)

  if (is.null(ytm)) {
    ytm <- qlg_bond_yield(
      bond,
      day_counter = day_counter,
      compounding = compounding,
      frequency = frequency
    )
  }

  base_clean_price <- qlg_bond_price_from_yield(
    bond,
    ytm,
    day_counter = day_counter,
    compounding = compounding,
    frequency = frequency
  )

  out <- tibble::tibble(
    shift_bp = shifts_bp,
    yield = ytm + shifts_bp / 10000
  ) |>
    dplyr::mutate(
      clean_price = purrr::map_dbl(
        .data$yield,
        ~ qlg_bond_price_from_yield(
          bond,
          .x,
          day_counter = day_counter,
          compounding = compounding,
          frequency = frequency
        )
      ),
      price_change = .data$clean_price - base_clean_price,
      price_change_pct = .data$price_change / base_clean_price
    )

  out
}


#' Bond Z-spread
#'
#' Calculate the constant spread over a discount curve that reproduces
#' the observed clean price of the bond.
#'
#' @param bond A QuantLib bond object.
#' @param discount_curve A QuantLib yield term structure.
#' @param clean_price Clean price. If NULL, Bond_cleanPrice(bond) is used.
#' @param day_counter QuantLib day counter.
#' @param compounding Compounding convention.
#' @param frequency Payment frequency.
#' @param settlement_date Optional settlement date as character or Date.
#'
#' @return Z-spread as a numeric value.
#'
#' @export
qlg_bond_zspread <- function(
  bond,
  discount_curve,
  clean_price = NULL,
  day_counter = Actual365Fixed(),
  compounding = "Compounded",
  frequency = "Annual",
  settlement_date = NULL
) {
  if (!exists("BondFunctions_zSpread", mode = "function")) {
    stop("BondFunctions_zSpread() is not available in this QuantLib build.")
  }

  if (is.null(clean_price)) {
    clean_price <- Bond_cleanPrice(bond)
  }

  price <- BondPrice(
    as.numeric(clean_price),
    BondPrice_Clean_get()
  )

  curve_handle <- YieldTermStructureHandle(discount_curve)

  if (is.null(settlement_date)) {
    out <- tryCatch(
      BondFunctions_zSpread(
        bond,
        price,
        curve_handle,
        day_counter,
        compounding,
        frequency
      ),
      error = function(e1) {
        tryCatch(
          BondFunctions_zSpread(
            bond,
            price,
            discount_curve,
            day_counter,
            compounding,
            frequency
          ),
          error = function(e2) {
            stop(
              "BondFunctions_zSpread() failed. Original error: ",
              conditionMessage(e1),
              "\nFallback error: ",
              conditionMessage(e2)
            )
          }
        )
      }
    )
  } else {
    settlement_date <- qlg_date(as.character(as.Date(settlement_date)))

    out <- tryCatch(
      BondFunctions_zSpread(
        bond,
        price,
        curve_handle,
        day_counter,
        compounding,
        frequency,
        settlement_date
      ),
      error = function(e1) {
        tryCatch(
          BondFunctions_zSpread(
            bond,
            price,
            discount_curve,
            day_counter,
            compounding,
            frequency,
            settlement_date
          ),
          error = function(e2) {
            stop(
              "BondFunctions_zSpread() with settlement_date failed. Original error: ",
              conditionMessage(e1),
              "\nFallback error: ",
              conditionMessage(e2)
            )
          }
        )
      }
    )
  }

  as.numeric(out)
}


#' Bond Summary
#'
#' @param bond A QuantLib bond object.
#' @param discount_curve Optional QuantLib yield term structure for Z-spread.
#' @param day_counter QuantLib day counter.
#' @param compounding Compounding convention.
#' @param frequency Payment frequency.
#'
#' @return A tibble with bond summary information.
#'
#' @export
qlg_bond_summary <- function(
  bond,
  discount_curve = NULL,
  day_counter = Actual365Fixed(),
  compounding = "Compounded",
  frequency = "Annual"
) {
  requireNamespace("tibble", quietly = TRUE)

  ytm <- qlg_bond_yield(
    bond,
    day_counter = day_counter,
    compounding = compounding,
    frequency = frequency
  )

  dur <- qlg_bond_duration(
    bond,
    ytm = ytm,
    day_counter = day_counter,
    compounding = compounding,
    frequency = frequency
  )

  conv <- qlg_bond_convexity(
    bond,
    ytm = ytm,
    day_counter = day_counter,
    compounding = compounding,
    frequency = frequency
  )

  dv01 <- qlg_bond_dv01(
    bond,
    ytm = ytm,
    day_counter = day_counter,
    compounding = compounding,
    frequency = frequency
  )

  pv01 <- qlg_bond_pv01(
    bond,
    ytm = ytm,
    day_counter = day_counter,
    compounding = compounding,
    frequency = frequency
  )

  out <- tibble::tibble(
    item = c(
      "NPV",
      "Clean Price",
      "Dirty Price",
      "Accrued Amount",
      "Yield",
      "Yield (bp)",
      "Duration",
      "Convexity",
      "DV01",
      "PV01"
    ),
    value = c(
      Instrument_NPV(bond),
      Bond_cleanPrice(bond),
      Bond_dirtyPrice(bond),
      Bond_accruedAmount(bond),
      ytm,
      ytm * 10000,
      dur,
      conv,
      dv01,
      pv01
    )
  )

  if (!is.null(discount_curve) && exists("qlg_bond_zspread", mode = "function")) {
    z <- tryCatch(
      qlg_bond_zspread(
        bond = bond,
        discount_curve = discount_curve,
        clean_price = Bond_cleanPrice(bond),
        day_counter = day_counter,
        compounding = compounding,
        frequency = frequency
      ),
      error = function(e) NA_real_
    )

    out <- rbind(
      out,
      tibble::tibble(
        item = c("Z-spread", "Z-spread (bp)"),
        value = c(z, z * 10000)
      )
    )
  }

  out
}
