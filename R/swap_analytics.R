# swap_analytics.R

#' Swap NPV
#'
#' @param swap A QuantLib Swap object.
#'
#' @return Swap NPV.
#' @export
qlg_swap_npv <- function(swap) {
  swap$NPV()
}

#' Swap leg NPV
#'
#' @param swap A QuantLib Swap object.
#' @param leg_no Leg number. QuantLib uses 0-based leg indexing.
#'
#' @return Swap leg NPV.
#' @export
qlg_swap_leg_npv <- function(swap, leg_no) {
  swap$legNPV(as.integer(leg_no))
}

#' Swap fixed leg NPV
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @return Fixed leg NPV.
#' @export
qlg_swap_fixed_leg_npv <- function(swap) {
  qlg_swap_leg_npv(swap, 0L)
}

#' Swap floating leg NPV
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @return Floating leg NPV.
#' @export
qlg_swap_floating_leg_npv <- function(swap) {
  qlg_swap_leg_npv(swap, 1L)
}

#' Swap fair fixed rate
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @return Fair fixed rate. Returns NA if unavailable.
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
#' @return Fair spread. Returns NA if unavailable.
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
#' @return A tibble of fixed leg cashflows.
#' @export
qlg_swap_fixed_leg_table <- function(swap) {
  qlg_leg_to_cashflow_tbl(swap$fixedLeg())
}

#' Swap floating leg cashflow table
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @return A tibble of floating leg cashflows.
#' @export
qlg_swap_floating_leg_table <- function(swap) {
  qlg_leg_to_cashflow_tbl(swap$floatingLeg())
}

#' Swap summary
#'
#' @param swap A QuantLib VanillaSwap-like object.
#'
#' @return A tibble summarising swap valuation measures.
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
#' @return A tibble of overnight leg cashflows.
#' @export
qlg_ois_overnight_leg_table <- function(swap) {
  qlg_leg_to_cashflow_tbl(swap$overnightLeg())
}

#' Summarise OIS cashflow legs
#'
#' @param fixed_leg Fixed leg cashflow table.
#' @param overnight_leg Overnight leg cashflow table.
#'
#' @return A tibble summarising OIS cashflow legs.
#' @export
qlg_ois_summary <- function(fixed_leg, overnight_leg) {
  requireNamespace("tibble", quietly = TRUE)
  requireNamespace("dplyr", quietly = TRUE)

  pick_col <- function(x, candidates) {
    hit <- intersect(candidates, names(x))

    if (length(hit) == 0) {
      NA_character_
    } else {
      hit[[1]]
    }
  }

  safe_date_range <- function(x, date_col) {
    if (is.na(date_col) || nrow(x) == 0) {
      return(list(first = as.Date(NA), last = as.Date(NA)))
    }

    d <- suppressWarnings(as.Date(as.character(x[[date_col]])))

    if (all(is.na(d))) {
      return(list(first = as.Date(NA), last = as.Date(NA)))
    }

    list(
      first = min(d, na.rm = TRUE),
      last = max(d, na.rm = TRUE)
    )
  }

  summarise_leg <- function(x, leg_name) {
    amount_col <- pick_col(
      x,
      c("amount", "Amount", "cashflow_amount", "cashflow")
    )

    date_col <- pick_col(
      x,
      c("date", "Date", "payment_date", "pay_date")
    )

    date_range <- safe_date_range(x, date_col)

    total_amount <- if (is.na(amount_col)) {
      NA_real_
    } else {
      sum(as.numeric(x[[amount_col]]), na.rm = TRUE)
    }

    tibble::tibble(
      leg = leg_name,
      cashflow_count = nrow(x),
      first_payment_date = date_range$first,
      last_payment_date = date_range$last,
      total_amount = total_amount
    )
  }

  dplyr::bind_rows(
    summarise_leg(fixed_leg, "fixed_leg"),
    summarise_leg(overnight_leg, "overnight_leg")
  )
}

#' Run OIS cashflow example
#'
#' @return A list containing today, settlement date, fixed leg, overnight leg, and summary.
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


  swap <- qlg_make_eonia_ois(
    swap_tenor = QuantLib::Period(5, "Years"),
    forecast_handle = forecast_handle,
    fixed_rate = 0.002
  )

  fixed_leg <- qlg_swap_fixed_leg_table(swap)
  overnight_leg <- qlg_ois_overnight_leg_table(swap)

  list(
    today = QuantLib::Date_ISO(today),
    settlement_date = QuantLib::Date_ISO(settlement_date),
    fixed_leg = fixed_leg,
    overnight_leg = overnight_leg,
    summary = qlg_ois_summary(
      fixed_leg = fixed_leg,
      overnight_leg = overnight_leg
    )
  )
}
