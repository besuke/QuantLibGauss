# Date and period utilities ------------------------------------------------

.qlg_as_ql_date <- function(x) {
  if (inherits(x, "_p_Date")) {
    return(x)
  }

  qlg_date(x)
}

#' Convert QuantLib object to character
#'
#' @param x Object to print.
#'
#' @return Character scalar.
#' @export
qlg_chr <- function(x) {
  tryCatch(
    x$`__str__`(),
    error = function(e1) {
      tryCatch(
        as.character(x),
        error = function(e2) "<unprintable>"
      )
    }
  )
}

#' Set QuantLib evaluation date
#'
#' @param eval_date ISO date string, R Date, or QuantLib Date.
#'
#' @return The QuantLib Date invisibly.
#' @export
qlg_set_eval_date <- function(eval_date) {
  d <- .qlg_as_ql_date(eval_date)
  QuantLib::Settings_instance()$setEvaluationDate(d)
  invisible(d)
}

#' Advance a QuantLib date by calendar days
#'
#' @param calendar_obj QuantLib calendar object.
#' @param date_obj ISO date string, R Date, or QuantLib Date.
#' @param n_days Number of days.
#'
#' @return A QuantLib Date object.
#' @export
qlg_advance_days <- function(calendar_obj, date_obj, n_days) {
  QuantLib::Calendar_advance(
    calendar_obj,
    .qlg_as_ql_date(date_obj),
    as.integer(n_days),
    "Days"
  )
}

#' Build a QuantLib Period
#'
#' @param x Tenor string such as 1D, 1W, 3M, 18M, or 2Y.
#' @param unit Optional unit when x is numeric.
#'
#' @return A QuantLib Period object.
#' @export
qlg_period <- function(x = 1, unit = NULL) {
  if (!is.null(unit)) {
    unit <- tolower(as.character(unit)[1])
    n <- as.integer(x)

    if (unit %in% c("d", "day", "days")) {
      return(qlg_period_days(n))
    }

    if (unit %in% c("w", "week", "weeks")) {
      return(qlg_period_weeks(n))
    }

    if (unit %in% c("m", "month", "months")) {
      return(qlg_period_months(n))
    }

    if (unit %in% c("y", "year", "years")) {
      return(qlg_period_years(n))
    }

    stop("Unsupported period unit: ", unit, call. = FALSE)
  }

  if (length(x) != 1 || is.na(x)) {
    stop("tenor must be a single non-NA string", call. = FALSE)
  }

  x <- trimws(toupper(as.character(x)))

  m <- regexec("^([0-9]+)\\s*([DWMY])$", x)
  hit <- regmatches(x, m)[[1]]

  if (length(hit) == 0) {
    stop(
      "Unsupported tenor format: ", x,
      ". Use forms like 1D, 1W, 3M, 18M, 2Y.",
      call. = FALSE
    )
  }

  n <- as.integer(hit[2])
  u <- hit[3]

  switch(
    u,
    "D" = qlg_period_days(n),
    "W" = qlg_period_weeks(n),
    "M" = qlg_period_months(n),
    "Y" = qlg_period_years(n),
    stop("Unsupported tenor unit: ", u, call. = FALSE)
  )
}

#' Build a QuantLib day Period
#'
#' @param n Number of days.
#'
#' @return A QuantLib Period object.
#' @export
qlg_period_days <- function(n) {
  QuantLib::Period(as.integer(n), "Days")
}

#' Build a QuantLib week Period
#'
#' @param n Number of weeks.
#'
#' @return A QuantLib Period object.
#' @export
qlg_period_weeks <- function(n) {
  QuantLib::Period(as.integer(n), "Weeks")
}

#' Build a QuantLib month Period
#'
#' @param n Number of months.
#'
#' @return A QuantLib Period object.
#' @export
qlg_period_months <- function(n) {
  QuantLib::Period(as.integer(n), "Months")
}

#' Build a QuantLib year Period
#'
#' @param n Number of years.
#'
#' @return A QuantLib Period object.
#' @export
qlg_period_years <- function(n) {
  QuantLib::Period(as.integer(n), "Years")
}

#' Build and print a QuantLib Period
#'
#' @param x Tenor string such as 1D, 1W, 3M, or 2Y.
#'
#' @return Character scalar.
#' @export
qlg_period_chr <- function(x) {
  qlg_chr(qlg_period(x))
}
