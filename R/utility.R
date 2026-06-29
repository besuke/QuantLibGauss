# utility.R

#' Convert ISO date to QuantLib Date
#'
#' @param x Date or character scalar.
#'
#' @return QuantLib Date object.
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
#' @param x QuantLib Date object.
#'
#' @return ISO date string.
#'
#' @export
qlg_iso <- function(x) {
  tryCatch(
    QuantLib::Date_ISO(x),
    error = function(e) as.character(x)
  )
}


#' Set QuantLib evaluation date
#'
#' @param x Date or character scalar.
#'
#' @return Invisibly returns QuantLib Date object.
#'
#' @export
qlg_eval_date <- function(x) {
  d <- qlg_date(x)

  invisible(
    QuantLib::Settings_instance()$setEvaluationDate(d = d)
  )

  invisible(d)
}


#' Get QuantLib evaluation date
#'
#' @return ISO date string.
#'
#' @export
qlg_eval_date_get <- function() {
  settings <- QuantLib::Settings_instance()

  d <- tryCatch(
    QuantLib::Settings_getEvaluationDate(settings),
    error = function(e1) {
      tryCatch(
        settings$evaluationDate(),
        error = function(e2) {
          stop(
            "Failed to get QuantLib evaluation date. ",
            conditionMessage(e1),
            call. = FALSE
          )
        }
      )
    }
  )

  qlg_iso(d)
}


#' Select QuantLib day counter
#'
#' @param day_counter Day counter name or QuantLib day counter object.
#'
#' @return QuantLib day counter object.
#'
#' @export
qlg_day_counter <- function(day_counter = "Actual365Fixed") {
  if (!is.character(day_counter)) {
    return(day_counter)
  }

  switch(
    day_counter,
    Actual365Fixed = QuantLib::Actual365Fixed(),
    Actual360 = QuantLib::Actual360(),
    Thirty360 = QuantLib::Thirty360(),
    Thirty360_European = QuantLib::Thirty360("European"),
    ActualActual_ISDA = QuantLib::ActualActual("ISDA"),
    ActualActual_Bond = QuantLib::ActualActual("Bond"),
    stop("Unsupported day counter: ", day_counter, call. = FALSE)
  )
}


#' Build QuantLib DateVector
#'
#' @param dates Character vector or Date vector.
#'
#' @return QuantLib DateVector.
#'
#' @export
qlg_make_date_vector <- function(dates) {
  dv <- QuantLib::DateVector()

  for (d in dates) {
    QuantLib::DateVector_append(
      dv,
      qlg_date(d)
    )
  }

  dv
}


#' Create QuantLib QuoteHandle
#'
#' @param x Numeric quote.
#'
#' @return QuantLib QuoteHandle.
#'
#' @export
qlg_quote_handle <- function(x) {
  QuantLib::QuoteHandle(
    QuantLib::SimpleQuote(x)
  )
}


#' Push rate helpers into QuantLib RateHelperVector
#'
#' @param helpers List of QuantLib rate helpers.
#'
#' @return QuantLib RateHelperVector.
#'
#' @export
qlg_push_rate_helpers <- function(helpers) {
  vec <- QuantLib::RateHelperVector()

  purrr::walk(
    helpers,
    ~ QuantLib::RateHelperVector_push_back(vec, .x)
  )

  vec
}


#' Convert QuantLib leg to cashflow table
#'
#' @param leg QuantLib Leg or CashFlow vector.
#'
#' @return A tibble with cashflow dates and amounts.
#'
#' @export
qlg_leg_to_cashflow_tbl <- function(leg) {
  tibble::tibble(
    idx = seq_len(leg$size())
  ) |>
    dplyr::mutate(
      cashflow = purrr::map(
        .data$idx,
        function(i) leg[i][[1]]
      ),
      date = purrr::map_chr(
        .data$cashflow,
        function(cf) {
          qlg_iso(
            QuantLib::CashFlow_date(cf)
          )
        }
      ),
      amount = purrr::map_dbl(
        .data$cashflow,
        function(cf) {
          QuantLib::CashFlow_amount(cf)
        }
      )
    ) |>
    dplyr::select(
      .data$date,
      .data$amount
    )
}
