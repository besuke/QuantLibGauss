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

#' Set QuantLib evaluation date
#'
#' @export
qlg_set_eval_date <- function(date) {
  qd <- qlg_date(date)
  invisible(QuantLib::Settings_instance()$setEvaluationDate(d = qd))
  qd
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
