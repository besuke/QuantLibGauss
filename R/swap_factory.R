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
