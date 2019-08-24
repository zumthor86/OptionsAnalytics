#' Extract option characteristics needed for greeks calculation
#'
#' @param epic Instrument epic
#'
#' @return List containing time to maturity, strike price, option type, underlyer price
#' @export
#'
#' @examples
prepare_greeks_calculation <- function(epic) {
  underlyer_quote <- request_prices(
    epic = get_option_underlyer(epic),
    resolution = "MINUTE",
    n_prices = 1
  )

  option_details <- get_option_details(epic)

  time_to_mat <- compute_ttm_years(underlyer_quote$date_time, option_details$expiry_datetime)

  list(
    time_to_mat = time_to_mat,
    strike_price = option_details$strike_price,
    option_type = option_details$option_type,
    underlyer_price = underlyer_quote$close
  )
}
