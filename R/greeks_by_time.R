#' Compute an options greeks over time for single option
#'
#' @param underlyer_prices Numeric vector containing option underlyer prices
#' @param underlyer_datetimes DateTime vector of the option underlyer prices
#' @param strike_price Option strike
#' @param option_type Option type
#' @param expiry Option expiry
#'
#' @return List of option greeks for each underlyer price
#' @export
#'
#' @examples
greeks_by_time <- function(underlyer_prices,
                           underlyer_datetimes,
                           strike_price,
                           option_type,
                           expiry) {
  partial_greeks <- purrr::partial(get_greeks,
    strike_price = strike_price,
    option_type = option_type
  )


  time_to_mat <- compute_ttm_years(underlyer_datetimes, expiry)

  purrr::map2(
    underlyer_prices,
    time_to_mat,
    ~ partial_greeks(underlyer_price = .x, time_to_mat = .y)
  )
}
