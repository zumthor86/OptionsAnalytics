#' Compute greeks over time for single option
#'
#' @param underlyer_prices Numeric vector containing option underlyer prices
#' @param option_leg Option leg object
#' @param underlyer_datetimes DateTime vector of the option underlyer prices
#'
#' @return List of option greeks for each underlyer price
#' @export
#' @importFrom purrr map2
#'
#' @examples
greeks_by_time <- function(option_leg,
                           underlyer_prices,
                           underlyer_datetimes) {
  partial_greeks <- purrr::partial(get_greeks,
    strike_price = option_leg$strike_price,
    option_type = option_leg$option_type
  )


  time_to_mat <- compute_ttm_years(underlyer_datetimes, option_leg$expiry)

  purrr::map2(
    underlyer_prices,
    time_to_mat,
    ~ partial_greeks(underlyer_price = .x, time_to_mat = .y)
  )
}
