#' Compute options implied volatility over time
#'
#' @param option_leg Option leg
#' @param underlying_prices Close prices of the option's underlying
#'
#' @return Dataframe of implied_volatility
#' @export
#'
#' @examples
compute_implied_volatility <- function(
                       option_leg,
                       underlying_prices
                       ) {

  assertthat::assert_that(length(option_leg$prices$close) == length(underlying_prices),
                          msg = "Underlyer prices must have same length as option prices")

  implied_vol <- purrr::partial(
    .f = fOptions::GBSVolatility,
    TypeFlag = tolower(option_leg$option_type),
    r = 0.005,
    b = 0,
    X = option_leg$strike_price
  )

  time_to_mat <- compute_ttm_years(option_leg$prices$date_time, option_leg$expiry)

  iv_by_time <- purrr::pmap_dbl(
    .f = implied_vol,
    .l = list(
      Time = time_to_mat,
      S = underlying_prices,
      price = option_leg$prices$close
    )
  )

  dplyr::bind_cols(date_time = option_leg$prices$date_time, implied_vol = iv_by_time)
}
