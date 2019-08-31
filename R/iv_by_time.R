#' Compute Implied Volatility over time
#'
#' @param underlying_prices Close prices of the option's underlying
#' @param option_prices Dataframe consisting of close prices and date time of the option
#' @param strike Option strike price
#' @param option_type Option type, "c" or "p"
#'
#' @return
#' @export
#'
#' @examples
iv_by_time <- function(underlying_prices,
                       option_prices,
                       option_datetimes,
                       epic) {
  option_details <- get_option_details(epic)

  expiry <- option_details$expiry_datetime

  implied_vol <- purrr::partial(
    .f = fOptions::GBSVolatility,
    TypeFlag = tolower(option_details$option_type),
    r = 0.005,
    b = 0,
    X = option_details$strike_price
  )

  time_to_mat <- compute_ttm_years(option_datetimes, expiry)

  iv_by_time <- purrr::pmap_dbl(
    .f = implied_vol,
    .l = list(
      Time = time_to_mat,
      S = underlying_prices,
      price = option_prices
    )
  )

  dplyr::bind_cols(date_time = option_datetimes, implied_vol = iv_by_time)
}
