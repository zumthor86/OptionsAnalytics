#' Compute Implied Volatility over time
#'
#' @param underlying_prices Close prices of the option's underlying
#' @param option_prices Close prices of the option itself
#' @param strike Option strike price
#' @param option_type Option type, "c" or "p"
#'
#' @return
#' @export
#'
#' @examples
iv_by_time <- function(underlying_prices,
                       option_prices,
                       strike,
                       option_type = "C") {
  expiry <- get_option_expiry_datetime(option_prices$epic[1])

  implied_vol <- purrr::partial(
    .f = fOptions::GBSVolatility,
    TypeFlag = tolower(option_type),
    r = 0.005,
    b = 0,
    X = strike
  )


  time_to_mat <- compute_ttm_years(option_prices$date_time, expiry)

  iv_by_time <- purrr::pmap_dbl(
    .f = implied_vol,
    .l = list(
      Time = time_to_mat,
      S = underlying_prices$close,
      price = option_prices$close
    )
  )

  dplyr::bind_cols(date_time = option_prices$date_time, implied_vol = iv_by_time)
}
