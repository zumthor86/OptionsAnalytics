#' Compute options strategy greeks by time
#'
#' @param strategy Option strategy object
#'
#' @return Dataframe of greeks
#' @export
#'
#' @examples
compute_strategy_greeks <- function(strategy) {
  positions <- purrr::map_dbl(strategy$legs, "position")

  greeks <- purrr::map(strategy$legs,
    greeks_by_time,
    underlyer_prices = strategy$underlyer_prices$close,
    underlyer_datetimes = strategy$underlyer_prices$date_time
  ) %>%
    purrr::map(~ dplyr::bind_rows(.) %>%
      as.matrix()) %>%
    purrr::map2(positions, ~ .x * .y) %>%
    purrr::reduce(.f = `+`) %>%
    dplyr::as_tibble()

  dplyr::bind_cols(date_time = strategy$underlyer_prices$date_time, greeks)
}

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

#' Compute greeks for an option
#'
#' @param r Annualized rate of interest, *as a decimal*, eg. 0.5 \% is 0.05
#' @param b Annualized cost of carry *as a decimal*
#' @param underlyer_annual_vol Annualized volatility of the underlyer *as a decimal*
#' @param option_type "c" or "p", call or put
#' @param exposure Amount bet per point, eg. 1
#' @param time_to_mat Time to maturity, in years
#' @param strike_price Option strike price
#' @param underlyer_price Option underlyer price
#' @param greek_selections Greeks to compute
#'
#' @importFrom fOptions GBSGreeks
#'
#' @return Named list of option sensitivities/greeks: "delta", "gamma", "vega", "rho", "theta"
#' @export
#'
#' @examples
get_greeks <- function(r = 0.05,
                       b = 0.05,
                       underlyer_annual_vol = 0.115,
                       exposure = 1,
                       time_to_mat,
                       strike_price,
                       underlyer_price,
                       option_type,
                       greek_selections = c("delta", "gamma", "vega", "theta")) {
  get_greek <- purrr::partial(fOptions::GBSGreeks,
    TypeFlag = tolower(option_type),
    S = underlyer_price,
    X = strike_price,
    Time = time_to_mat,
    r = r,
    b = b,
    sigma = underlyer_annual_vol
  )

  greeks <- purrr::map(greek_selections, ~ get_greek(Selection = .))

  names(greeks) <- greek_selections

  greeks$vega <- greeks$vega * exposure / 100

  greeks$theta <- greeks$theta * exposure / 100

  greeks
}
