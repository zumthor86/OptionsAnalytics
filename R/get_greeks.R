#' Compute greeks for an option
#'
#' @param epic Option epic
#' @param r Annualized rate of interest, *as a decimal*, eg. 0.5 \% is 0.05
#' @param b Annualized cost of carry *as a decimal*
#' @param underlyer_annual_vol Annualized volatility of the underlyer *as a decimal*
#' @param option_type "c" or "p", call or put
#' @param exposure Amount bet per point, eg. 1
#'
#' @return Named list of option sensitivities/greeks: "delta", "gamma", "vega", "rho", "theta"
#' @export
#'
#' @examples
get_greeks <- function(epic,
                       r = 0.05,
                       b = 0,
                       underlyer_annual_vol = 0.075,
                       exposure = 1,
                       greek_selections = c("delta", "gamma", "vega", "theta")) {
  underlyer_quote <- request_prices(
    epic = get_option_underlyer(epic),
    resolution = "MINUTE",
    n_prices = 1
  )

  option_details <- get_option_details(epic)

  time_to_mat <- compute_ttm_years(underlyer_quote$date_time, option_details$expiry_datetime)

  get_greek <- purrr::partial(fOptions::GBSGreeks,
    TypeFlag = tolower(option_details$option_type),
    S = underlyer_quote$close,
    X = option_details$strike_price,
    Time = time_to_mat,
    r = r,
    b = b,
    sigma = underlyer_annual_vol
  )

  greeks <- purrr::map(greek_selections, ~ get_greek(Selection = .))

  names(greeks) <- greek_selections

  greeks$vega <- greeks$vega * exposure / 100

  # greeks$theta <- greeks$theta * exposure / 100

  greeks
}
