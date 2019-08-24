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
#' @return Named list of option sensitivities/greeks: "delta", "gamma", "vega", "rho", "theta"
#' @export
#'
#' @examples
get_greeks <- function(r = 0.05,
                       b = 0,
                       underlyer_annual_vol = 0.075,
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
