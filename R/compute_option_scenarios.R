#' Compute option price scenarios at a given point in time
#'
#' @param scenario_datetime Datetime at which to compute option price scenarios
#' @param option_leg
#' @param underlyer_min
#' @param underlyer_max
#' @param vol_min
#' @param vol_max
#' @param n_scenarios
#'
#' @return Matrix of option prices for different underlyer prices and volatility
#' @export
#'
#' @examples
compute_option_scenarios <- function(option_leg,
                                     scenario_datetime,
                                     underlyer_prices,
                                     underlyer_min = 0.9,
                                     underlyer_max = 1.1,
                                     vol_min = 0.7,
                                     vol_max = 1.3,
                                     n_scenarios = 20) {

  time_to_mat <- compute_ttm_years(scenario_datetime, expiry = option_leg$expiry)

  current_vol <- compute_implied_volatility(option_leg, underlyer_prices)

  partial_options <- purrr::partial(fOptions::GBSOption,
    TypeFlag = tolower(option_leg$option_type),
    X = option_leg$strike_price,
    Time = time_to_mat,
    r = 0.05, b = 0.05
  )

  underlyer_space <- tail(underlyer_prices,1) * seq(underlyer_min, underlyer_max, length.out = n_scenarios)

  volatility_span <- seq(vol_min, vol_max, length.out = n_scenarios)

  volatility_space <- tail(current_vol$implied_vol,1) * volatility_span

  option_scenarios <- purrr::cross_df(list("vol" = volatility_space, "underlyer" = underlyer_space)) %>%
    dplyr::mutate(price = purrr::map2_dbl(vol, underlyer, ~ partial_options(S = .y, sigma = .x) %>% slot("price")))

  matrix(
    data = option_scenarios$price,
    nrow = length(underlyer_space),
    ncol = length(volatility_space),
    byrow = TRUE,
    dimnames = list(underlyer_space, volatility_span)
  )
}
