#' Compute option price scenarios at a given point in time
#'
#' @param scenario_datetime Datetime at which to compute option price scenarios
#' @param option_leg Option Leg object
#' @param vol_min Lower bound of volatility scenarios as a fraction of current volatility
#' @param vol_max Upper bound of volatility scenarios as a fraction of current volatility
#' @param n_scenarios Number of scenarios to compute for underlyer volatility
#' @param underlyer_min Minimum underlyer price
#' @param underlyer_max Maximum underlyer price
#' @param underlyer_prices Vector of underlyer closing prices, matching the option leg prices
#'
#' @return Matrix of option prices for different underlyer prices and volatility
#' @export
#'
#' @importFrom purrr partial
#' @importFrom fOptions GBSOption
#' @importFrom purrr cross_df
#' @importFrom purrr map2_dbl
#' @importFrom dplyr mutate
#' @importFrom utils tail
#' @importFrom rlang .data
#' @examples
compute_option_scenarios <- function(option_leg,
                                     scenario_datetime,
                                     underlyer_prices,
                                     underlyer_min,
                                     underlyer_max,
                                     underlyer_margin = 20,
                                     vol_min = 0.7,
                                     vol_max = 1.3,
                                     n_scenarios = 20) {
  time_to_mat <- compute_ttm_years(scenario_datetime, expiry = option_leg$expiry)

  if (time_to_mat < 0) {
    warning("Scenario datetime is after option expiry", call. = FALSE)
    time_to_mat <- 0
  }

  current_vol <- compute_implied_volatility(option_leg, underlyer_prices)

  partial_options <- purrr::partial(fOptions::GBSOption,
    TypeFlag = tolower(option_leg$option_type),
    X = option_leg$strike_price,
    Time = time_to_mat,
    r = 0.05, b = 0.05
  )

  underlyer_space <- seq(underlyer_min-underlyer_margin, underlyer_max+underlyer_margin, 5)

  volatility_span <- seq(vol_min, vol_max, length.out = n_scenarios)

  volatility_space <- utils::tail(current_vol$implied_vol, 1) * volatility_span

  option_scenarios <- purrr::cross_df(list("vol" = volatility_space, "underlyer" = underlyer_space)) %>%
    dplyr::mutate(price = purrr::map2_dbl(.data$vol, .data$underlyer, ~ partial_options(S = .y, sigma = .x) %>% slot("price")))

  option_scenarios$price[is.nan(option_scenarios$price)] <- 0

  matrix(
    data = option_scenarios$price,
    nrow = length(underlyer_space),
    ncol = n_scenarios,
    byrow = TRUE,
    dimnames = list(underlyer_space, volatility_span)
  )
}
