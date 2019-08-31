#' Compute option price scenarios at a given point in time
#'
#' @param epic Option epic
#' @param underlyer_price Option underlyers price
#' @param scenario_datetime Datetime at which to compute option price scenarios
#'
#' @return Matrix of option prices for different underlyer prices and volatility
#' @export
#'
#' @examples
compute_option_scenarios <- function(epic,
                                     underlyer_price,
                                     scenario_datetime,
                                     underlyer_min = 0.9,
                                     underlyer_max = 1.1,
                                     vol_min = 0.7,
                                     vol_max = 1.3,
                                     n_scenarios = 20) {
  option <- request_prices(epic, resolution = "MINUTE", n_prices = 1)

  details <- get_option_details(epic)

  time_to_mat <- compute_ttm_years(scenario_datetime, expiry = details$expiry_datetime)

  vol <- iv_by_time(
    underlying_prices = underlyer_price,
    option_prices = option$close,
    option_datetimes = option$date_time, epic = epic
  )

  partial_options <- purrr::partial(fOptions::GBSOption,
    TypeFlag = tolower(details$option_type),
    X = details$strike_price,
    Time = time_to_mat,
    r = 0.05, b = 0
  )

  underlyer_space <- underlyer_price * seq(underlyer_min, underlyer_max, length.out = n_scenarios)

  volatility_space <- vol$implied_vol * seq(vol_min, vol_max, length.out = n_scenarios)

  option_scenarios <- purrr::cross_df(list("vol" = volatility_space, "underlyer" = underlyer_space)) %>%
    dplyr::mutate(price = purrr::map2_dbl(vol, underlyer, ~ partial_options(S = .y, sigma = .x) %>% slot("price")))

  matrix(
    data = option_scenarios$price,
    nrow = length(underlyer_space),
    ncol = length(volatility_space),
    byrow = TRUE
  )
}
