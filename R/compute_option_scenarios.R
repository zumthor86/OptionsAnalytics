compute_option_scenarios <- function(epic, underlyer_price, scenario_datetime) {

  option <- request_prices(epic, resolution = "MINUTE", n_prices=1)

  details <- get_option_details(epic)

  time_to_mat <- compute_ttm_years(scenario_datetime, expiry = details$expiry_datetime)

  vol <- iv_by_time(underlying_prices = underlyer_price,
                    option_prices = option$close,
                    option_datetimes = option$date_time, epic = epic)

  partial_options <- purrr::partial(fOptions::GBSOption, TypeFlag = tolower(details$option_type),
                                    X = details$strike_price,
                                    Time = time_to_mat,
                                    r = 0.05, b = 0)

  volatility_span <-

    underlyer_space <- underlyer_price*seq(0.9,1.10,0.01)

  volatility_space <- vol$implied_vol*volatility_span

  option_scenarios <- purrr::cross_df(list("vol"=volatility_space, "underlyer"=underlyer_space)) %>%
    dplyr::mutate(price = purrr::map2_dbl(vol, underlyer, ~ partial_options(S = .y, sigma = .x) %>% slot("price")))

  matrix(data = option_scenarios$price,
         nrow = length(underlyer_space),
         ncol=length(volatility_space),
         byrow = TRUE)
}
