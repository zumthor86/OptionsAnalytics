#' Compute options strategy greeks by time
#'
#' @param resolution Resolution of prices
#' @param strategy Option strategy object
#' @param n_prices Number of prices
#'
#' @return Dataframe of greeks
#' @export
#'
#' @examples
compute_strategy_greeks_by_time <- function(strategy, resolution, n_prices) {
  underlyer_data <- request_prices(strategy[[1]]$underlyer,
    resolution = resolution,
    n_prices = n_prices
  )

  positions <- purrr::map_dbl(strategy, "position")

  greeks <- purrr::map(strategy,
    greeks_by_time,
    underlyer_prices = underlyer_data$close,
    underlyer_datetimes = underlyer_data$date_time
  ) %>%
    purrr::map(~ dplyr::bind_rows(.) %>%
      as.matrix()) %>%
    purrr::map2(positions, ~ .x * .y) %>%
    purrr::reduce(.f = `+`) %>%
    dplyr::as_tibble()

  dplyr::bind_cols(date_time = underlyer_data$date_time, greeks)
}
