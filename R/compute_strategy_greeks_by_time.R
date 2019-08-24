#' Compute options strategy greeks by time
#'
#' @param epics List of instrument epics
#' @param resolution Resolution of prices
#' @param n_prices Number of prices
#' @param positions Vector containing position for each option in the strategy
#'
#' @return
#' @export
#'
#' @examples
compute_strategy_greeks_by_time <- function(epics, resolution, n_prices, positions) {
  underlyer_prices <- get_option_underlyer(epics[[1]]) %>%
    request_prices(resolution, n_prices)

  epics %>%
    purrr::map(get_option_details) %>%
    purrr::map(~ greeks_by_time(
      underlyer_prices = underlyer_prices$close,
      underlyer_datetimes = underlyer_prices$date_time,
      strike_price = .$strike_price,
      option_type = .$option_type,
      expiry = .$expiry_datetime
    ) %>%
      dplyr::bind_rows() %>%
      as.matrix()) %>%
    purrr::map2(positions, ~ .x * .y) %>%
    purrr::reduce(.f = `+`)
}
