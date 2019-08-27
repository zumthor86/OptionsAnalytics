#' Compute options strategy greeks by time
#'
#' @param epics List of instrument epics
#' @param resolution Resolution of prices
#' @param n_prices Number of prices
#' @param positions Vector containing position for each option in the strategy
#'
#' @return Dataframe of greeks
#' @export
#'
#' @examples
compute_strategy_greeks_by_time <- function(epics, underlyer_prices, underlyer_datetimes, positions) {

  greeks <- epics %>%
    purrr::map(get_option_details) %>%
    purrr::map(~ greeks_by_time(
      underlyer_prices = underlyer_prices,
      underlyer_datetimes = underlyer_datetimes,
      strike_price = .$strike_price,
      option_type = .$option_type,
      expiry = .$expiry_datetime
    ) %>%
      dplyr::bind_rows() %>%
      as.matrix()) %>%
    purrr::map2(positions, ~ .x * .y) %>%
    purrr::reduce(.f = `+`) %>%
    dplyr::as_data_frame()

  dplyr::bind_cols(date_time = underlyer_datetimes, greeks)


}
