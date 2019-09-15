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
