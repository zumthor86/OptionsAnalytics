#' Calculate strategy prices
#'
#' @param common_prices List of prices spanning same timeframe
#' @param positions_matrix Matrix containing options positions
#'
#' @return Dataframe of strategy prices
#'
#'
#' @examples
calc_strategy_prices <- function(common_prices, positions_matrix) {
  prices <- common_prices %>%
    purrr::map("close") %>%
    dplyr::bind_cols() %>%
    as.matrix()

  strategy_prices <- prices %*% positions_matrix

  tibble::tibble(
    date_time = common_prices[[1]]$date_time,
    close = strategy_prices[, 1]
  )
}
