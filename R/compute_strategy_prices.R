#' Compute strategy prices
#'
#' @param strategy Strategy object
#'
#' @return Dataframe of strategy prices
#' @export
#'
#' @examples
compute_strategy_prices <- function(strategy) {
  prices <- purrr::map(strategy$legs, "prices")

  prices[["underlyer"]] <- strategy$underlyer_prices

  # TODO group by expiry and then intersect prices with outer joins, and then forward fill
  # prices with tidyr::fill

  common_prices <- intersect_prices(prices)

  positions_matrix <- purrr::map_dbl(strategy$legs, "position") %>%
    matrix(nrow = attr(strategy, "n_legs"))

  calc_strategy_prices(common_prices[1:attr(strategy, "n_legs")], positions_matrix)
}

#' Calculate strategy prices
#'
#' @param common_prices List of prices spanning same timeframe
#' @param positions_matrix Matrix containing options positions
#'
#' @return Dataframe of strategy prices
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
