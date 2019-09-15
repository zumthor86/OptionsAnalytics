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

  common_prices <- intersect_prices(prices)

  positions_matrix <- purrr::map_dbl(strategy$legs, "position") %>%
    matrix(nrow = attr(strategy, "n_legs"))

  calc_strategy_prices(common_prices[1:attr(strategy, "n_legs")], positions_matrix)
}
