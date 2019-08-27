#' Create options matrix of prices from epics
#'
#' @param epics List of epics
#' @param resolution Price resolution, eg. MINUTE_5, HOUR
#' @param n_prices Number of prices to request
#'
#' @return Matrix of dimension (n_prices, n_options)
#' @export
#'
#' @examples
compute_strategy_prices <- function(common_prices, positions_matrix) {

  prices <- common_prices %>%
    purrr::map("close") %>%
    dplyr::bind_cols() %>%
    as.matrix()

  strategy_prices <- prices %*% positions_matrix

  tibble::tibble(
    date_time = common_prices[[1]]$date_time,
    close = strategy_prices
  )
}
