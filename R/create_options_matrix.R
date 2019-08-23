#' Create options matrix of prices from epics
#'
#' @param epics List of epics
#' @param resolution Price resolution, eg. MINUTE_5, HOUR
#' @param n_prices Number of prices
#'
#' @return
#' @export
#'
#' @examples
create_options_matrix <- function(epics, resolution, n_prices){

  prices <- epics %>%
    map(~request_prices(.,resolution, n_prices))

  common_prices <- intersect_prices(prices)

  idx <- common_prices %>%
    purrr::map("date_time")

  prices <- common_prices %>%
    purrr::map("close") %>%
    bind_cols() %>%
    as.matrix()

  list(idx = idx[[1]],
       prices = prices)

}
