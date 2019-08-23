#' Return commmon time series
#'
#' @param ... Dataframes containing time series observations
#'
#' @return Input dataframes with common time series to all input dataframes
#' @export
#'
#' @importFrom purrr reduce
#'
#' @examples
intersect_prices <- function(prices) {

  common_idx <- map(prices, "date_time") %>%
    purrr::reduce(intersect) %>%
    lubridate::as_datetime()

  purrr::map(prices, ~ dplyr::filter(., .data$date_time %in% common_idx))
}
