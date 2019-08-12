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
intersect_prices <- function(...){

  price_ts <- list(...)

  common_idx <- map(price_ts, "date_time") %>%
    purrr::reduce(intersect) %>%
    lubridate::as_datetime()

  purrr::map(price_ts, ~dplyr::filter(., .data$date_time %in% common_idx))

}
