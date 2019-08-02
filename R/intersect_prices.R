intersect_prices <- function(...){

  price_ts <- list(...)

  common_idx <- lubridate::as_datetime(purrr::reduce(price_ts, ~intersect(.x$date_time, .y$date_time)))

  purrr::map(price_ts, ~dplyr::filter(., .data$date_time %in% common_idx))

}
