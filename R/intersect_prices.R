intersect_prices <- function(...){

  price_ts <- list(...)

  common_idx <- map(price_ts, "date_time") %>%
    reduce(intersect) %>%
    lubridate::as_datetime()

  purrr::map(price_ts, ~dplyr::filter(., .data$date_time %in% common_idx))

}
