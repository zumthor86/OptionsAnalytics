compute_ttm_years <- function(current_time, expiry) {

  time_interval <- lubridate::interval(current_time, expiry)

  lubridate::int_length(time_interval) / as.numeric(lubridate::dyears())
}
