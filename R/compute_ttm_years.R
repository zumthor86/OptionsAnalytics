compute_ttm_years <- function(current_time, expiry) {
  time_to_mat <- lubridate::int_length(lubridate::interval(current_time, expiry))/as.numeric(lubridate::dyears())
}
