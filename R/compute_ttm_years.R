#' Compute the time until maturity of an option, measured in years
#'
#' @param current_time Current DateTime
#' @param expiry DateTime of option expiry
#'
#' @return Time to maturity, measured in years
#' @export
#' @importFrom lubridate interval
#' @importFrom lubridate int_length
#' @importFrom lubridate dyears
#' @examples
compute_ttm_years <- function(current_time, expiry) {
  time_interval <- lubridate::interval(current_time, expiry)

  lubridate::int_length(time_interval) / as.numeric(lubridate::dyears())
}
