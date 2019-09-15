#' Request historical prices
#'
#' @param epic Instrument ID
#' @param resolution Time interval of prices eg. "HOUR", "DAY", "MINUTE_15"
#' @param n_prices Number of prices to request
#' @param price_type "bid" or "ask"
#'
#' @return Dataframe of historical prices
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom purrr map
#' @importFrom purrr map_int
#' @importFrom purrr map_dbl
#' @importFrom purrr pluck
#' @importFrom dplyr bind_cols
#' @importFrom lubridate ymd_hms
#' @importFrom httr content
#'
#' @examples
request_prices <-
  function(epic = "IX.D.SPTRD.DAILY.IP",
             resolution = "HOUR",
             n_prices = 5,
             price_type = "bid") {
    assertthat::assert_that(is.numeric(n_prices), msg = "n_prices must be an integer")

    allowable_resolutions <- c(
      "SECOND", "MINUTE", "MINUTE_2", "MINUTE_3", "MINUTE_5",
      "MINUTE_10", "MINUTE_15", "MINUTE_30", "HOUR", "HOUR_2",
      "HOUR_3", "HOUR_4", "DAY", "WEEK", "MONTH"
    )

    is_allowed <- (resolution %in% allowable_resolutions)

    assertthat::assert_that(is_allowed, msg = "Invalid price resolution")

    response <- make_ig_request(
      api_version = 3,
      path = file.path("prices", epic),
      query = list(
        "resolution" = resolution,
        "max" = round(n_prices),
        "pageSize" = round(n_prices)
      )
    )

    prcs <- httr::content(response)

    price_names <- c(
      "closePrice",
      "openPrice",
      "highPrice",
      "lowPrice"
    )

    prices <- purrr::map_depth(prcs$prices, 2, ~ pluck(., price_type, .default = 0)) %>%
      dplyr::bind_rows() %>%
      dplyr::select(dplyr::one_of(price_names))

    dateTime <- purrr::map_chr(prcs$prices, "snapshotTimeUTC") %>%
      lubridate::ymd_hms()

    volume <- purrr::map_int(prcs$prices, "lastTradedVolume")

    names(prices) <- c("close", "open", "high", "low")

    dplyr::bind_cols(
      date_time = dateTime,
      prices,
      volume = volume,
      epic = rep(epic, length(volume))
    )
  }
