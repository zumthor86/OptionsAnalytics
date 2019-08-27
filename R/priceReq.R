#' Request historical prices
#'
#' @param epic Instrument ID
#' @param resolution Time interval of prices eg. "HOUR", "DAY", "MINUTE_15"
#' @param n_prices Number of prices to request
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
             n_prices = 100
           ) {
    response <- make_ig_request(
      api_version = 3,
      path = file.path("prices", epic),
      query = list(
        "resolution" = resolution,
        "max" = n_prices,
        "pageSize" = n_prices
      )
    )

    prcs <- httr::content(response)

    price_names <- c(
      "closePrice",
      "openPrice",
      "highPrice",
      "lowPrice"
    )

    prices <- purrr::map_depth(prcs$prices, 2, ~ pluck(., "bid", .default = 0)) %>%
      dplyr::bind_rows() %>%
      dplyr::select(dplyr::one_of(price_names))

    dateTime <- purrr::map(prcs$prices, ~ purrr::pluck(., "snapshotTimeUTC")) %>%
      lubridate::ymd_hms()

    volume <- purrr::map_int(prcs$prices, ~ purrr::pluck(., "lastTradedVolume", .default = 0))

    names(prices) <- c("close", "open", "high", "low")

    dplyr::bind_cols(
      date_time = dateTime,
      prices,
      volume = volume,
      epic = rep(epic, length(volume))
    )
  }
