# TODO: Document

get_option_details <- function(epic) {
  response <- make_ig_request(
    path = file.path(
      "markets",
      epic
    ),
    api_version = 3
  ) %>%
    httr::content()

  instrument_name <- response$instrument$name

  strike <- as.numeric(stringr::str_extract(instrument_name, "\\d{4,}"))

  type <- stringr::str_extract(instrument_name, "C(?=ALL)|P(?=UT)")

  expiry <- lubridate::ymd_hm(response$instrument$expiryDetails$lastDealingDate)

  list(
    strike_price = strike,
    option_type = type,
    expiry_datetime = expiry
  )
}
