get_option_expiry_datetime <- function(epic) {
  response <- make_ig_request(
    path = file.path("markets", epic),
    api_version = 3
  ) %>%
    httr::content()

  lubridate::ymd_hm(response$instrument$expiryDetails$lastDealingDate)
}
