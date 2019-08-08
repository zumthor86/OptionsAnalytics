get_option_expiry_datetime <- function(epic){

  req_url <- glue::glue("https://api.ig.com/gateway/deal/markets/{epic}")

  response <- httr::GET(
    url = req_url,
    config = httr::add_headers(
      VERSION = 3,
      `X-IG-API-KEY` = Sys.getenv("IG_API_KEY"),
      CST = .session$headers$cst,
      `X-SECURITY-TOKEN` = .session$headers$`x-security-token`
    )
  ) %>% httr::content()

  lubridate::ymd_hm(response$instrument$expiryDetails$lastDealingDate)

}
