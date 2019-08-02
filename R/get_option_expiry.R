get_option_expiry <- function(epic){

  req_url <- glue::glue('https://api.ig.com/gateway/deal/markets/{epic}')

  response <- httr::GET(
    url = req_url,
    config = httr::add_headers(
      VERSION = 3,
      `X-IG-API-KEY` = Sys.getenv("IG_API_KEY"),
      CST = .session$headers$cst,
      `X-SECURITY-TOKEN` = .session$headers$`x-security-token`
    )
  )

  response %>% httr::content() %>% purrr::pluck('instrument', 'expiryDetails', 'lastDealingDate')

}
