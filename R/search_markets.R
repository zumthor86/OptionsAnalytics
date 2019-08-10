search_markets <- function(search_term){

  req_url <- httr::parse_url("https://api.ig.com/gateway/deal/markets")

  req_url$query <- list(searchTerm = search_term)

  comp <- httr::parse_url('https://api.ig.com/gateway/deal/markets?searchTerm=US%20500%20Call')

  response <- httr::GET(
    url = req_url,
    config = httr::add_headers(
      VERSION = 1,
      `X-IG-API-KEY` = Sys.getenv("IG_API_KEY"),
      CST = .session$headers$cst,
      `X-SECURITY-TOKEN` = .session$headers$`x-security-token`
    )
  ) %>% httr::content()

  bind_rows(response$markets)


}

