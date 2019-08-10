request_market_navigation <- function(node_id){

  request_url <- glue::glue("https://api.ig.com/gateway/deal/marketnavigation/{node_id}")

  response <- httr::GET(
    url = request_url,
    config = httr::add_headers(
      VERSION = 1,
      `X-IG-API-KEY` = Sys.getenv("IG_API_KEY"),
      CST = .session$headers$cst,
      `X-SECURITY-TOKEN` = .session$headers$`x-security-token`
    )
  ) %>% httr::content()

}
