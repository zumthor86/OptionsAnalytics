priceReq <- function(epic = "IX.D.SPTRD.DAILY.IP", resolution='HOUR', n_prices=100) {
  reqUrl <- glue::glue("https://api.ig.com/gateway/deal/prices/{epic}?resolution={resolution}&max={n_prices}&pageSize={n_prices}")

  response <- httr::GET(
    url = reqUrl,
    config = httr::add_headers(
      VERSION = 3,
      `X-IG-API-KEY` = Sys.getenv("IG_API_KEY"),
      CST = .session$headers$cst,
      `X-SECURITY-TOKEN` = .session$headers$`x-security-token`
    )
  )

  prcs <- httr::content(response)

  closes <- sapply(prcs$prices, `[[`, c("closePrice", "bid"))
  opens <- sapply(prcs$prices, `[[`, c("openPrice", "bid"))
  highs <- sapply(prcs$prices, `[[`, c("highPrice", "bid"))
  lows <- sapply(prcs$prices, `[[`, c("lowPrice", "bid"))
  dateTime <-
    sapply(prcs$prices, `[[`, "snapshotTimeUTC") %>% lubridate::ymd_hms()
  volume <- sapply(prcs$prices, `[[`, "lastTradedVolume")


  data_frame(
    epic = epic,
    close = closes,
    open = opens,
    high = highs,
    low = lows,
    date_time = dateTime,
    volume = volume
  )

}
