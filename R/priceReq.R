request_prices <-
  function(epic = "IX.D.SPTRD.DAILY.IP",
           resolution = 'HOUR',
           n_prices = 100) {
    reqUrl <-
      glue::glue(
        "https://api.ig.com/gateway/deal/prices/{epic}?resolution={resolution}&max={n_prices}&pageSize={n_prices}"
      )

    Sys.sleep(2)

    response <- httr::GET(
      url = reqUrl,
      config = httr::add_headers(
        VERSION = 3,
        `X-IG-API-KEY` = Sys.getenv("IG_API_KEY"),
        CST = .session$headers$cst,
        `X-SECURITY-TOKEN` = .session$headers$`x-security-token`
      )
    )

    assertthat::assert_that(response$status_code==200, msg = glue::glue("Response code: {response$status_code}"))

    prcs <- httr::content(response)

    price_names <- c("closePrice",
                     "openPrice",
                     "highPrice",
                     "lowPrice")

    prices <- map(price_names,
                  function(x)
                    map_dbl(prcs$prices,
                            ~ purrr::possibly(pluck(., x, "bid"))))

    dateTime <- map(prcs$prices, ~ pluck(., "snapshotTimeUTC")) %>%
      lubridate::ymd_hms()

    volume <- map_int(prcs$prices, ~ purrr::possibly(pluck(., "lastTradedVolume")))

    names(prices) <- c('close', 'open', 'high', 'low')

      bind_cols(date_time=dateTime,
                prices,
                volume=volume,
                epic= rep(epic, length(volume)))

  }
