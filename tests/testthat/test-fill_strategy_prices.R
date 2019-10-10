test_that("Refreshing prices returns correct price data", {
  hours_diff <- lubridate::hour(Sys.time()) - lubridate::hour(lubridate::with_tz(Sys.time(), "UTC"))

  start_time <- format_price_request(strategy$legs[[1]]$prices$date_time[1],0)

  end_time <- format_price_request(tail(strategy$legs[[1]]$prices$date_time, 1),0)

  strategy$legs[[1]]$prices <- head(strategy$legs[[1]]$prices, 10)

  price_data <- request_prices_range(
    strategy$legs[[1]]$epic,
    start_time,
    end_time
  )

  refreshed_strategy <- refresh_strategy(strategy)

  expect_equal(price_data$close, refreshed_strategy$legs[[1]]$prices$close)
})
