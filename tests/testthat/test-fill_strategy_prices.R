test_that("Refreshing prices returns correct prices", {
  hours_diff <- lubridate::hour(Sys.time()) - lubridate::hour(lubridate::with_tz(Sys.time(), "UTC"))

  start_time <- format_price_request(strategy$legs[[1]]$prices$date_time[1], 0)

  end_time <- format_price_request(tail(strategy$legs[[1]]$prices$date_time, 1), 0)

  original_prices <- strategy$legs[[1]]$prices

  strategy$legs[[1]]$prices <- head(strategy$legs[[1]]$prices, 10)

  refreshed_strategy <- refresh_strategy(strategy)

  expect_equal(refreshed_strategy$legs[[1]]$prices$close, original_prices$close)
})

test_that("No missing price data", {
  diffs <- tail(strategy$legs[[1]]$prices$date_time, -1) - tail(dplyr::lag(strategy$legs[[1]]$prices$date_time), -1)

  expect_equal(as.numeric(mean(diffs)), 1)
})
