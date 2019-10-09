test_that("Refreshing prices returns correct price data", {

  start_time <-  format(strategy$legs[[1]]$prices$date_time[1],
                        format = "%Y-%m-%dT%H:%M:%S")

  end_time <- format(tail(strategy$legs[[1]]$prices$date_time,1),
                     format = "%Y-%m-%dT%H:%M:%S" )

  strategy$legs[[1]]$prices <- head(strategy$legs[[1]]$prices,10)

  price_data <- request_prices_range(strategy$legs[[1]]$epic, start_time, end_time)

  refreshed_strategy <- refresh_strategy(strategy)

  print(price_data$close)

  print(refreshed_strategy$legs[[1]]$prices$close)

  expect_equal(price_data$close, refreshed_strategy$legs[[1]]$prices$close)

})
