test_that("Strategy prices are calculated", {
  expect_s3_class(compute_strategy_prices(strategy), "tbl")
})
