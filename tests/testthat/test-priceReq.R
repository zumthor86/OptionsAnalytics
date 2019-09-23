test_that("Throws input errors", {
  expect_error(request_prices(n_prices = "12"))
  expect_error(request_prices(resolution = "year"))
})

prices <- request_prices(resolution = "MINUTE", n_prices = 5)

test_that("Returns dataframe of correct dimensions", {
  expect_is(prices, "data.frame")
  expect_equal(dim(prices), c(5, 7))
})
