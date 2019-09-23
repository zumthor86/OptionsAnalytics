test_that("create_strategy works", {
  expect_s3_class(
    object = strategy,
    class = "option_strategy", exact = TRUE
  )
})
