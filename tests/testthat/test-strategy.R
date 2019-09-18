market_price <- signif(request_prices(n_prices = 1)$close, 2)
call <- glue::glue("OP.D.SPX1.{market_price}C.IP")
put <- glue::glue("OP.D.SPX1.{market_price}P.IP")


test_that("create_strategy works", {
  expect_s3_class(object = create_strategy(epics = c(call, put),
                                           positions = c(1,1),
                                           resolution = "HOUR", 5),
                  class = "option_strategy", exact = TRUE)

})




