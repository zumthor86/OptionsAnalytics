initiate_ig_session("DEMO")

market_price <- signif(request_prices(n_prices = 2)$close[1], 2)

call <- glue::glue("OP.D.SPX1.{market_price}C.IP")

put <- glue::glue("OP.D.SPX1.{market_price}P.IP")

strategy <- create_strategy(
  epics = c(call, put),
  positions = c(1, 0.5),
  resolution = "HOUR",
  n_prices = 20
)

tmp_strat_file <- tempfile()
