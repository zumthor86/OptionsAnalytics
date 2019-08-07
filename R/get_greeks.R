get_greeks <- function(underlyer="SP 500", strike_price, expiry, r=0.005, b=0, underlyer_annual_vol=0.075, option_type="C", exposure=1){

  underlyer_quote <- priceReq(epic=.underlyer_epics[underlyer],
                              resolution="MINUTE",
                              n_prices=1)

  epic <- get_option_epic(strike = strike_price,
                                 option_type = option_type,
                                 expiry = expiry)

  expiry_datetime <- get_option_expiry_datetime(epic)

  time_to_mat <- compute_ttm_years(underlyer_quote$date_time, expiry_datetime)

  get_greek <- purrr::partial(fOptions::GBSGreeks,
                              TypeFlag = tolower(option_type),
                               S = underlyer_quote$close,
                               X = strike_price,
                               Time = time_to_mat,
                               r=r,
                               b=b,
                               sigma = underlyer_annual_vol
                               )

  greek_selections <- c("delta", "gamma", "vega", "rho", "theta")

  greeks <- purrr::map(greek_selections, ~get_greek(Selection=.))

  names(greeks) <- greek_selections

  greeks$vega <- greeks$vega*exposure/100

  greeks$theta <- greeks$theta*exposure/100

  greeks

}
