iv_by_time <- function(underlying_epic = "IX.D.SPTRD.DAILY.IP", strike, option_type='C', resolution = 'HOUR', n_prices=100, month=1){

  underlying_prices <- priceReq(epic = underlying_epic,
                                resolution = resolution,
                                n_prices = n_prices)

  option_prices <- get_option_price_history(strike = strike,
                           option_type = option_type,
                           month = month,
                           n_prices = n_prices,
                           resolution = resolution)

  expiry <- get_option_expiry(option_prices$epic[1])

  implied_vol <- purrr::partial(...f=fOptions::GBSVolatility,
                                TypeFlag=tolower(option_type),
                                r=0.005,
                                b=0,
                                X=strike)

  time_to_mat <- as.numeric((lubridate::ymd_hm(expiry)-option_prices$dateTime)/365)

  iv_by_time <- pmap_dbl(.f = possibly(implied_vol, NULL), .l = list(Time = time_to_mat,
                                   S = underlying_prices$close,
                                   price = option_prices$close))

  bind_cols(dateTime = option_prices$dateTime, implied_vol=iv_by_time)

}
