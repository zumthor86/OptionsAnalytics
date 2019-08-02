iv_by_time <- function(underlying_prices, option_prices, strike, option_type='C', resolution = 'HOUR'){


  expiry <- get_option_expiry(option_prices$epic[1])

  implied_vol <- purrr::partial(...f=fOptions::GBSVolatility,
                                TypeFlag=tolower(option_type),
                                r=0.005,
                                b=0,
                                X=strike)


  time_to_mat <- compute_ttm_years(option_prices$date_time, lubridate::ymd_hm(expiry))

  iv_by_time <- pmap_dbl(.f = possibly(implied_vol, NULL), .l = list(Time = time_to_mat,
                                   S = underlying_prices$close,
                                   price = option_prices$close))

  bind_cols(date_time = option_prices$date_time, implied_vol=iv_by_time)

}
