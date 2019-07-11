get_straddle_price_history <- function(put_strike, call_strike, n_prices, resolution, expiry){

  if(!hasArg(call_strike)){

    call_strike <- put_strike

  }

  put <- get_option_price_history(strike = put_strike,
                                  option_type = 'P',
                                  n_prices =  n_prices,
                                  resolution = resolution,
                                  expiry = expiry
                                  )

  call <- get_option_price_history(strike = call_strike,
                                   option_type = 'C',
                                   n_prices =  n_prices,
                                   resolution = resolution,
                                   expiry = expiry
                                   )

  # call <- map(call, unlist) %>%


  price_columns <- c('close', 'open', 'high', 'low')


  put <- put %>%
    map_df(~ modify_if(., is.null, ~ 0 )) %>%
    modify_if(is.list, unlist)

  call <- call %>%
    map_df(~ modify_if(., is.null, ~ 0 )) %>%
    modify_if(is.list, unlist)

  bind_cols(dateTime = put$dateTime,
            put[,price_columns]+call[,price_columns])

}

