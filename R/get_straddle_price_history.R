get_straddle_price_history <- function(put_strike,
                                       call_strike,
                                       n_prices,
                                       resolution,
                                       expiry){

  if (!hasArg(call_strike)){

    call_strike <- put_strike

  }

  put <- get_option_price_history(strike = put_strike,
                                  option_type = "P",
                                  n_prices =  n_prices,
                                  resolution = resolution,
                                  expiry = expiry
                                  )

  call <- get_option_price_history(strike = call_strike,
                                   option_type = "C",
                                   n_prices =  n_prices,
                                   resolution = resolution,
                                   expiry = expiry
                                   )

 price_columns <- c("close", "open", "high", "low")


  bind_cols(date_time = put$date_time,
            put[, price_columns] + call[, price_columns])

}
