get_option_price_history <- function(strike,
                                     option_type = "C",
                                     expiry,
                                     n_prices,
                                     resolution){

  epic <- get_option_epic(strike = strike,
                          option_type = option_type,
                          expiry = expiry)

  request_prices(epic = epic,
           n_prices = n_prices,
           resolution = resolution
           )
}
