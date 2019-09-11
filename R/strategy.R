new_option_leg <- function(strike_price=numeric(),
                           expiry = character(),
                           option_type = character(),
                           underlyer = character(),
                           size = double(),
                           position = double(),
                           prices = tibble::tibble(),
                           epic = character()
){


  structure(list("strike_price" = strike_price,
                 "expiry" = expiry,
                 "option_type" = option_type,
                 "underlyer" = underlyer,
                 "position" = position,
                 "prices" = prices,
                 "epic" = epic),
            class = "option_leg")

}

new_option_strategy <- function(legs=list(),
                                underlyer_prices = tibble::tibble()){

  structure(list("legs" = legs,
                 "underlyer_prices" = underlyer_prices),
            n_legs = length(legs),
            class = "option_strategy")

}



print.option_leg <- function(option_leg){

  cat(" Underlyer:", option_leg$underlyer, "\n",
      "Strike price:", option_leg$strike_price, "\n",
      "Option Type:", option_leg$option_type, "\n",
      "Expiry:", as.character(option_leg$expiry), "\n",
      "Position:", option_leg$position, "\n")


}

print.option_strategy <- function(option_strategy){

  purrr::map(option_strategy, print)

}

select_legs <- function(env, ...){

  legs <- rlang::enexprs(...)

  purrr::map(legs, eval, env)

}

option_leg <- function(epic, position, resolution, n_prices){

  details <- get_option_details(epic)

  underlyer <- get_option_underlyer(epic)[[1]]

  prices <- request_prices(epic, resolution, n_prices)

  new_option_leg(strike_price = details$strike_price,
                 expiry = details$expiry_datetime,
                 option_type = details$option_type,
                 underlyer = underlyer,
                 position = position,
                 prices = prices,
                 epic = epic
  )

}

create_strategy <- function(epics, positions, resolution, n_prices){

  legs <- purrr::map2(epics, positions, option_leg, resolution, n_prices)

  underlyer_epic <- legs[[1]]$underlyer

  new_option_strategy(legs, request_prices(underlyer_epic, resolution, n_prices))

}
