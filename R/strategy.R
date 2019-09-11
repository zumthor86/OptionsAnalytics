new_option_leg <- function(strike_price=numeric(),
                           expiry = character(),
                           option_type = character(),
                           underlyer = character(),
                           size = double(),
                           position = double(),
                           epic = character()
){


  structure(list("strike_price" = strike_price,
                 "expiry" = expiry,
                 "option_type" = option_type,
                 "underlyer" = underlyer,
                 "position" = position,
                 "epic" = epic),
            class = "option_leg")

}

new_option_strategy <- function(legs=list()){

  structure(legs,
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


all_mkts <- all_mkts %>%
  dplyr::select(epic, expiry, instrumentName) %>%
  dplyr::mutate(instrumentName = paste(.data$expiry, .data$instrumentName, sep = " "))

all_mkt_epics <- all_mkts$epic

all_mkt_epics <- set_names(all_mkt_epics, all_mkts$instrumentName)

epic_env <- rlang::as_environment(all_mkt_epics)


select_legs <- function(env, ...){

  legs <- rlang::enexprs(...)

  purrr::map(legs, eval, env)

}

option_leg <- function(epic, position){

  details <- get_option_details(epic)

  underlyer <- get_option_underlyer(epic)[[1]]

  new_option_leg(strike_price = details$strike_price,
                 expiry = details$expiry_datetime,
                 option_type = details$option_type,
                 underlyer = underlyer,
                 position = position,
                 epic = epic
  )

}

create_strategy <- function(epics, positions){

  purrr::map2(epics, positions, option_leg) %>%
    new_option_strategy()

}
