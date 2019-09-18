new_option_leg <- function(strike_price = double(),
                           expiry = double(),
                           option_type = character(),
                           underlyer = character(),
                           position = double(),
                           prices = tibble::tibble(),
                           epic = character()) {


  structure(list(
    "strike_price" = strike_price,
    "expiry" = expiry,
    "option_type" = option_type,
    "underlyer" = underlyer,
    "position" = position,
    "prices" = prices,
    "epic" = epic
  ),
  class = "option_leg"
  )
}

validate_option_leg <- function(leg){

  assertthat::assert_that(
    purrr::every(
      list(
        leg$strike_price,
        leg$position
      ),
      is.double
    ),
    purrr::every(
      list(
        leg$option_type,
        leg$underlyer,
        leg$epic
      ),
      is.character
    ),
    is.data.frame(leg$prices),
    lubridate::is.POSIXct(leg$expiry)
  )

  leg

}


new_option_strategy <- function(legs = list(),
                                underlyer_prices = tibble::tibble()) {

  structure(list(
    "legs" = legs,
    "underlyer_prices" = underlyer_prices
  ),
  n_legs = length(legs),
  class = "option_strategy"
  )
}

validate_option_strategy <- function(strategy){

  assertthat::assert_that(
    is.list(strategy$legs),
    is.data.frame(strategy$underlyer_prices),
    purrr::every(strategy$legs, inherits, what = "option_leg")
  )

  strategy

}



print.option_leg <- function(option_leg) {
  cat(
    " Underlyer:", option_leg$underlyer, "\n",
    "Strike price:", option_leg$strike_price, "\n",
    "Option Type:", option_leg$option_type, "\n",
    "Expiry:", as.character(option_leg$expiry), "\n",
    "Position:", option_leg$position, "\n"
  )
}

print.option_strategy <- function(option_strategy) {
  purrr::map(option_strategy$legs, print)
}

select_legs <- function(env, ...) {
  legs <- rlang::enexprs(...)

  purrr::map(legs, eval, env)
}

option_leg <- function(epic, position, resolution, n_prices) {
  details <- get_option_details(epic)

  prices <- request_prices(epic, resolution, n_prices)

  new_option_leg(
    strike_price = details$strike_price,
    expiry = details$expiry_datetime,
    option_type = details$option_type,
    underlyer = details$underlyer,
    position = position,
    prices = prices,
    epic = epic
  ) %>%
    validate_option_leg()
}

create_strategy <- function(epics, positions, resolution, n_prices) {
  assertthat::assert_that(length(epics) == length(positions),
    msg = "Number of epics and positions should be equal"
  )

  legs <- purrr::map2(epics, positions, option_leg, resolution, n_prices)

  new_option_strategy(legs, request_prices(legs[[1]]$underlyer, resolution, n_prices)) %>%
    validate_option_strategy()
}
