new_option_leg <- function(strike_price = integer(),
                           expiry = double(),
                           option_type = character(),
                           underlyer = character(),
                           position = double(),
                           opening_price = double(),
                           prices = tibble::tibble(),
                           epic = character()) {
  structure(list(
    "strike_price" = strike_price,
    "expiry" = expiry,
    "option_type" = option_type,
    "underlyer" = underlyer,
    "position" = position,
    "opening_price" = opening_price,
    "prices" = prices,
    "epic" = epic
  ),
  class = "option_leg"
  )
}

validate_option_leg <- function(leg) {

  # if (is.na(leg$opening_price)) assertthat::assert_that(is.double(leg$opening_price))


  assertthat::assert_that(
    purrr::every(
      list(
        leg$opening_price,
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
    is.integer(leg$strike_price),
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

validate_option_strategy <- function(strategy) {
  assertthat::assert_that(
    is.list(strategy$legs),
    is.data.frame(strategy$underlyer_prices),
    purrr::every(strategy$legs, inherits, what = "option_leg")
  )

  strategy
}

print.option_strategy <- function(option_strategy) {
  purrr::map(
    option_strategy$legs,
    ~ unclass(.) %>% .[c(
      "strike_price",
      "expiry",
      "option_type",
      "position",
      "opening_price"
    )]
  ) %>%
    dplyr::bind_rows()
}

option_leg <- function(epic, position, opening_price, resolution, n_prices) {
  details <- get_option_details(epic)

  prices <- request_prices(epic, resolution, n_prices)

  if (is.na(opening_price)) opening_price <- tail(prices$close, 1)

  new_option_leg(
    strike_price = as.integer(details$strike_price),
    expiry = details$expiry_datetime,
    option_type = details$option_type,
    underlyer = details$underlyer,
    position = as.double(position),
    opening_price = opening_price,
    prices = prices,
    epic = epic
  ) %>%
    validate_option_leg()
}

#' Instantiate new strategy object
#'
#' @param epics Vector or epics for each leg of strategy
#' @param positions Integer vector specifiying positions in each leg
#' @param opening_prices Double vector of opening prices, only used when using a live strategy
#' @param resolution Plot resolution, eg. "HOUR", "MINUTE_5", "DAY"
#' @param n_prices Number of prices
#'
#' @return
#' @export
#'
#' @examples
create_strategy <- function(epics, positions, opening_prices = NULL, resolution, n_prices) {
  if (is.null(opening_prices)) opening_prices <- rep(NA_real_, length(epics))

  legs <- purrr::pmap(
    list(
      epics,
      positions,
      opening_prices
    ),
    ~ option_leg(..1, ..2, ..3, resolution, n_prices)
  )

  new_option_strategy(legs, request_prices(legs[[1]]$underlyer, resolution, n_prices)) %>%
    validate_option_strategy()
}
