#' Fill in missing prices for option legs
#'
#' @param legs List of option legs
#'
#' @return Option legs with new price data added
#'
#' @importFrom purrr::flatten
#'
#' @examples
fill_strategy_prices <- function(legs) {
  expiries <- unique(map(legs, "expiry"))

  split_strat <- purrr::map(
    expiries,
    ~ purrr::keep(
      legs,
      function(x) x$expiry == .
    )
  )

  purrr::flatten(purrr::map(split_strat, fill_prices))
}


#' Add additional data to option leg
#'
#' @param legs List of option legs with common expiry
#'
#' @return Option legs with new price data added based on union datetimes
#'
#'
#' @examples
fill_prices <- function(legs) {
  union_datetime <- purrr::map(legs, list("prices", "date_time")) %>%
    purrr::reduce(union) %>%
    lubridate::as_datetime()

  purrr::map(
    legs,
    ~ purrr::modify_in(
      ., "prices",
      ~ expand_prices(., union_datetime)
    )
  )
}

#' Fill missing leg prices
#'
#' @param leg_prices Dataframe of option leg prices
#' @param union_datetime Vector or datetimes to join on
#'
#' @importFrom tidyr fill
#'
#' @return Prices dataframe with missing values filled with prior prices
#'
#' @examples
expand_prices <- function(leg_prices, union_datetime) {
  dplyr::full_join(
    leg_prices,
    tibble(date_time = union_datetime)
  ) %>%
    dplyr::arrange(.data$date_time) %>%
    tidyr::fill(dplyr::everything(),
      .direction = "up"
    )
}


#' Refresh strategy prices to current datetime
#'
#' @param strategy Option strategy
#'
#' @return Option strategy with up to date prices
#' @export
#'
#' @examples
refresh_strategy <- function(strategy) {
  start_times <- purrr::map(strategy$legs, list("prices", "date_time")) %>%
    purrr::map(~ tail(., 1)) %>%
    append(list(tail(strategy$underlyer_prices$date_time, 1))) %>%
    purrr::map(~ format(., format = "%Y-%m-%dT%H:%M:%S"))

  end_time <- lubridate::floor_date(lubridate::with_tz(Sys.time(),
    tzone = "UTC"
  ),
  unit = "hour"
  ) %>%
    format(format = "%Y-%m-%dT%H:%M:%S")

  epics <- purrr::map_chr(strategy$legs, "epic") %>%
    append(strategy$legs[[1]]$underlyer)

  fresh_prices <- purrr::map2(epics, start_times, ~ request_prices_range(.x, .y, end_time))

  prices_iter <- iterators::iter(fresh_prices)

  strategy$legs <- purrr::map(
    strategy$legs,
    ~ purrr::modify_in(.,
      .where = "prices",
      .f = ~ unique(dplyr::bind_rows(.x, iterators::nextElem(prices_iter)))
    )
  )

  strategy <- purrr::modify_in(strategy,
    "underlyer_prices",
    .f = ~ unique(dplyr::bind_rows(.x, iterators::nextElem(prices_iter)))
  )

  strategy$legs <- fill_strategy_prices(strategy$legs)

  strategy
}
