#' Load option strategy
#'
#' @param path File name
#'
#' @return Strategy object
#' @export
#'
#' @importFrom purrr modify_in
#' @importFrom jsonlite read_json
#' @importFrom purrr modify
#' @importFrom lubridate as_datetime
#'
#' @examples
load_strategy <- function(path) {
  strat_file <- jsonlite::read_json(path)

  class(strat_file) <- "option_strategy"

  strat_file <- strat_file %>% purrr::modify_in(.where = list("underlyer_prices"), dplyr::bind_rows)

  strat_file <- strat_file %>% purrr::modify_in(.where = list("underlyer_prices", "date_time"), lubridate::as_datetime)

  strat_file$legs <- purrr::modify(
    strat_file$legs,
    ~ purrr::modify_in(.x, "prices",dplyr::bind_rows)
  )

  strat_file$legs <- purrr::modify(
    strat_file$legs,
    ~ purrr::modify_in(.x, "expiry", lubridate::as_datetime)
  )

  strat_file$legs <- purrr::modify(
    strat_file$legs,
    ~ purrr::modify_in(.x, list("prices", "date_time"), lubridate::as_datetime)
  )

  strat_file$legs <- purrr::modify(strat_file$legs, ~ `class<-`(., "option_leg"))

  strat_file <- `attr<-`(strat_file, "n_legs", length(strat_file$legs))
}

#' Save option strategy
#'
#' @param strategy Strategy object
#' @param path File name
#'
#' @return
#' @export
#'
#' @importFrom jsonlite write_json
#'
#' @examples
save_strategy <- function(strategy, path) {
  jsonlite::write_json(strategy,
    path,
    force = T,
    auto_unbox = T,
    POSIXt = "ISO8601"
  )
}
