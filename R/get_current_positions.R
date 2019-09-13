#' Get current positions
#'
#' @return Summary dataframe of open positions
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom purrr modify_at
#' @importFrom purrr map_chr
#' @examples
get_current_positions <- function(){

  pos <- make_ig_request(path = "positions", api_version = 2) %>% httr::content()

  size <- pos$positions %>% purrr::map_dbl(list("position", "size"))

  opening_price <- pos$positions %>% purrr::map_dbl(list("position", "level"))

  direction <- pos$positions %>% purrr::map_chr(list("position", "direction"))

  epic <- pos$positions %>% purrr::map_chr(list("market", "epic"))

  expiry <- pos$positions %>% purrr::map_chr(list("market", "expiry"))

  tibble::tibble(epic, opening_price, size, direction, expiry) %>%
    purrr::modify_at("direction", ~ifelse(.=="BUY", 1, -1))

}

