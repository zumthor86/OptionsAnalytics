#' Get current positions
#'
#' @return Summary dataframe of open positions
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom purrr modify_at
#' @importFrom purrr map_chr
#' @examples
get_current_positions <- function() {
  pos <- make_ig_request(path = "positions", api_version = 2) %>% httr::content()

  size <- pos$positions %>% purrr::map_dbl(list("position", "size"))

  opening_prices <- pos$positions %>% purrr::map_dbl(list("position", "level"))

  direction <- pos$positions %>% purrr::map_chr(list("position", "direction")) %>%
    purrr::modify(~ dplyr::if_else(. == "BUY", 1, -1)) %>% as.numeric()

  positions <- size * direction

  epics <- pos$positions %>% purrr::map_chr(list("market", "epic"))

  tibble::tibble(epics, positions, opening_prices)
}
