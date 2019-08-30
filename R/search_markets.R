#' Search IG Markets
#'
#' @param search_term
#'
#' @return Dataframe consisting of search results
#' @export
#'
#' @examples
search_markets <- function(search_term) {
  query <- list(searchTerm = search_term)

  response <- make_ig_request(api_version = 1, query = query, path = "markets") %>%
    httr::content()

  dplyr::bind_rows(response$markets)
}
