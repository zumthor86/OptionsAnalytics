#' Request market node data
#'
#' @param node_id Numeric id for a particular node in the market hierarchy
#'
#' @return Node data, containing either IDs of children nodes or market information if its a leaf
#' @export
#'
#' @examples
#' /dontrun{
#'
#' request_market_navigation <- function(346003)
#'
#' }
request_market_navigation <- function(node_id){

  path <- glue::glue("marketnavigation/{node_id}")

  make_ig_request(path = path, api_version = 1) %>%
    httr::content()

}
