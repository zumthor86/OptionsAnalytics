#' Create named list of market nodes
#'
#' @param node_id
#'
#' @return Named list where the node_ids constitute the values
#' @export
#'
#' @examples
create_nodes_list <- function(node_id, recursive = FALSE) {
  if (!recursive) {
    mkts <- request_market_navigation(node_id)

    return(purrr::set_names(map(mkts$nodes, "id"), map(mkts$nodes, "name")))
  } else {
    mkts <- traverse_market_hierarchy(node_id)

    return(purrr::set_names(mkts$epic, mkts$instrumentName))
  }
}
