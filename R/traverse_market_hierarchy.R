#' Traverse IG markets hierarchy
#'
#' @param node_id ID of the node from which to begin the traversal, see \href{https://labs.ig.com/rest-trading-api-reference/service-detail?id=544}{API documentation}
#' @param pause Due to api limits it may be necessary when traversing lots of nodes to introduce a pause to avoid exceeding said limits
#'
#' @return A dataframe containing markets details
#' @export
#'
#' @examples
traverse_market_hierarchy <- function(node_id, pause = 0) {
  slow_mkt_nav <- slowExec(pause, request_market_navigation)


  results <- slow_mkt_nav(node_id)

  ids <- results$nodes %>%
    purrr::map("id")

  if (!is.null(results$markets)) {
    return(dplyr::bind_rows(results$markets))
  } else {
    all_mkts <- purrr::map_df(ids, traverse_market_hierarchy, pause = pause)


    return(all_mkts)
  }
}
