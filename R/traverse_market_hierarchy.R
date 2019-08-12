traverse_market_hierarchy <- function(node_id, pause=1) {
  slow_mkt_nav <- slowExec(pause, request_market_navigation)

  results <- slow_mkt_nav(node_id)

  ids <- results$nodes %>%
    purrr::map("id")

  if (!is.null(results$markets)) {

    return(dplyr::bind_rows(results$markets))

  } else{

    all_mkts <- purrr::map_df(ids, traverse_market_hierarchy)

    return(all_mkts)

  }


}
