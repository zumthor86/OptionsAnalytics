traverse_market_hierarchy <- function(node_id) {
  slow_mkt_nav <- slowExec(2, request_market_navigation)

  results <- slow_mkt_nav(node_id)

  ids <- results$nodes %>%
    map("id")

  if (!is.null(results$markets)) {

    return(bind_rows(results$markets))

  } else{

    all_mkts <- map_df(ids, traverse_market_hierarchy)

    return(all_mkts)

  }


}
