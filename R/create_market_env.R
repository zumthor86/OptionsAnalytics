#' Create market environment
#'
#' @param node_id Integer ID of that particular market node
#' @param pause Seconds to pause between requests
#'
#' @return
#' @export
#'
#' @importFrom rlang as_environment
#'
#' @examples
create_market_env <- function(node_id, pause=0){

  instruments <- traverse_market_hierarchy(node_id, pause)

  instruments <- instruments %>%
    dplyr::select(.data$epic, .data$expiry, .data$instrumentName) %>%
    dplyr::mutate(instrumentName = paste(.data$expiry, .data$instrumentName, sep = " "))

  all_mkt_epics <- instruments$epic

  all_mkt_epics <- purrr::set_names(all_mkt_epics, instruments$instrumentName)

  rlang::as_environment(all_mkt_epics)

}
