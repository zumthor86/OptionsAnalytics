#' Create matrix of greeks for multiple options
#'
#' @param epics List of epics
#'
#' @return
#' @export
#'
#' @examples
create_greeks_matrix <- function(epics){

  greeks_matrix <- epics %>%
    map(get_greeks) %>%
    map(unlist) %>%
    bind_cols() %>%
    as.matrix()

  rownames(greeks_matrix) <- c("delta", "gamma", "vega", "rho", "theta")

  greeks_matrix

}

