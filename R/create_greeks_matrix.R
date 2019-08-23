#' Create matrix of greeks for multiple options
#'
#' @param epics List of epics
#'
#' @return Matrix of dimension (n_greeks, n_options)
#' @export
#'
#' @examples
create_greeks_matrix <- function(epics) {
  epics %>%
    purrr::map(get_greeks) %>%
    dplyr::bind_rows() %>%
    t()
}
