#' Compute greeks for multiple options
#'
#' @param epics List of epics
#'
#' @return Matrix of dimension (n_greeks, n_options)
#' @export
#'
#' @examples
compute_strategy_greeks <- function(epics, position_matrix) {
  greeks <- epics %>%
    purrr::map(prepare_greeks_calculation) %>%
    purrr::map(~ get_greeks(
      time_to_mat = .$time_to_mat,
      strike_price = .$strike_price,
      underlyer_price = .$underlyer_price,
      option_type = .$option_type
    )) %>%
    dplyr::bind_rows() %>%
    t()

  as.data.frame(greeks %*% position_matrix)
}
