#' Plot an existing positions PnL scenarios at a given time
#'
#' @param scenario_datetime
#' @param epics
#' @param opening_prices
#' @param directions
#' @param sizes
#'
#' @return 3D surface plot of PnL scenarios at given time
#' @export
#'
#' @examples
plot_positions_scenarios <- function(epics, opening_prices, directions, sizes, scenario_datetime) {
  underlyer_price <- request_prices(get_option_underlyer(positions$epic[1]), resolution = "MINUTE", n_prices = 1)$close

  options_scenarios <- purrr::map(positions$epic, ~ compute_option_scenarios(., underlyer_price, scenario_datetime))

  pnl_scen <- purrr::pmap(
    list(options_scenarios, opening_prices, directions, sizes),
    ~ (..1 - ..2) * ..3 * ..4
  ) %>% purrr::reduce(`+`)

  plot_scenario_surface(pnl_scen)
}
