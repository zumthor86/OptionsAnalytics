#' Plot an existing positions PnL scenarios at a given time
#'
#' @param scenario_datetime
#' @param epics
#' @param positions
#' @param opening_prices
#'
#' @return 3D surface plot of PnL scenarios at given time
#'
#'
#' @examples
plots_strategy_scenarios <- function(strategy, scenario_datetime) {

  options_scenarios <- purrr::map(strategy$legs,
                                  ~ compute_option_scenarios(.,
                                                             scenario_datetime = scenario_datetime,
                                                             underlyer_prices = strategy$underlyer_prices$close,
                                                             ))

  opening_prices <- purrr::map(strategy$legs, "opening_price")

  positions <- purrr::map(strategy$legs, "position")

  pnl_scen <- purrr::pmap(
    list(options_scenarios, opening_prices, positions),
    ~ (..1 - ..2) * ..3
  ) %>% purrr::reduce(`+`)

  plot_scenario_surface(pnl_scen)
}




