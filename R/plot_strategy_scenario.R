#' Plot strategy scenarios
#'
#' @param epics List of epics
#' @param scenario_datetime Datetime at which to evaluate scenarios
#'
#' @return
#' @export
#'
#' @examples
plot_strategy_scenario <- function(strategy, scenario_datetime) {
  option_scenarios <- purrr::map(
    strategy$legs,
    compute_option_scenarios,
    scenario_datetime,
    strategy$underlyer_prices$close
  )

  options_positions <- purrr::map_dbl(strategy$legs, "position")

  options_scenarios <- purrr::map2(option_scenarios, options_positions, ~ .x * .y)

  strategy_scenarios <- purrr::reduce(options_scenarios, `+`)

  plot_scenario_surface(strategy_scenarios)
}
