#' Plot strategy scenarios
#'
#' @param epics List of epics
#' @param scenario_datetime Datetime at which to evaluate scenarios
#'
#' @return
#' @export
#'
#' @examples
plot_strategy_scenario <- function(epics, scenario_datetime) {
  underlyer <- request_prices(epic = get_option_underlyer(epics[1]), resolution = "MINUTE", n_prices = 1)

  options_scenarios <- purrr::map(epics, ~ OptionsAnalytics::compute_option_scenarios(., underlyer$close, scenario_datetime))

  strategy_scenarios <- purrr::reduce(options_scenarios, `+`)

  OptionsAnalytics::plot_scenario_surface(strategy_scenarios)
}
