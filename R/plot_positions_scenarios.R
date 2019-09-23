#' Plot an existing positions PnL scenarios at a given time
#'
#' @param strategy Strategy object
#' @param scenario_datetime Time at which to evaluate scenarios
#'
#' @return 3D surface plot of PnL scenarios at given time
#'
#'
#' @examples
plot_strategy_scenarios <- function(strategy, scenario_datetime) {
  options_scenarios <- purrr::map(
    strategy$legs,
    ~ compute_option_scenarios(.,
      scenario_datetime = scenario_datetime,
      underlyer_prices = strategy$underlyer_prices$close,
    )
  )

  opening_prices <- purrr::map(strategy$legs, "opening_price")

  positions <- purrr::map(strategy$legs, "position")

  pnl_scen <- purrr::pmap(
    list(options_scenarios, opening_prices, positions),
    ~ (..1 - ..2) * ..3
  ) %>% purrr::reduce(`+`)

  plot_scenario_surface(pnl_scen)
}

#' Plot option scenarios
#'
#' @param scenario_matrix Matrix of underlyer/volatility permutations
#'
#' @return 3D surface plot of scenarios
#'
#' @examples
plot_scenario_surface <- function(scenario_matrix) {
  font_style <- list(
    "family" = "sans-serif",
    "size" = 16,
    "color" = "white"
  )

  legend_style <- list(
    "font" = list(
      "family" = "sans-serif",
      "size" = 12,
      "color" = "white"
    )
  )

  breakeven <- matrix(data = 0, nrow = nrow(scenario_matrix), ncol = ncol(scenario_matrix))

  plotly::plot_ly(showscale = FALSE) %>%
    plotly::add_surface(
      x = ~ colnames(scenario_matrix),
      y = ~ rownames(scenario_matrix),
      z = ~scenario_matrix, name = "Scenarios",
      hovertemplate = paste0("Volatility: %{x}<br>Underlyer: %{y}<br>Price: %{z}")
    ) %>%
    plotly::layout(
      scene = list(
        xaxis = list(color = "white", title = list(text = "Volatility Space", font = font_style)),
        yaxis = list(color = "white", title = list(text = "Underlyer Space", font = font_style)),
        zaxis = list(color = "white", title = list(text = "Price", font = font_style))
      ),
      legend = legend_style,
      plot_bgcolor = "#252525",
      paper_bgcolor = "#252525"
    ) %>%
    plotly::add_surface(
      x = ~ colnames(scenario_matrix),
      y = ~ rownames(scenario_matrix),
      z = ~breakeven,
      hoverinfo = "none",
      color = I("white"),
      opacity = 0.5
    )
}
