#' Plot option scenarios
#'
#' @param scenario_matrix
#'
#' @return
#' @export
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
