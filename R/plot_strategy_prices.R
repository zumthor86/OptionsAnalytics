#' Generate plot of options exposures
#'
#' @param strategy Strategy object
#'
#' @return plotly object
#' @export
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom plotly plot_ly
#' @importFrom plotly add_lines
#' @importFrom plotly layout
#' @importFrom plotly subplot
#'
#' @examples
plot_strategy_prices <- function(strategy) {
  strategy_prices <- compute_strategy_prices(strategy)

  strategy_greeks <- compute_strategy_greeks(strategy)

  greeks_colors <- RColorBrewer::brewer.pal(n = 4, name = "YlOrRd")

  greeks_plot <- plotly::plot_ly(strategy_greeks, x = ~date_time) %>%
    plotly::add_lines(y = ~delta, name = "delta", color = I(greeks_colors[[1]])) %>%
    plotly::add_lines(y = ~vega, name = "vega", color = I(greeks_colors[[2]])) %>%
    plotly::add_lines(y = ~gamma, name = "gamma", color = I(greeks_colors[[3]])) %>%
    plotly::add_lines(y = ~theta, name = "theta", color = I(greeks_colors[[4]])) %>%
    plotly::layout(
      xaxis = list("type" = "category", "title" = "DateTime", color = "white"),
      yaxis = list("title" = "Exposures", color = "white")
    )

  underlyer_plot <- plotly::plot_ly(strategy$underlyer_prices, x = ~date_time) %>%
    plotly::add_lines(y = ~close, name = "underlyer", color = I("white")) %>%
    plotly::layout(
      yaxis = list("title" = "Price", color = "white"),
      xaxis = list("type" = "category")
    )

  strategy_plot <- plotly::plot_ly(strategy_prices) %>%
    plotly::add_lines(x = ~date_time, y = ~close, name = "strategy") %>%
    plotly::layout(
      yaxis = list("title" = "Price", color = "white"),
      xaxis = list("type" = "category")
    )

  legend_style <- list(
    "font" = list(
      "family" = "sans-serif",
      "size" = 12,
      "color" = "white"
    )
  )

  plot_title <- list(
    "text" = "Strategy Prices",
    "font" = list(
      "family" = "sans-serif",
      "size" = 16,
      "color" = "white"
    )
  )

  plotly::subplot(strategy_plot, underlyer_plot, greeks_plot, shareX = T, nrows = 3) %>%
    plotly::layout(
      plot_bgcolor = "#252525",
      paper_bgcolor = "#252525",
      xaxis = list("showticklabels" = F, showline = F),
      legend = legend_style, title = plot_title
    )
}
