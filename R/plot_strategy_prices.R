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

  common_strat_greeks <- strategy_greeks %>% dplyr::semi_join(strategy_prices)

  greeks_plot <- plotly::plot_ly(common_strat_greeks, x = ~date_time) %>%
    plotly::add_trace(y = ~delta, name = "delta", color = I(greeks_colors[[1]]), mode = "lines+markers", type = "scatter") %>%
    plotly::add_trace(y = ~vega, name = "vega", color = I(greeks_colors[[2]]), mode = "lines+markers", type = "scatter") %>%
    plotly::add_trace(y = ~gamma, name = "gamma", color = I(greeks_colors[[3]]), mode = "lines+markers", type = "scatter") %>%
    plotly::add_trace(y = ~theta, name = "theta", color = I(greeks_colors[[4]]), mode = "lines+markers", type = "scatter") %>%
    style_plot(y_axis_title = "Exposures", x_axis_title = "DateTime")

  common_strat_prices <- strategy$underlyer_prices %>%
    dplyr::semi_join(strategy_prices, by = "date_time")

  underlyer_plot <- plotly::plot_ly(common_strat_prices, x = ~date_time) %>%
    plotly::add_trace(y = ~close, name = "underlyer", color = I("white"), mode = "lines+markers", type = "scatter") %>%
    style_plot(y_axis_title = "Price")

  strategy_plot <- plotly::plot_ly(strategy_prices) %>%
    plotly::add_trace(x = ~date_time, y = ~close, name = "strategy", mode = "lines+markers", type = "scatter") %>%
    style_plot(y_axis_title = "Price")

  plotly::subplot(strategy_plot, underlyer_plot, greeks_plot, shareX = T, nrows = 3) %>%
    style_subplot("Strategy Prices")
}
