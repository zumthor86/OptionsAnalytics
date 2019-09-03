#' Generate plot of options exposures
#'
#' @param epics List of options epics
#' @param positions List of options positions, i.e. -1 for short one contract, +2 for long two contracts
#' @param resolution Plot resolution
#' @param n_prices Number of prices
#'
#' @return plotly object
#' @export
#'
#' @examples
plot_strategy_prices <- function(epics, positions, resolution, n_prices) {
  underlyer_epic <- OptionsAnalytics::get_option_underlyer(epics[[1]])

  epics["underlyer"] <- underlyer_epic

  position_matrix <- matrix(positions,
    nrow = length(positions)
  )

  common_prices <- purrr::map(epics, ~ OptionsAnalytics::request_prices(.,
    resolution = resolution,
    n_prices = n_prices
  )) %>%
    OptionsAnalytics::intersect_prices()

  strategy_prices <- OptionsAnalytics::compute_strategy_prices(common_prices[1:length(positions)], position_matrix)

  strategy_greeks <- OptionsAnalytics::compute_strategy_greeks_by_time(
    epics = epics[1:length(positions)],
    underlyer_prices = common_prices$underlyer$close,
    underlyer_datetimes = common_prices$underlyer$date_time,
    positions = positions
  )

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

  underlyer_plot <- plotly::plot_ly(common_prices$underlyer, x = ~date_time) %>%
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

  price_plots <- plotly::subplot(strategy_plot, underlyer_plot, greeks_plot, shareX = T, nrows = 3) %>%
    plotly::layout(
      plot_bgcolor = "#252525",
      paper_bgcolor = "#252525",
      xaxis = list("showticklabels" = F, showline = F),
      legend = legend_style, title = plot_title
    )
}
