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
analyze_options_position <- function(epics, positions, resolution, n_prices){

  underlyer_epic <- OptionsAnalytics::get_option_underlyer(epics[[1]])

  epics["underlyer"] = underlyer_epic

  position_matrix <- matrix(positions,
                            nrow = length(positions))

  common_prices <- purrr::map(epics, ~ OptionsAnalytics::request_prices(.,resolution = resolution,
                                                                        n_prices = n_prices)) %>%
    OptionsAnalytics::intersect_prices()

  strategy_prices <- OptionsAnalytics::compute_strategy_prices(common_prices[1:length(positions)],position_matrix) %>%
    purrr::modify_at("date_time", as.factor)

  greeks2 <- OptionsAnalytics::compute_strategy_greeks_by_time(epics = epics[1:length(positions)],
                                                               underlyer_prices = common_prices$underlyer$close,
                                                               underlyer_datetimes = common_prices$underlyer$date_time,
                                                               positions = positions) %>%
    purrr::modify_at("date_time", as.factor)


  greeks_plot <- plotly::plot_ly(greeks2, x=~date_time) %>%
    plotly::add_lines(y=~delta, name="delta") %>%
    plotly::add_lines(y=~vega, name = "vega") %>%
    plotly::add_lines(y=~gamma, name="gamma") %>%
    plotly::add_lines(y=~theta, name = "theta")

  greeks_now <- OptionsAnalytics::extract_current_greeks(greeks2)

  greeks_bar_plot <- plotly::plot_ly(x = greeks_now[,1],
                             y = rownames(greeks_now),
                             orientation = "h",
                             type = "bar",
                             name = "Current greeks",
                             color = I("blue"))

  underlyer_plot <- plotly::plot_ly(common_prices$underlyer, x=~as.factor(date_time)) %>%
    plotly::add_lines(y=~close, name = "underlyer")

  strategy_plot <-  plotly::plot_ly(strategy_prices) %>%
    plotly::add_lines(x=~date_time, y=~close, name = "strategy")

  price_plots <- plotly::subplot(strategy_plot, greeks_plot, underlyer_plot, shareX = T, nrows = 3)

  plotly::subplot(price_plots, greeks_bar_plot, nrows = 1, widths = c(0.8, 0.2)) %>%
    plotly::layout(legend = list(orientation = "h"), xaxis=list("showticklabels" = F))


}
