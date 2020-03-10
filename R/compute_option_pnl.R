#' Plot option Profit and Loss Scenarios
#'
#' @param strategy Strategy object
#' @param plot_range Maximum pnl range, integer
#'
#' @return Plot of PnL
#' @export
#'
#' @importFrom pracma trapz
#'
#' @examples
plot_strategy_pnl <- function(strategy) {
  strikes <- purrr::map_int(strategy$legs, "strike_price")

  plot_range <- 100*max(abs(purrr::map_dbl(strategy$legs, "position")))

  pnls <- purrr::map(
    strategy$legs,
    ~ compute_option_pnl(., underlyer_prices = strategy$underlyer_prices$close)
  )

  opening_prices <- purrr::map(strategy$legs, "opening_price")

  positions <- purrr::map(strategy$legs, "position")

  pnl_scen <- purrr::pmap(
    list(pnls, opening_prices, positions),
    ~ (..1 - ..2) * ..3
  ) %>% purrr::reduce(`+`)

  pos <- which(pnl_scen > 0)

  if (min(pos) == 1 & max(pos) == length(pnl_scen)) {
    pos <- which(pnl_scen * -1 > 0)
  }

  start_pos <- min(pos)

  if (start_pos > 1) {
    inter_min <- stats::approx(
      row.names(pnl_scen)[(start_pos - 1):start_pos],
      pnl_scen[(start_pos - 1):start_pos]
    )

    min_brkeven <- round(inter_min$x[which.min(abs(inter_min$y))], 2)
  }

  end_pos <- max(pos)

  if (end_pos < length(pnl_scen)) {
    inter_max <- stats::approx(
      row.names(pnl_scen)[end_pos:(end_pos + 1)],
      pnl_scen[end_pos:(end_pos + 1)]
    )

    max_brkeven <- round(inter_max$x[which.min(abs(inter_max$y))], 2)
  }

  underlyer <- rownames(pnl_scen)

  max_profit <- max(pnl_scen)

  plot_title <- list(
    "text" = "Strategy Profit at expiration",
    "font" = list(
      "family" = "sans-serif",
      "size" = 16,
      "color" = "white"
    )
  )

  data_subset <- abs(pnl_scen) < plot_range

  base_plot <- plotly::plot_ly() %>%
    plotly::add_trace(
      x = underlyer[data_subset],
      y = pnl_scen[data_subset, 1],
      type = "scatter",
      mode = "lines+markers"
    ) %>%
    plotly::layout(
      showlegend = FALSE,
      plot_bgcolor = "#252525",
      paper_bgcolor = "#252525",
      yaxis = list("title" = "Profit/Loss", color = "white"),
      xaxis = list("title" = "Underlyer", color = "white"),
      title = plot_title
    )

  if (exists("min_brkeven")) {
    base_plot <- base_plot %>%
      plotly::add_segments(
        x = min_brkeven,
        xend = min_brkeven,
        y = 0,
        yend = plot_range,
        color = I("grey")
      )
  }

  if (exists("max_brkeven")) {
    base_plot <- base_plot %>%
      plotly::add_segments(
        x = max_brkeven,
        xend = max_brkeven,
        y = 0,
        yend = plot_range,
        color = I("grey")
      )
  }

  base_plot
}

#' Compute option PnL Scenarios at expiry
#'
#' @param option_leg Option Leg object
#' @param underlyer_prices Vector of underlyer closing prices
#'
#' @return
#'
#' @examples
compute_option_pnl <- function(option_leg,
                               underlyer_prices) {
  partial_options <- purrr::partial(fOptions::GBSOption,
    TypeFlag = tolower(option_leg$option_type),
    X = option_leg$strike_price,
    Time = 0,
    r = 0.05,
    b = 0.05,
    sigma = 0.1
  )

  last_price <- tail(underlyer_prices, 1)

  underlyer_space <- seq(signif(last_price / 1.1, 3), signif(last_price * 1.1, 3), 5)

  option_scenarios <- purrr::map_dbl(underlyer_space, ~ partial_options(S = .) %>% slot("price"))

  option_scenarios[is.nan(option_scenarios)] <- 0

  matrix(
    data = option_scenarios,
    nrow = length(underlyer_space),
    ncol = 1, dimnames = list(underlyer_space, "profit")
  )
}
