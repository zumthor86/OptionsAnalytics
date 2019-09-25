#' Plot option Profit and Loss Scenarios
#'
#' @param strategy Strategy object
#'
#' @return Plot of PnL
#' @export
#'
#' @examples
plot_strategy_pnl <- function(strategy, underlyer_margin = 40) {
  strikes <- purrr::map_int(strategy$legs, "strike_price")

  strike_min <- min(strikes)

  strike_max <- max(strikes)

  pnls <- purrr::map(strategy$legs,
                     ~compute_option_pnl(.,underlyer_prices = strategy$underlyer_prices$close,
                                                        underlyer_min = strike_min,
                                                        underlyer_max = strike_max,
                                                        underlyer_margin))

  opening_prices <- purrr::map(strategy$legs, "opening_price")

  positions <- purrr::map(strategy$legs, "position")

  pnl_scen <- purrr::pmap(
    list(pnls, opening_prices, positions),
    ~ (..1 - ..2) * ..3
  ) %>% purrr::reduce(`+`)

  plotly::plot_ly() %>% plotly::add_trace(x = rownames(pnl_scen),
                                          y = pnl_scen[,1],
                                          type = "scatter", mode = "lines+markers")
}

#' Compute option PnL Scenarios at expiry
#'
#' @param option_leg Option Leg object
#' @param underlyer_prices Vector of underlyer closing prices
#' @param underlyer_min Minimum underlyer price to compute profit on
#' @param underlyer_max Maximum underlyer price to compute profit on
#' @param underlyer_margin Additional padding at ends of pnl chart
#'
#' @return
#'
#' @examples
compute_option_pnl <- function(option_leg,
                                     underlyer_prices,
                                     underlyer_min,
                                     underlyer_max,
                                     underlyer_margin = 20
                                     ) {



  partial_options <- purrr::partial(fOptions::GBSOption,
                                    TypeFlag = tolower(option_leg$option_type),
                                    X = option_leg$strike_price,
                                    Time = 0,
                                    r = 0.05,
                                    b = 0.05,
                                    sigma = 0.1
  )

  underlyer_space <- seq(underlyer_min-underlyer_margin, underlyer_max+underlyer_margin, 5)

  option_scenarios <- purrr::map_dbl(underlyer_space, ~ partial_options(S = .) %>% slot("price"))

  option_scenarios[is.nan(option_scenarios)] <- 0

  matrix(
    data = option_scenarios,
    nrow = length(underlyer_space),
    ncol = 1, dimnames = list(underlyer_space, "profit")
  )

}
