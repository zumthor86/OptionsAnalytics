#' Compute options implied volatility over time
#'
#' @param option_leg Option leg
#' @param underlying_prices Close prices of the option's underlying
#'
#' @return Dataframe of implied_volatility
#' @export
#' @importFrom fOptions GBSVolatility
#' @importFrom purrr pmap_dbl
#' @examples
compute_implied_volatility <- function(
                                       option_leg,
                                       underlying_prices) {
  assertthat::assert_that(length(option_leg$prices$close) == length(underlying_prices),
    msg = "Underlyer prices must have same length as option prices"
  )

  implied_vol <- purrr::partial(
    .f = fOptions::GBSVolatility,
    TypeFlag = tolower(option_leg$option_type),
    r = 0.005,
    b = 0,
    X = option_leg$strike_price
  )

  time_to_mat <- compute_ttm_years(option_leg$prices$date_time, option_leg$expiry)

  iv_by_time <- purrr::pmap_dbl(
    .f = implied_vol,
    .l = list(
      Time = time_to_mat,
      S = underlying_prices,
      price = option_leg$prices$close
    )
  )

  dplyr::bind_cols(date_time = option_leg$prices$date_time, implied_vol = iv_by_time)
}

#' Plot IV for each strategy leg
#'
#' @param strategy Strategy object
#'
#' @return
#' @export
#'
#' @examples
plot_implied_vols <- function(strategy) {
  iv <- purrr::map(strategy$legs, ~OptionsAnalytics::compute_implied_volatility(., underlying_prices = strategy$underlyer_prices$close))

  strikes <- purrr::map(strategy$legs, "strike_price")

  option_type <- purrr::map(strategy$legs, "option_type")

  leg_names <- paste0(strikes, " : ", option_type)

  cmbd <- OptionsAnalytics:::intersect_prices(append(iv, tibble::tibble(strategy$underlyer_prices)))

  legs <- head(cmbd, -1)

  names(legs) <- leg_names

  iv_plot <- plotly::plot_ly(legs[[1]],
                             x=~date_time,
                             y=~implied_vol,
                             type = "scatter",
                             mode = "line",
                             name = names(legs[1]))

  for (leg in list(tail(legs,-1))){


    iv_plot <- plotly::add_lines(iv_plot,
                                 y=~leg[[1]]$implied_vol,
                                 name = names(leg))%>%
      style_plot()

  }



  underlyer_plot <- plotly::plot_ly(tail(cmbd, 1)[[1]],
                                    x= ~date_time,
                                    y = ~close,
                                    type = "scatter",
                                    mode = "line") %>%
    style_plot


  plotly::subplot(iv_plot, underlyer_plot, shareX = T, nrows = 2)%>%
    style_subplot(plot_title = "Implied Volatilities")
}


