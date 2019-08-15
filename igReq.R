
slowPriceReq <- slowExec(pause = 1, f = priceReq)

sp500 <- slowPriceReq("IX.D.SPTRD.DAILY.IP", "MINUTE", 1)$close

strikes <- seq(round(sp500, -1) - 250, round(sp500, -1) + 250, 10)

callOptionChainEpics <- paste0("OP.D.SPX1.", strikes, "C.IP")

chainPrices <- purrr::map(.x = callOptionChainEpics,
                          .f = slowPriceReq,
                          resolution = "MINUTE",
                          nPrices = 1)

cls <- vapply(X = chainPrices, FUN = `[[`, "close" , numeric(1))

validPrices <- (!sapply(cls, is.null) & cls>0)

hasValue <- vapply(cls, FUN = function(x) x>0, logical(1))

chainPrices <- bind_rows(chainPrices[validPrices])

strikes <- strikes[validPrices]

ttm <- int_length(interval(chainPrices$dateTime[1], ymd_hm("2019-02-14T21:15")))/31536000

pVol <- partial(.f = fOptions::GBSVolatility,
            TypeFlag = "c",
            S = sp500,
            Time = ttm,
            r=0.005,
            b=0)

vols <- pmap_dbl(.l = list(price = chainPrices$close, X = strikes), .f = pVol)

pGreek <- partial(.f = fOptions::GBSGreeks,
                  TypeFlag = "c",
                  S = sp500,
                  Time = ttm,
                  r = 0.005,
                  b = 0,
                  sigma = 0.1279)

timeValue <- chainPrices$close - pmax((sp500 - strikes),0)

vega <- pmap_dbl(.l = list(X = strikes, Selection = "vega"), .f = pGreek)
delta <- pmap_dbl(.l = list(X = strikes, Selection = "delta"), .f = pGreek)
gamma <- pmap_dbl(.l = list(X = strikes, Selection = "gamma"), .f = pGreek)
rho <- pmap_dbl(.l = list(X = strikes, Selection = "rho"), .f = pGreek)
theta <- pmap_dbl(.l = list(X = strikes, Selection = "theta"), .f = pGreek)



optDf <- data_frame(vol = vols,
                    strike = strikes[validPrices],
                    vega = vega,
                    delta = delta,
                    gamma = gamma,
                    rho = rho,
                    theta = theta,
                    timeValue = timeValue)

p <- ggplot(optDf, aes(x = strike, y = theta))+
  geom_point()+
  geom_vline(xintercept = sp500, colour = "red")

plotly::ggplotly(p)

sp500 <- priceReq(epic = 'IX.D.SPTRD.DAILY.IP', resolution = 'HOUR', nPrices = 300, session = session)

straddle <- get_straddle_price_history(2950, 300, resolution = 'HOUR')

vix <- priceReq(epic = 'IN.D.VIX.MONTH1.IP', resolution = 'HOUR', nPrices = 300, session = session)

library(magrittr)

lag <- dplyr::lag

sp500 %<>%
  mutate(returns = (close/lag(close))-1)

straddle %<>%
  mutate(returns = (close/lag(close))-1)

vix %<>%
  mutate(returns = (close/lag(close))-1)

combd <- sp500 %>%
  left_join(straddle, by = 'dateTime', suffix = c('_sp', '_str'))

straddle_2850 <- get_straddle_price_history(strike = 2850,
                                            n_prices = 300,
                                            resolution = 'HOUR',month = 1)

straddle_2850_2 <- get_straddle_price_history(strike = 2850,
                                              n_prices = 100,
                                              resolution = 'HOUR',month = 2)

library(bdscale)


timeseries_plot(implied_vol, y = implied_vol)

timeseries_plot(strangle, y=close)





ts <- list('strategy'= strangle, 'underlyer'=sp500)


straddle_plot <- function(n_prices, resolution, expiry, strike){

  call_opt <- OptionsAnalytics::get_option_price_history(strike = strike,
                                                          expiry = expiry,
                                                          n_prices = n_prices,
                                                          resolution = resolution)

  straddle <- OptionsAnalytics::get_straddle_price_history(put_strike = strike,
                                                           call_strike = strike,
                                                           n_prices = n_prices,
                                                           resolution = resolution,
                                                           expiry = expiry)



  sp500 <- OptionsAnalytics::request_prices(resolution = resolution,
                                      n_prices = n_prices)

  prices <- OptionsAnalytics::intersect_prices(straddle,
                                               sp500,
                                               call_opt)

  implied_vol <- OptionsAnalytics::iv_by_time(underlying_prices = prices[[2]],
                                              option_prices = prices[[3]],
                                              strike = strike)

  prices[[3]] <- implied_vol

  names(prices[[3]]) <- c('date_time', "close")

  names(prices) <- c("strategy", "underlying", "iv")

  plot_fn <- purrr::partial(plotly::plot_ly, x=~as.factor(date_time),
                            y=~close,
                            mode='lines',
                            type='scatter')

  plots <- purrr::imap(prices, ~plot_fn(data=.x, name=.y))

  plotly::subplot(plots,nrows = 3, shareX = T)

}






