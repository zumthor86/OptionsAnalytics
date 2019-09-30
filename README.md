
<!-- README.md is generated from README.Rmd. Please edit that file -->
OptionsAnalytics
================

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/zumthor86/OptionsAnalytics.svg?branch=master)](https://travis-ci.org/zumthor86/OptionsAnalytics) <!-- badges: end -->

The goal of OptionsAnalytics is to provide a high level interface for constructing and analysing options strategies using [IG Index](https://www.ig.com/uk/welcome-page) REST API

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("zumthor86/OptionsAnalytics")
```

.Renviron file
--------------

Before using this package you need to obtain API keys from IG index. Its recommended to request one for your live account and demo account as the API limits are quite stringent, so once you have used up your allowance in your live account you can switch to the demo API key. Store these environment variables in an `.Renviron` file as follows:

``` r
LIVE_IG_API_KEY="<your live API key>" 
LIVE_IG_USERNAME="<your live API username>" 
LIVE_IG_PASSWORD="<your live API password>" 
LIVE_IG_HOST="api.ig.com" 
DEMO_IG_API_KEY="<your demo API key>" 
DEMO_IG_USERNAME="<your demo username>" 
DEMO_IG_PASSWORD="<your demo password>" 
DEMO_IG_HOST="demo-api.ig.com"
```

Start IG session
----------------

To start an IG session:

``` r
library(OptionsAnalytics)

initiate_ig_session(env = "LIVE")
```

Understanding the IG Index API
------------------------------

All IG instruments are organized into a market hierarchy. For example, an Option on the SP500 has a child node for each option expiry and each of those expiries in turn could have a child node for a particular range of strike prices. The "leaf" nodes contain the actual tradable instruments.

IG uses an instrument ID known as an 'epic'. Unfortunately the epics dont always map neatly to option characteristics, and you wont know what the epic is even if you know exactly what option you are looking for. For this reason you can create a market environment that will make instrument lookup easier (and faster). Names of instruments in this environment have the form Expiry, Underlyer, Strike, Option type.

Create market enviroment
------------------------

``` r

# Commodity options node_id = 166251

comm_mkts <- create_market_env(166251, pause = 1)

head(names(comm_mkts))
#> [1] "NOV-19 US Crude 6650 PUT"                 
#> [2] "FEB-20 Gold Futures 1670 CALL"            
#> [3] "NOV-19 US Crude 6000 CALL"                
#> [4] "NOV-19 US Crude 7450 CALL"                
#> [5] "30-SEP-19 Daily Oil (Nov Future) 5740 PUT"
#> [6] "NOV-19 US Crude 4400 CALL"

comm_mkts$`DEC-19 Gold Futures 1515 CALL`
#> [1] "OP.D.GC2.1515C.IP"
```

Strategy Object
---------------

There are three ways to create a strategy object:

### Instantiate new strategy from scratch

Using the market environment created you can look up the options you'd like to add to your strategy, and then specify the positions for each leg, as well as the resolution of the plots and amount of price history. Note that when creating a strategy this way, the most recent prices will be used as the opening prices for the strategy.

``` r

gold_strat <- create_strategy(c(comm_mkts$`DEC-19 Gold Futures 1525 CALL`,
                                comm_mkts$`DEC-19 Gold Futures 1565 CALL`),
                              positions = c(1,-3),
                              resolution = "HOUR",
                              n_prices = 100)
```

### Load strategy from previous session

Strategies can be persisted as a json file and then loaded subsequently

``` r
tmp <- tempfile()

save_strategy(strategy = gold_strat, path = tmp)

gold_strat1 <- load_strategy(tmp)

identical(gold_strat, gold_strat1)
#> [1] TRUE

unlink(tmp)
```

### Using a live strategy

The `get_current_positions()` function will return a tibble of options legs. You can then pass the epics, positions and opening\_prices to `create_strategy`

Plot the strategy over time
---------------------------

`plot_strategy_prices` will provide an interactive plot of the strategy prices, the underlyer, as well as the greeks. Note that a negative price indicates that the strategy was constructed at a net credit, i.e. the value of premiums sold is greater than premiums bought.

``` r

prices <- plot_strategy_prices(gold_strat)
#> Joining, by = "date_time"
```

Plot strategy scenarios at a point in time
------------------------------------------

`plot_strategy_scenarios` allows you to evaluate the possible price of the strategy for different values of the underlyer and implied volatility at a chosen point in time.

``` r

scenarios <- plot_strategy_scenarios(gold_strat, 
                        scenario_datetime =  gold_strat$legs[[1]]$expiry-lubridate::days(20))
```

Plot strategy pnl
-----------------

Assuming that all the legs have the same expiration, one can plot the expiration PnL

``` r

pnl <- plot_strategy_pnl(gold_strat)
```
