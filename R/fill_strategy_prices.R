fill_strategy_prices <- function(legs) {

  expiries <- unique(map(legs, "expiry"))

  split_strat <- purrr::map(expiries,
                            ~purrr::keep(legs,
                                         function(x) x$expiry == .))

  purrr::flatten(map(split_strat, fill_prices))

}


fill_prices <- function(legs){

  union_datetime <- map(legs, list("prices", "date_time")) %>%
    purrr::reduce(union) %>%
    as_datetime()

  purrr::map(legs,
             ~ purrr::modify_in(., c("prices"),
                                ~ expand_prices(.,union_datetime)))



}

expand_prices <- function(leg_prices, union_datetime){

  dplyr::full_join(leg_prices,
                   tibble(date_time=union_datetime)) %>%
    dplyr::arrange(date_time) %>%
    tidyr::fill(dplyr::everything(),
                .direction = "up")


}




