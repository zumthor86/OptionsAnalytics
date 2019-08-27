#' Extract latest greeks
#'
#' @param greeks_df Dataframe of greeks over time
#'
#' @return Matrix of greeks
#' @export
#'
#' @examples
extract_current_greeks <- function(greeks_df){

  tail(greeks_df[,c("delta", "gamma", "vega", "theta")],1) %>%
    t()

}
