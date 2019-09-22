#' Retrieve option details
#'
#' @param epic Option epic
#'
#' @return List of strike_price, option_type, expiry_datetime, underlyer
#' @export
#'
#' @examples
get_option_details <- function(epic) {
  response <- make_ig_request(
    path = file.path(
      "markets",
      epic
    ),
    api_version = 3
  ) %>%
    httr::content()

  instrument_name <- response$instrument$name

  underlyer_id <- response$instrument$market_id

  strike <- as.numeric(stringr::str_extract(instrument_name, "\\d{4,}"))

  type <- stringr::str_extract(instrument_name, "C(?=ALL)|P(?=UT)")

  expiry <- lubridate::ymd_hm(response$instrument$expiryDetails$lastDealingDate)

  underlyer <- get_option_underlyer(epic)[[1]]

  list(
    strike_price = strike,
    option_type = type,
    expiry_datetime = expiry,
    underlyer = underlyer
  )
}

#' Find option underlyer
#'
#' @param epic Option epic
#'
#' @return Option underlyer
#'
#' @importFrom stringr str_extract
#' @examples
get_option_underlyer <- function(epic) {
  underlyer <- stringr::str_extract(epic, "SI|GC|OIL|SP|EURO|BR|EGBP|EJPY|GBPY|GBP|AUD|CAD|JPY+")

  underlyers <- c("SP" = "IX.D.SPTRD.DAILY.IP",
                  "BR" = "CS.D.GBPUSD.TODAY.IP",
                  "AUD" = "CS.D.AUDUSD.TODAY.IP",
                  "EGBP" = "CS.D.EURGBP.TODAY.IP",
                  "EJPY" = "CS.D.EURJPY.TODAY.IP",
                  "EURO" = "CS.D.EURUSD.TODAY.IP",
                  "GBPY" = "CS.D.GBPJPY.TODAY.IP",
                  "GBP" = "CS.D.GBPUSD.TODAY.IP",
                  "CAD" = "CS.D.USDCAD.TODAY.IP",
                  "JPY" = "CS.D.USDJPY.TODAY.IP",
                  "OIL" = "CC.D.CL.USS.IP",
                  "GC" = "CS.D.USCGC.TODAY.IP",
                  "SI" = "CS.D.USCSI.TODAY.IP")

  underlyers[underlyer]
}
