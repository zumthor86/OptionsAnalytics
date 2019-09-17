#' Find option underlyer
#'
#' @param epic Option epic
#'
#' @return Option underlyer
#' @export
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
