#' Find option underlyer
#'
#' @param epic Option epic
#'
#' @return Option underlyer
#' @export
#'
#' @examples
get_option_underlyer <- function(epic) {
  underlyer <- stringr::str_extract(epic, "(?<=\\.D\\.).*(?=\\.\\d)")

  underlyers <- c(
    "GOLD" = "CS.D.USCGC.TODAY.IP",
    "GC" = "CS.D.USCGC.TODAY.IP",
    "SPX1" = "IX.D.SPTRD.DAILY.IP",
    "SPX2" = "IX.D.SPTRD.DAILY.IP",
    "SPX3" = "IX.D.SPTRD.DAILY.IP",
    "SPX4" = "IX.D.SPTRD.DAILY.IP",
    "SPX5" = "IX.D.SPTRD.DAILY.IP",
    "DSPX" = "IX.D.SPTRD.DAILY.IP",
    "SPXWEEK" = "IX.D.SPTRD.DAILY.IP",
    "EURUSD" = "CS.D.EURUSD.TODAY.IP"
  )


  underlyers[underlyer]
}
