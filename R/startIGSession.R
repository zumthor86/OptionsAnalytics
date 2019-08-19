#' Initiate IG Session
#'
#' @return
#' @export
#'
#' @examples
initiate_ig_session <- function() {
  body <- list(
    identifier = Sys.getenv("IG_USERNAME"),
    password = Sys.getenv("IG_PASSWORD")
  )

  .session <<- httr::POST(
    url = "https://api.ig.com/gateway/deal/session",
    body = body,
    config = httr::add_headers(
      `X-IG-API-KEY` = Sys.getenv("IG_API_KEY"),
      `VERSION` = 2
    ),
    encode = "json"
  )
}
