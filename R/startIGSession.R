#' Initiate IG Session
#'
#' @param key Name of api key environment variable
#'
#' @return List of session information in global environment
#' @export
#'
#' @examples
initiate_ig_session <- function(key = "IG_API_KEY") {
  body <- list(
    identifier = Sys.getenv("IG_USERNAME"),
    password = Sys.getenv("IG_PASSWORD")
  )

  .session <<- httr::POST(
    url = "https://api.ig.com/gateway/deal/session",
    body = body,
    config = httr::add_headers(
      `X-IG-API-KEY` = Sys.getenv(key),
      `VERSION` = 2
    ),
    encode = "json"
  )
}
