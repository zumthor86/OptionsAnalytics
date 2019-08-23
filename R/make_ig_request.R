#' Make generic IG API request
#'
#' @param path API endpoint
#' @param api_version Certain endpoints are available for certain versions
#' @param query Optional query parameters
#'
#' @return
#' @export
#'
#' @examples
make_ig_request <- function(path, api_version, query = NULL) {
  request_url <- httr::modify_url(
    url = glue::glue("https://{Sys.getenv('IG_HOST')}"),
    path = file.path("gateway", "deal", path),
    query = query
  )

  response <- httr::GET(
    url = request_url,
    config = httr::add_headers(
      VERSION = api_version,
      `X-IG-API-KEY` = Sys.getenv("IG_API_KEY"),
      CST = .session$headers$cst,
      `X-SECURITY-TOKEN` = .session$headers$`x-security-token`
    )
  )

  assertthat::assert_that(response$status_code == 200,
    msg = glue::glue("Response code: {httr::content(response)[[1]]}")
  )

  response
}
