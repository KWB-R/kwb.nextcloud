# nextcloud_request ------------------------------------------------------------

#' @importFrom httr GET VERB http_error content
#' @importFrom xml2 xml_text xml_find_all
#' @keywords internal
nextcloud_request <- function(
  href, verb = "GET", auth = nextcloud_auth(), body = NULL, as = "response"
)
{
  verb <- match.arg(verb, c("GET", "PROPFIND", "MKCOL"))

  as <- match.arg(as, c("response", "raw", "text", "parsed", "content"))

  url <- href_to_url(href)

  response <- if (verb == "GET"){

    httr::GET(url, config = auth)

  } else if (verb == "PROPFIND") {

    httr::VERB(verb, url, config = auth, body = body)

  } else if (verb == "MKCOL") {

    httr::VERB(verb, url, config = auth)
  }

  if (httr::http_error(response)) {

    xml <- httr::content(response)

    stop(
      xml2::xml_text(xml2::xml_find_all(xml, "/d:error/s:message")),
      call. = FALSE
    )
  }

  if (as == "response") {

    response

  } else {

    httr::content(response, as = as)
  }
}
