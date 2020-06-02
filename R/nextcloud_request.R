# nextcloud_request ------------------------------------------------------------
nextcloud_request <- function(
  href, verb = "GET", auth = nextcloud_auth(), body = NULL, as = "response"
)
{
  verb <- match.arg(verb, c("GET", "PROPFIND"))

  as <- match.arg(as, c("response", "raw", "text", "parsed", "content"))

  url <- href_to_url(href)

  response <- if (verb == "GET"){

    httr::GET(url, auth)

  } else if (verb == "PROPFIND") {

    httr::VERB(
      verb, url, auth, body = body
    )
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
