# nextcloud_request ------------------------------------------------------------

#' @importFrom httr GET VERB http_error content
#' @importFrom xml2 xml_text xml_find_all
#' @keywords internal
nextcloud_request <- function(
  href, verb = "GET", auth = nextcloud_auth(), body = NULL, as = "response",
  really = FALSE, headers = list()
)
{
  # Combine authentication and headers (if any)
  config <- c(auth, if (length(headers)) do.call(httr::add_headers, headers))

  verb <- match.arg(verb, c("GET", "PROPFIND", "PUT", "DELETE"))

  as <- match.arg(as, c("response", "raw", "text", "parsed", "content"))

  url <- href_to_url(href)

  response <- if (verb == "GET") {

    httr::GET(url, config)

  } else if (verb == "PROPFIND") {

    httr::VERB(verb, url, config, body = body)

  } else if (verb == "PUT") {

    httr::PUT(url, config, body = body)

  } else if (verb == "DELETE") {

    if (really) {

      httr::DELETE(url, config)

    } else {

      message("I will not really delete ", url, "!")
      return()
    }
  }

  if (httr::http_error(response)) {

    xml <- httr::content(response)

    find_text <- function(x) xml2::xml_text(xml2::xml_find_all(xml, x))

    elements <- c("exception", "message")

    xpaths <- stats::setNames(paste0("/d:error/s:", elements), elements)

    values <- lapply(xpaths, find_text)

    stop(call. = FALSE, sprintf(
      "\nException: %s\nMessage: %s", values$exception, values$message
    ))
  }

  if (as == "response") {

    response

  } else {

    httr::content(response, as = as)
  }
}
