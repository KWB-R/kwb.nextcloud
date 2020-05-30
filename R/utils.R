# remove_leading_slashes -------------------------------------------------------
remove_leading_slashes <- function(x)
{
  gsub("^/+", "", x)
}

# stop_on_httr_error -----------------------------------------------------------
stop_on_httr_error <- function(response)
{
  if (httr::http_error(response)) {

    xml <- httr::content(response)

    stop(
      xml2::xml_text(xml2::xml_find_all(xml, "/d:error/s:message")),
      call. = FALSE
    )
  }
}
