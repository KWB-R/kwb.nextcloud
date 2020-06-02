# create_directories -----------------------------------------------------------
create_directories <- function(paths)
{
  unlist(lapply(unique(paths), kwb.utils::createDirectory))
}

# create_download_dir ----------------------------------------------------------
create_download_dir <- function(pattern)
{
  kwb.utils::createDirectory(file.path(
    "~/../Downloads", basename(tempfile(pattern = pattern))
  ))
}

# decode_url -------------------------------------------------------------------
decode_url <- function(x, to = "latin1")
{
  iconv(utils::URLdecode(x), from = "UTF-8", to = to)
}

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

# unique_dirnames --------------------------------------------------------------
unique_dirnames <- function(x)
{
  setdiff(unique(dirname(x)), ".")
}
