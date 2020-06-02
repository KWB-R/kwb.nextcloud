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

# indent -----------------------------------------------------------------------
indent <- function(x, depth = 0L)
{
  if (depth > 0L) {
    paste0(kwb.utils::space(depth), x)
  } else {
    x
  }
}

# key_value --------------------------------------------------------------------
key_value <- function(key, value, .list = NULL)
{
  if (! is.null(.list)) {

    keys <- names(.list)
    values <- unlist(.list)

    return(do.call(paste, lapply(seq_along(.list), function(i) {
      key_value(keys[i], values[i])
    })))
  }

  paste0(key, "=\"", value, "\"")
}

# remove_leading_slashes -------------------------------------------------------
remove_leading_slashes <- function(x)
{
  gsub("^/+", "", x)
}

# unique_dirnames --------------------------------------------------------------
unique_dirnames <- function(x)
{
  setdiff(unique(dirname(x)), ".")
}
