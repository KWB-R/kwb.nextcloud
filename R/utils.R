# create_directories -----------------------------------------------------------

#' @importFrom kwb.utils createDirectory
#' @keywords internal
create_directories <- function(paths)
{
  unlist(lapply(unique(paths), kwb.utils::createDirectory))
}

# create_download_dir ----------------------------------------------------------

#' @importFrom kwb.utils createDirectory
#' @keywords internal
create_download_dir <- function(pattern)
{
  kwb.utils::createDirectory(file.path(
    "~/../Downloads", basename(tempfile(pattern = pattern))
  ))
}

# dav_path ---------------------------------------------------------------------
dav_path <- function()
{
  "/remote.php/dav"
}

# decode_url -------------------------------------------------------------------
decode_url <- function(x, to = "latin1")
{
  iconv(utils::URLdecode(x), from = "UTF-8", to = to)
}

# exclude_directories ----------------------------------------------------------

#' @importFrom kwb.utils selectColumns
#' @keywords internal
exclude_directories <- function(file_info)
{
  file_info[! kwb.utils::selectColumns(file_info, "isdir"), , drop = FALSE]
}

# fileid_to_version_href -------------------------------------------------------
fileid_to_version_href <- function(fileid = "", user = nextcloud_user())
{
  file.path(dav_path(), "versions", user, "versions", fileid)
}

# get_file_or_folder_info ------------------------------------------------------
get_file_or_folder_info <- function(
  path, user = nextcloud_user(), auth = nextcloud_auth()
)
{
  stopifnot(is.character(path), length(path) == 1L)

  list_files(
    path, parent_only = TRUE, full_info = TRUE, user = user, auth = auth
  )
}

# href_to_url ------------------------------------------------------------------
href_to_url <- function(href)
{
  file.path(nextcloud_url(), href)
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

# is_cloud_directory -----------------------------------------------------------
is_cloud_directory <- function(
  path, user = nextcloud_user(), auth = nextcloud_auth()
)
{
  prop_info <- list_cloud_files(
    path,
    full_info = TRUE,
    user = user,
    auth = auth,
    parent_only = TRUE
  )

  stopifnot(nrow(prop_info) == 1L)

  kwb.utils::selectColumns(prop_info, "isdir")
}

# is_directory -----------------------------------------------------------------
is_directory <- function(file)
{
  file.info(kwb.utils::safePath(file))[, "isdir"]
}

# is_try_error -----------------------------------------------------------------
is_try_error <- function(x)
{
  inherits(x, "try-error")
}

# lookup_fileid ----------------------------------------------------------------
lookup_fileid <- function(
  path, user = nextcloud_user(), auth = nextcloud_auth()
)
{
  stopifnot(is.character(path))

  if (length(path) > 1L) {
    return(lapply(path, lookup_fileid))
  }

  info <- get_file_or_folder_info(path, user = user, auth = auth)

  kwb.utils::selectColumns(info, "fileid")
}

# nextcloud_auth ---------------------------------------------------------------
nextcloud_auth <- function(
  user = nextcloud_user(), password = nextcloud_password()
)
{
  httr::authenticate(user, password)
}

# path_to_file_href ------------------------------------------------------------
path_to_file_href <- function(path = "", user = nextcloud_user())
{
  file.path(dav_path(), "files", user, path)
}

# remove_leading_slashes -------------------------------------------------------
remove_leading_slashes <- function(x)
{
  gsub("^/+", "", x)
}

# rename_properties ------------------------------------------------------------
rename_properties <- function(result)
{
  prop_names <- get_property_info()[, c("name","column")]

  kwb.utils::renameColumns(result, renamings = kwb.utils::toLookupList(
    keys = gsub("-", ".", prop_names$name),
    values = prop_names$column
  ))
}

# stopifnot_folder -------------------------------------------------------------
stopifnot_folder <- function(
  path, user = nextcloud_user(), auth = nextcloud_auth()
)
{
  if (! is_cloud_directory(path, user = user, auth = auth)) {

    stop(call. = FALSE, sprintf(
      "The given path (%s) does not represent a folder.", path
    ))
  }
}

# to_posix ---------------------------------------------------------------------
to_posix <- function(x)
{
  stopifnot(is.character(x), all(is.na(x) | grepl("GMT$", x)))

  locale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", locale))
  Sys.setlocale("LC_TIME", "C")

  as.POSIXct(x, format = "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT")
}

# unique_dirnames --------------------------------------------------------------
unique_dirnames <- function(x)
{
  setdiff(unique(dirname(x)), ".")
}
