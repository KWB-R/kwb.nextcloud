# create_download_dir ----------------------------------------------------------

#' @importFrom kwb.utils createDirectory
#' @keywords internal
create_download_dir <- function(pattern, dbg = TRUE)
{
  kwb.utils::createDirectory(
    file.path("~/../Downloads", basename(tempfile(pattern = pattern))),
    dbg = dbg
  )
}

# webdav_base_url --------------------------------------------------------------
webdav_base_url <- function(leading_slash = TRUE)
{
  # The base url for all WebDAV operations for a Nextcloud instance is
  # /remote.php/dav.
  # https://docs.nextcloud.com/server/latest/developer_manual/client_apis/WebDAV/basic.html

  paste0(if (leading_slash) "/", "remote.php/dav")
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
  file.path(webdav_base_url(), "versions", user, "versions", fileid)
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

#' @importFrom kwb.utils removeLeadingSlashes
href_to_url <- function(href)
{
  file.path(nextcloud_url(), kwb.utils::removeLeadingSlashes(href))
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
    parent_only = TRUE,
    silent = TRUE
  )

  stopifnot(nrow(prop_info) == 1L)

  kwb.utils::selectColumns(prop_info, "isdir")
}

# is_directory -----------------------------------------------------------------
is_directory <- function(file)
{
  file.info(kwb.utils::safePath(file))[, "isdir"]
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

#' Convert Path to href as Required for Download
#'
#' @param path path to file on Nextcloud server
#' @param user name of Nextcloud user, default:
#'   \code{kwb.nextcloud:::nextcloud_user()}
#' @param leading_slash should the returned href start with a slash?
#' @export
path_to_file_href <- function(
  path = "", user = nextcloud_user(), leading_slash = FALSE
)
{
  base_url <- webdav_base_url(leading_slash = leading_slash)
  file.path(base_url, "files", utils::URLencode(user), path)
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
