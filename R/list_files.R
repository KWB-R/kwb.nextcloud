if (FALSE)
{
  info <- kwb.nextcloud:::list_files(
    path = "departments",
    pattern = "xlsx$",
    recursive = TRUE,
    max_depth = 2,
    full_info = TRUE
  )

  View(info)

  # Paths to files to be downloaded
  (paths <- file.path(kwb.utils::getAttribute(info, "root"), info$file))
  info$href_orig

  # Download files by href (important when downloading versions)
  downloaded_files <- kwb.nextcloud:::download_files(info$href_orig)

  # Download files by path
  downloaded_files_2 <- kwb.nextcloud:::download_files(paths = paths)

  kwb.utils::hsOpenWindowsExplorer(dirname(dirname(downloaded_files[1])))

  # Get information on available versions. There seems to be only an entry for
  # files that have more than one version
  version_info <- kwb.nextcloud:::get_version_info(info$fileid[! info$isdir])

  View(version_info)

  merge(info[, c("file", "fileid")], version_info, all.x = TRUE, by = "fileid")

  href <- version_info$href

  kwb.nextcloud:::download_files(href)
}

# list_files -------------------------------------------------------------------

#' List Files on the Nextcloud Server
#'
#' @param path path to the nextcloud folder to be listed
#' @param pattern an optional regular expression. Only file names which match
#'   the regular expression will be returned.
#' @param recursive if \code{TRUE} the contents of subfolders are listed as
#'   well, up to the given \code{max_depth}.
#' @param full_info if \code{TRUE} the full file information is returned as a
#'   data frame. If \code{FALSE} (the default) only the (relative) file paths
#'   are returned as a vector of character.
#' @param user user name, by default taken from the environment variable
#'   "NEXTCLOUD_USER".
#' @param auth authentication header as provided by
#'   \code{kwb.nextcloud:::nextcloud_auth}
#' @param max_depth maximum recursion depth if \code{recursive = TRUE}. By
#'   default \code{max_depth} is \code{NA} meaning that the function behaves
#'   "fully recursive".
#' @param \dots further arguments passed to
#'   \code{\link[kwb.utils]{listToDepth}}.
#' @export
#' @return vector of character or data frame, each with attribute "root" being
#'   set to the value of \code{path}.
#'
list_files <- function(
  path = "",
  pattern = NULL,
  recursive = FALSE,
  full_info = FALSE,
  user = nextcloud_user(),
  auth = nextcloud_auth(),
  max_depth = NA,
  ...
)
{
  #kwb.utils::assignPackageObjects("kwb.nextcloud")
  #kwb.utils:::assignArgumentDefaults(list_files)

  file_info <- kwb.utils::listToDepth(
    path,
    max_depth = ifelse(recursive, max_depth, 0L),
    full_info = full_info,
    FUN = list_cloud_files,
    pattern = pattern,
    user = user,
    auth = auth
    , ...
  )

  result <- if (full_info) {

    kwb.utils::moveColumnsToFront(file_info, c("file", "isdir"))

  } else {

    kwb.utils::selectColumns(file_info, "file")
  }

  structure(result, root = path)
}

# list_cloud_files -------------------------------------------------------------
list_cloud_files <- function(
  path = character(),
  full_info = FALSE,
  pattern = NULL,
  user = nextcloud_user(),
  auth = nextcloud_auth(),
  method = 1L
)
{
  #kwb.utils::assignPackageObjects("kwb.nextcloud")
  #path = "proposals/bmbf_digital/Previous-projects/Budget"
  #user = nextcloud_user();password = nextcloud_password();method=1L

  if (length(path) == 0L) {

    # Return an empty result data frame as a template
    return(list_cloud_files(path = "", full_info)[FALSE, ])
  }

  path <- remove_leading_slashes(path)

  stopifnot(method %in% 1:2)

  message("Listing files in ", path)

  content <- nextcloud_request(
    href = path_to_file_href(path, user),
    verb = "PROPFIND",
    auth = auth,
    body = request_body_list_files(),
    as = "parsed"
  )

  to_numeric <- function(xx) as.numeric(kwb.utils::defaultIfNULL(xx, "0"))

  if (method == 1L) {

    result <- parse_xml_content_1(content)

    pull <- function(x) kwb.utils::selectColumns(result, x)

    result$href_orig <- pull("href")

    start <- min(nchar(pull("href"))) + 1L

    result$href <- substr(x = pull("href"), start, stop = nchar(pull("href")))

    result$getlastmodified <- to_posix(x = pull("getlastmodified"))
    result$getetag <- gsub('"', "", pull("getetag"))
    result$fileid <- to_numeric(pull("fileid"))
    result$size <- to_numeric(pull("size"))
    result$has.preview <- pull("has.preview") != "false"
    result$favorite <- to_numeric(pull("favorite"))
    result$comments.unread <- to_numeric(pull("comments.unread"))

    # Provide columns as required by kwb.utils::listToDepth()
    result$isdir <- pull("resourcetype") == "list()"
    result$file <- pull("href")

  } else if (method == 2L) {

    result <- parse_xml_content_2(content)
  }

  # Exclude the requested folder itself
  columns <- if (full_info) names(result) else c("file", "isdir")

  pull <- function(x) kwb.utils::selectColumns(result, x)

  keep <- nzchar(pull("file"))

  if (! is.null(pattern)) {
    keep <- keep & (pull("isdir") | grepl(pattern, pull("file")))
  }

  structure(result[keep, columns], root = path)
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

# parse_xml_content_1 ----------------------------------------------------------
parse_xml_content_1 <- function(content)
{
  x_all <- xml2::as_list(content)

  responses <- unname(x_all$multistatus)

  kwb.utils::safeRowBindAll(lapply(responses, parse_response))
}

# parse_response ---------------------------------------------------------------
parse_response <- function(response)
{
  #response <- responses[[2L]]
  elements <- names(response)

  stopifnot(all(elements %in% c("href", "propstat")))
  stopifnot(sum(elements == "href") == 1L)

  propstats <- lapply(response[elements == "propstat"], parse_propstat)

  for (i in seq_along(propstats)) {
    names_i <- names(propstats[[i]])
    is_status <- names_i == "status"
    names_i[is_status] <- paste0(names_i[is_status], ".", i)
    names(propstats[[i]]) <- names_i
  }

  cbind(
    kwb.utils::noFactorDataFrame(href = parse_href(href = response$href)),
    do.call(cbind, c(unname(propstats), list(stringsAsFactors = FALSE))),
    stringsAsFactors = FALSE
  )
}

# parse_href -------------------------------------------------------------------
parse_href <- function(href)
{
  stopifnot(is.list(href), length(href) == 1L)
  result <- href[[1L]]
  stopifnot(length(result) == 1L)
  result
}

# parse_propstat ---------------------------------------------------------------
parse_propstat <- function(propstat)
{
  #propstat <- propstats[[3]]

  stopifnot(identical(sort(names(propstat)), c("prop", "status")))

  cbind(
    parse_prop(prop = propstat$prop),
    status = parse_status(status = propstat$status),
    stringsAsFactors = FALSE
  )
}

# parse_status -----------------------------------------------------------------
parse_status <- function(status)
{
  stopifnot(is.list(status), length(status) == 1L)
  result <- status[[1]]
  stopifnot(is.character(result), length(result) == 1L)
  result
}

# parse_prop -------------------------------------------------------------------
parse_prop <- function(prop)
{
  stopifnot(is.list(prop))
  stopifnot(all(names(prop) %in% c(
    "comments-unread",
    "favorite",
    "fileid",
    "getcontentlength",
    "getcontenttype",
    "getetag",
    "getlastmodified",
    "has-preview",
    "owner-display-name",
    "permissions",
    "resourcetype",
    "share-types",
    "size",
    "quota-used-bytes",
    "quota-available-bytes"
  )))

  do.call(kwb.utils::noFactorDataFrame, lapply(prop, function(x) {
    if (length(x) == 0L) "" else as.character(x)
  }))
}

# parse_xml_content_2 ----------------------------------------------------------
parse_xml_content_2 <- function(content)
{
  extract_text <- function(xpath) {
    text <- xml2::xml_text(xml2::xml_find_all(content, paste0("//", xpath)))
  }

  result <- lapply(xpaths(), extract_text)

  lengths(result)

  result <- do.call(data.frame, result)

  stats::setNames(result, xpaths_to_names(xpaths()))
}

# xpaths -----------------------------------------------------------------------
xpaths <- function()
{
  c(
    "d:href",
    "d:getlastmodified",
    "d:getetag",
    "d:getcontenttype",
    "d:resourcetype",
    "oc:fileid",
    "oc:permissions",
    "oc:size",
    "nc:has-preview",
    "oc:favorite",
    "oc:comments-unread",
    "oc:owner-display-name",
    "oc:share-types"
  )
}

# xpaths_to_names --------------------------------------------------------------
xpaths_to_names <- function(xpaths)
{
  sapply(strsplit(xpaths, ":"), "[", 2L)
}

# get_nextcloud_urls -----------------------------------------------------------
get_nextcloud_urls <- function(user = nextcloud_user(), ...)
{
  # Example "versions" URL (https://docs.nextcloud.com/server/17/
  # developer_manual/client_apis/WebDAV/versions.html)
  # https://cloud.example.com/remote.php/dav/versions/USER/versions/FILEID

  dictionary <- list(
    base = nextcloud_url(),
    dav = "remote.php/dav",
    user_files = "<dav>/files/<user>",
    user_versions = "<dav>/versions/<user>",
    url_files = "<base>/<user_files>/<path>",
    url_versions = "<base>/<user_versions>/versions/<fileid>"
  )

  kwb.utils::resolve(dictionary, user = user, ...)
}

# nextcloud_auth ---------------------------------------------------------------
nextcloud_auth <- function(
  user = nextcloud_user(), password = nextcloud_password()
)
{
  httr::authenticate(user, password)
}
