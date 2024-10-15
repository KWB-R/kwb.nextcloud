# download_files ---------------------------------------------------------------

#' Download Files from Cloud
#'
#' @param hrefs href strings specifying the files to be downloaded. The href
#'   strings are returned by \code{\link{list_files}}. Alternatively, the paths
#'   to the files can be given in \code{paths}.
#' @param target_dir path to local target directory
#' @param paths Alternatively to givin the href strings, the paths to the files
#'   can be given in this argument.
#' @param user name of nextcloud user. Default: result of calling
#'   \code{kwb.nextcloud:::nextcloud_user}
#' @param auth authentication as returned by
#'   \code{kwb.nextcloud:::nextcloud_user}
#' @param dbg logical indicating whether or not to show debug messages
#' @importFrom kwb.utils createDirectories defaultIfNULL uniqueDirnames
#' @importFrom kwb.file remove_common_root
#' @export
#'
download_files <- function(
  hrefs = NULL,
  target_dir = create_download_dir("nextcloud_", dbg = dbg),
  paths = NULL,
  user = nextcloud_user(),
  auth = nextcloud_auth(),
  dbg = TRUE
)
{
  #kwb.utils::assignPackageObjects("kwb.nextcloud")
  if (is.null(hrefs) && is.null(paths)) {
    stop("One of hrefs or paths must be given!")
  }

  if (! is.null(hrefs) && ! is.null(paths)) {
    stop("hrefs and paths must not be given at the same time!")
  }

  hrefs <- kwb.utils::defaultIfNULL(hrefs, path_to_file_href(paths, user))
  paths <- kwb.utils::defaultIfNULL(paths, hrefs)

  paths_decoded <- unlist(lapply(paths, decode_url))

  if (length(paths_decoded) == 0L) {
    message("Nothing to download.")
    return()
  }

  # Keep only the necessary tree structure
  target_paths <- kwb.file::remove_common_root(paths_decoded, dbg = FALSE)

  # Create required target folders
  kwb.utils::createDirectories(
    file.path(target_dir, kwb.utils::uniqueDirnames(target_paths)),
    dbg = dbg
  )

  # Create the full paths to the target files
  target_files <- file.path(target_dir, target_paths)

  unlist(mapply(
    FUN = download_from_href,
    hrefs,
    target_files,
    MoreArgs = list(auth = auth, dbg = dbg),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  ))
}

# download_from_href -----------------------------------------------------------

#' @importFrom kwb.utils catAndRun
#' @keywords internal
download_from_href <- function(
    href,
    target_file,
    auth = nextcloud_auth(),
    dbg = TRUE
)
{
  # Expect the target directory to exist
  stopifnot(file.exists(dirname(target_file)))

  kwb.utils::catAndRun(paste("Downloading", href), dbg = dbg, {

    response <- nextcloud_request(href, "GET", auth)

    write_content_to_file(response, target_file)
  })
}

# write_content_to_file --------------------------------------------------------

#' @importFrom httr content headers
#' @importFrom kwb.utils isTryError
#' @keywords internal
write_content_to_file <- function(response, target_file)
{
  content <- httr::content(response, type = "application/octet-stream")

  result <- try(writeBin(content, target_file))

  if (kwb.utils::isTryError(result)) {

    stop(
      "Could not write the response data with writeBin(). ",
      "The content type is: ", httr::headers(response)[["content-type"]],
      call. = FALSE
    )
  }

  target_file
}
