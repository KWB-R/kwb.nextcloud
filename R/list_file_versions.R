# list_file_versions -----------------------------------------------------------

#' Deprecated: List File Versions
#'
#' Pleas use \code{\link{get_file_versions}} instead.
#'
#' @param path relative path to folder of which file versions are to be listed
#' @param pattern optional. If specified, only files with names matching the
#'   pattern are considered.
#' @param \dots additional arguments passed to \code{\link{list_files}}, such as
#'   \code{recursive}, \code{max_depth}
#' @export
#'
list_file_versions <- function(path, pattern = NULL, ...)
{
  kwb.utils::warningDeprecated("list_file_versions", "get_file_versions")

  file_info <- list_files(path, pattern, full_info = TRUE, ...)
  get_file_versions(file_info)
}

# get_file_versions ------------------------------------------------------------

#' Get Information on Versions of Given Files
#'
#' @param file_info data frame as returned by \code{\link{list_files}} when
#'   being called with \code{full_info = TRUE}
#' @returns data frame
#' @importFrom kwb.utils moveColumnsToFront selectColumns
#' @export
get_file_versions <- function(file_info)
{
  # Remove information on directories
  file_info <- file_info[!kwb.utils::selectColumns(file_info, "is_dir"), ]

  # Get version information for the remaining files
  file_ids <- kwb.utils::selectColumns(file_info, "fileid")
  version_info <- get_version_info(file_ids)

  columns_file_info <- c("fileid", "file", "lastmodified", "etag")
  columns_version_info <- c("fileid", "version", "href")

  result <- merge(
    x = kwb.utils::selectColumns(file_info, columns_file_info),
    y = kwb.utils::selectColumns(version_info, columns_version_info),
    by = "fileid"
  )

  kwb.utils::moveColumnsToFront(
    result,
    columns = c("fileid", "file", "version")
  )
}
