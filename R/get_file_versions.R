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
  file_info <- file_info[!kwb.utils::selectColumns(file_info, "isdir"), ]

  if (nrow(file_info) == 0L) {
    message("There are no (non-directory) files within file_info.")
    return()
  }

  # Get version information for the remaining files
  file_ids <- kwb.utils::selectColumns(file_info, "fileid")
  version_info <- get_version_info(file_ids, dbg = FALSE)

  if (is.null(version_info)) {
    return()
  }

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
