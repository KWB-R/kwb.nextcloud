# list_file_versions -----------------------------------------------------------

#' List the Partner's File Versions (Except Current)
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
  file_info <- kwb.nextcloud::list_files(path, pattern, full_info = TRUE, ...)

  version_info <- kwb.nextcloud::get_version_info(file_info$fileid)

  columns_x <- c("fileid", "file", "lastmodified", "etag")
  columns_y <- c("fileid", "version", "href")

  result <- merge(
    kwb.utils::selectColumns(file_info, columns_x),
    kwb.utils::selectColumns(version_info, columns_y),
    by = "fileid"
  )

  kwb.utils::moveColumnsToFront(result, columns = c(
    "fileid", "file", "version"
  ))
}
