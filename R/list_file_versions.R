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
