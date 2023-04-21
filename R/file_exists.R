#' Does a File Exist on the Nextcloud Server?
#'
#' @param path path to the file
#' @return \code{TRUE} if the file exists, else \code{FALSE}
#' @export
file_exists <- function(path)
{
  stopifnot(is.character(path), length(path) == 1L)

  basename(path) %in% list_files(dirname(path), silent = TRUE)
}
