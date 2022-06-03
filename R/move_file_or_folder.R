# move_file_or_folder --------------------------------------------------------

#' Move a File or Folder on the Cloud
#'
#' @param path path to the file or folder to be moved
#' @param path_target path to the target file or folder to be moved to
#' @param overwrite overwrite if file/folder is already existing (default: FALSE)
#' @param \dots further arguments passed to
#'   \code{kwb.nextcloud:::nextcloud_request}
#' @param dbg print debug messages (default: TRUE)
#' @inheritParams list_files
#' @export
#' @importFrom kwb.utils catAndRun
move_file_or_folder <- function(path,
                                path_target,
                                overwrite = FALSE,
                                user = nextcloud_user(),
                                auth = nextcloud_auth(),
                                dbg = TRUE,
                                ...)
{
  href = path_to_file_href(path)

  url_target <- href_to_url(path_to_file_href(path_target))
  msg <- sprintf("Moving file/folder from '%s' to '%s'",
                 path,
                 path_target)
  kwb.utils::catAndRun(msg, expr = {
  response <- nextcloud_request(href,
                                "MOVE",
                                auth = auth,
                                headers = list(Destination = url_target,
                                               Overwrite = ifelse(overwrite,
                                                                  "T",
                                                                  "F")),
                                ...)},
  dbg = dbg
  )
}
