# move_file_or_folder --------------------------------------------------------

#' Move a File or Folder on the Cloud
#'
#' @param path path to the file or folder to be moved
#' @param path_target path to the target file or folder to be moved to
#' @param overwrite whether to overwrite the file or folder in case that the
#'   file or folder already exists (default: FALSE)
#' @param \dots further arguments passed to
#'   \code{kwb.nextcloud:::nextcloud_request}
#' @param dbg print debug messages (default: TRUE)
#' @inheritParams list_files
#' @export
#' @importFrom kwb.utils catAndRun
move_file_or_folder <- function(
  path,
  path_target,
  overwrite = FALSE,
  user = nextcloud_user(),
  auth = nextcloud_auth(),
  dbg = TRUE,
  ...
)
{
  response <- kwb.utils::catAndRun(
    messageText = sprintf(
      "Moving file/folder from '%s' to '%s'", path, path_target
    ),
    dbg = dbg,
    expr = {
      nextcloud_request(
        href = path_to_file_href(path),
        verb = "MOVE",
        auth = auth,
        headers = list(
          Destination = href_to_url(path_to_file_href(path_target)),
          Overwrite = ifelse(overwrite, "T", "F")
        ),
        ...
      )
    }
  )

  # What should the function return?
}
