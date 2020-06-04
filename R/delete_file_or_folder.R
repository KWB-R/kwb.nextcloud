# delete_file_or_folder --------------------------------------------------------

#' Delete a File or Folder on the Cloud
#'
#' @param path path to the file or folder to be deleted
#' @param \dots further arguments passed to
#'   \code{kwb.nextcloud:::nextcloud_request}
#' @inheritParams list_files
#' @export
#'
delete_file_or_folder <- function(
  path, user = nextcloud_user(), auth = nextcloud_auth(), ...
)
{
  href <- path_to_file_href(path, user = user)

  response <- nextcloud_request(href, "DELETE", auth = auth, ...)
}
