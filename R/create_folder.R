# create_folder ----------------------------------------------------------------

#' Create a Folder on the Cloud
#'
#' @param path path to the folder to be created
#' @inheritParams list_files
#' @export
#'
create_folder <- function(
  path, user = nextcloud_user(), auth = nextcloud_auth()
)
{
  href <- path_to_file_href(path, user = user)

  response <- nextcloud_request(href, "MKCOL", auth = auth)
}
