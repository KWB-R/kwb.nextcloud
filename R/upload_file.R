# upload_file ------------------------------------------------------------------

#' Upload a Local File to the Cloud
#'
#' @param file path to local file
#' @param target_path path to folder on the cloud into which to upload the file
#' @inheritParams list_files
#' @export
#'
upload_file <- function(
  file,
  target_path = "documents",
  user = nextcloud_user(),
  auth = nextcloud_auth()
)
{
  if (is_directory(file)) stop(
    file, " is a directory.\nThis function can only upload files."
  )

  if (! is_cloud_directory(path = target_path, user = user, auth = auth)) stop(
    "target_path (", target_path, ") must be a directory and not a file."
  )

  href <- path_to_file_href(file.path(target_path, basename(file)), user = user)

  kwb.utils::catAndRun(
    paste0("Uploading\n  ", file, "\nto\n  ", target_path),
    nextcloud_request(href, "PUT", auth = auth, body = httr::upload_file(file))
  )
}
