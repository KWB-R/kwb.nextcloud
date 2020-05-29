# download_files ---------------------------------------------------------------
download_files <- function(
  paths,
  user = nextcloud_user(),
  target_dir = kwb.utils::createDirectory(file.path(
    "~/../Downloads", basename(tempfile(pattern = "nextcloud_"))
  ))
)
{
  unlist(lapply(paths, function(path) {

    kwb.utils::catAndRun(paste("Downloading", path), {

      url <- get_nextcloud_urls(user, path = path)$url_files

      response <- httr::GET(url, nextcloud_auth())

      file <- file.path(target_dir, basename(path))

      writeBin(httr::content(response), file)

      file
    })
  }))
}
