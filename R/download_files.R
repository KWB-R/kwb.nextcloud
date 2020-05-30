# download_files ---------------------------------------------------------------
download_files <- function(
  paths,
  user = nextcloud_user(),
  target_dir = kwb.utils::createDirectory(file.path(
    "~/../Downloads", basename(tempfile(pattern = "nextcloud_"))
  ))
)
{
  #kwb.utils::assignPackageObjects("kwb.nextcloud")
  unlist(lapply(paths, function(path) {

    #path <- paths[1]
    kwb.utils::catAndRun(paste("Downloading", path), {

      url <- get_nextcloud_urls(user, path = path)$url_files

      response <- httr::GET(url, nextcloud_auth())

      stop_on_httr_error(response)

      file <- file.path(target_dir, basename(path))

      content <- httr::content(response)

      result <- try(writeBin(content, file))

      if (inherits(result, "try-error")) {

        stop(
          "Could not write the response data with writeBin(). ",
          "The content type is: ", httr::headers(response)[["content-type"]],
          call. = FALSE
        )
      }

      file
    })
  }))
}
