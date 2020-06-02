# download_files ---------------------------------------------------------------
download_files <- function(
  paths,
  target_dir = create_download_dir("nextcloud_"),
  user = nextcloud_user()
)
{
  #kwb.utils::assignPackageObjects("kwb.nextcloud")

  paths_decoded <- unlist(lapply(paths, decode_url))

  # Keep only the necessary tree structure
  target_paths <- kwb.file::remove_common_root(paths_decoded)

  # Create required target folders
  create_directories(file.path(target_dir, unique_dirnames(target_paths)))

  # Create the full paths to the target files
  target_files <- file.path(target_dir, target_paths)

  unlist(mapply(
    FUN = download_cloud_file,
    paths,
    target_files,
    MoreArgs = list(user = user),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  ))
}

# download_cloud_file ----------------------------------------------------------
download_cloud_file <- function(path, target_file, user = nextcloud_user())
{
  #i <- 1L
  #path <- paths[i]
  #target_file <- target_files[i]

  # Expect the target directory to exist
  stopifnot(file.exists(dirname(target_file)))

  kwb.utils::catAndRun(paste("Downloading", path), {

    url <- get_nextcloud_urls(user, path = path)$url_files

    response <- httr::GET(url, nextcloud_auth())

    stop_on_httr_error(response)

    write_content_to_file(response, target_file)

    target_file
  })
}

# write_content_to_file --------------------------------------------------------
write_content_to_file <- function(response, target_file)
{
  content <- httr::content(response, type = "application/octet-stream")

  result <- try(writeBin(content, target_file))

  if (inherits(result, "try-error")) {

    stop(
      "Could not write the response data with writeBin(). ",
      "The content type is: ", httr::headers(response)[["content-type"]],
      call. = FALSE
    )
  }
}
