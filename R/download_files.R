# download_files ---------------------------------------------------------------
download_files <- function(
  hrefs = NULL,
  target_dir = create_download_dir("nextcloud_"),
  paths = NULL,
  user = nextcloud_user(),
  auth = nextcloud_auth()
)
{
  #kwb.utils::assignPackageObjects("kwb.nextcloud")
  if (is.null(hrefs) && is.null(paths)) {
    stop("One of hrefs or paths must be given!")
  }

  if (! is.null(hrefs) && ! is.null(paths)) {
    stop("hrefs and paths must not be given at the same time!")
  }

  hrefs <- kwb.utils::defaultIfNULL(hrefs, path_to_file_href(paths, user))
  paths <- kwb.utils::defaultIfNULL(paths, hrefs)

  paths_decoded <- unlist(lapply(paths, decode_url))

  # Keep only the necessary tree structure
  target_paths <- kwb.file::remove_common_root(paths_decoded)

  # Create required target folders
  create_directories(file.path(target_dir, unique_dirnames(target_paths)))

  # Create the full paths to the target files
  target_files <- file.path(target_dir, target_paths)

  unlist(mapply(
    FUN = download_from_href,
    hrefs,
    target_files,
    MoreArgs = list(auth = auth),
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  ))
}

# download_from_href -----------------------------------------------------------
download_from_href <- function(href, target_file, auth = nextcloud_auth())
{
  #i <- 1L
  #path <- paths[i]
  #target_file <- target_files[i]

  # Expect the target directory to exist
  stopifnot(file.exists(dirname(target_file)))

  kwb.utils::catAndRun(paste("Downloading", href), {

    response <- nextcloud_request(href, "GET", auth)

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
