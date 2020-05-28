if (FALSE)
{
  path <- "proposals/bmbf_digital/Previous-projects/Budget/10_Filled_out_forms"

  # List files in folder
  file_info <- kwb.nextcloud:::list_files(path = path)

  # Check the result
  View(file_info)

  # Sum of file sizes = folder size?
  sum(file_info$size) == file_info$bytes[1]

  # Filter for xlsx files
  paths <- grep("\\.xlsx", file_info$path, value = TRUE)

  # Download xlsx files
  downloaded_files <- kwb.nextcloud:::download_files(paths)

  kwb.utils::hsOpenWindowsExplorer(dirname(downloaded_files[1]))
}

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

# list_files -------------------------------------------------------------------
list_files <- function(
  path, user = nextcloud_user(), password = nextcloud_password()
)
{
  #path = "proposals/bmbf_digital/Previous-projects/Budget/10_Filled_out_forms"
  #user = kwb.nextcloud:::nextcloud_user()
  #password = kwb.nextcloud:::nextcloud_password()

  urls <- get_nextcloud_urls(user, path = path)

  content <- parsed_propfind(urls$url_files, user, password)

  x_all <- xml2::as_list(content)

  to_numeric <- function(xx) as.numeric(kwb.utils::defaultIfNULL(xx, "0"))

  get_file_info <- function(x) {
    prop <- x$propstat$prop
    href <- x$href[[1]]
    offset <- nchar(urls$user_files) + 3L
    kwb.utils::noFactorDataFrame(
      id = gsub('"', '', prop$getetag[[1]]),
      #href = href,
      path = substr(href, offset, nchar(href)),
      #status = x$propstat$status[[1]],
      modified = prop$getlastmodified[[1]],
      bytes = to_numeric(prop$`quota-used-bytes`[[1]]),
      size = to_numeric(prop$getcontentlength[[1]])
    )
  }

  #x <- x_all$multistatus[[1]]
  do.call(rbind, lapply(unname(x_all$multistatus), get_file_info))
}

# get_nextcloud_urls -----------------------------------------------------------
get_nextcloud_urls <- function(user, ...)
{
  dictionary <- list(
    base = "https://cloud.kompetenz-wasser.de",
    dav = "remote.php/dav",
    user_files = "<dav>/files/<user>",
    user_versions = "<dav>/versions/<user>",
    url_files = "<base>/<user_files>/<path>",
    url_versions = "<base>/<user_versions>/<fileid>"
  )

  kwb.utils::resolve(dictionary, user = user, ...)
}

# parsed_propfind --------------------------------------------------------------
parsed_propfind <- function(
  url, user = nextcloud_user(), password = nextcloud_password()
)
{
  response <- httr::VERB("PROPFIND", url, nextcloud_auth(user, password))

  httr::content(response, as = "parsed")
}

# nextcloud_auth ---------------------------------------------------------------
nextcloud_auth <- function(
  user = nextcloud_user(), password = nextcloud_password()
)
{
  httr::authenticate(user, password)
}

# nextcloud_user ---------------------------------------------------------------
nextcloud_user <- function()
{
  Sys.getenv("NEXTCLOUD_USER")
}

# nextcloud_password -----------------------------------------------------------
nextcloud_password <- function()
{
  Sys.getenv("NEXTCLOUD_PASSWORD")
}
