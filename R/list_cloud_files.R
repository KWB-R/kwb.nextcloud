if (FALSE)
{
  file_info <- kwb.budget:::list_cloud_folder(
    folder = "proposals/bmbf_digital/Previous-projects/Budget/10_Filled_out_forms"
  )

  View(file_info)

  sum(file_info$size) == file_info$bytes[1]

  url <- get_nextcloud_urls("hsonne", fileid = file_info$id[1])$url_versions

  xml_versions <- parsed_propfind(url)

}

# list_cloud_folder ------------------------------------------------------------
list_cloud_folder <- function(
  folder, user = nextcloud_user(), password = nextcloud_password()
)
{
  if (FALSE) {
    folder = "proposals/bmbf_digital/Previous-projects/Budget/10_Filled_out_forms"
    user = Sys.getenv("NEXTCLOUD_USER")
    password = Sys.getenv("NEXTCLOUD_PASSWORD")
  }

  urls <- get_nextcloud_urls(user, folder = folder)

  content <- parsed_propfind(urls$url_files, user, password)

  x_all <- xml2::as_list(content)

  to_numeric <- function(xx) as.numeric(kwb.utils::defaultIfNULL(xx, "0"))

  get_file_info <- function(x) {
    prop <- x$propstat$prop
    kwb.utils::noFactorDataFrame(
      id = gsub('"', '', prop$getetag[[1]]),
      #path = x$href[[1]],
      filename = basename(x$href[[1]]),
      #status = x$propstat$status[[1]],
      modified = prop$getlastmodified[[1]],
      bytes = to_numeric(prop$`quota-used-bytes`[[1]]),
      size = to_numeric(prop$getcontentlength[[1]])
    )
  }

  do.call(rbind, lapply(unname(x_all$multistatus), get_file_info))
}

# get_nextcloud_urls -----------------------------------------------------------
get_nextcloud_urls <- function(user, ...)
{
  dictionary <- list(
    dav = "https://cloud.kompetenz-wasser.de/remote.php/dav",
    url_files = "<dav>/files/<user>/<folder>",
    url_versions = "<dav>/versions/<user>/<fileid>"
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
