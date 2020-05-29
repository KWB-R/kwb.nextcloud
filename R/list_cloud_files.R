if (FALSE)
{
  urls <- kwb.nextcloud:::get_nextcloud_urls("hsonne", path = "projects")

  info_1 <- kwb.nextcloud:::list_files("departments", method = 1L)
  info_2 <- kwb.nextcloud:::list_files("departments", method = 2L)

  View(info_1)
  View(info_2)

  inspect <- function(xx) writeLines(as.character(xx))

  inspect(x)
  inspect(y1)
  inspect(y2)
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
  path, user = nextcloud_user(), password = nextcloud_password(), method = 2L
)
{
  stopifnot(method %in% 1:2)

  #path = "proposals/bmbf_digital/Previous-projects/Budget/10_Filled_out_forms"
  #user = nextcloud_user()
  #password = nextcloud_password()

  urls <- get_nextcloud_urls(user, path = path)

  body <- if (method == 2L) {
    '<d:propfind xmlns:d="DAV:" xmlns:oc="http://owncloud.org/ns" xmlns:nc="http://nextcloud.org/ns">
    <d:prop>
    <d:getlastmodified />
    <d:getetag />
    <d:getcontenttype />
    <d:resourcetype />
    <oc:fileid />
    <oc:permissions />
    <oc:size />
    <d:getcontentlength />
    <nc:has-preview />
    <oc:favorite />
    <oc:comments-unread />
    <oc:owner-display-name />
    <oc:share-types />
    </d:prop>
    </d:propfind>'
  } # else NULL implicitly

  content <- parsed_propfind(urls$url_files, user, password, body = body)

  to_numeric <- function(xx) as.numeric(kwb.utils::defaultIfNULL(xx, "0"))

  if (method == 1L) {

    result <- parse_xml_content_1(content)

    result$bytes <- to_numeric(result$bytes)
    result$size <- to_numeric(result$size)
    result$path <- substr(
      x = result$href,
      start = nchar(urls$user_files) + 3L,
      stop = nchar(result$href)
    )
    result$etag <- gsub('"', '', result$etag)
    result

  } else if (method == 2L) {

    result <- parse_xml_content_2(content)
    View(result)

    result
  }

}

# parse_xml_content_1 ----------------------------------------------------------
parse_xml_content_1 <- function(content)
{
  x_all <- xml2::as_list(content)

  get_file_info <- function(x) {

    #x <- x_all$multistatus[[1L]]

    prop <- x$propstat$prop
    prop_1 <- function(name) kwb.utils::defaultIfNULL(prop[[name]][[1L]], "")

    kwb.utils::noFactorDataFrame(
      etag = prop$getetag[[1L]],
      href = x$href[[1L]],
      status = x$propstat$status[[1L]],
      modified = prop_1("getlastmodified"),
      bytes = prop_1("quota-used-bytes"),
      size = prop_1("getcontentlength")
    )
  }

  #x <- x_all$multistatus[[1]]
  do.call(rbind, lapply(unname(x_all$multistatus), get_file_info))
}

# parse_xml_content_2 ----------------------------------------------------------
parse_xml_content_2 <- function(content)
{
  extract_text <- function(xpath) {
    text <- xml2::xml_text(xml2::xml_find_all(content, paste0("//", xpath)))
  }

  result <- lapply(xpaths(), extract_text)

  lengths(result)

  result <- do.call(data.frame, result)

  stats::setNames(result, xpaths_to_names(xpaths()))
}

# xpaths -----------------------------------------------------------------------
xpaths <- function()
{
  c(
    "d:href",
    "d:getlastmodified",
    "d:getetag",
    "d:getcontenttype",
    "d:resourcetype",
    "oc:fileid",
    "oc:permissions",
    "oc:size",
    "nc:has-preview",
    "oc:favorite",
    "oc:comments-unread",
    "oc:owner-display-name",
    "oc:share-types"
  )
}

# xpaths_to_names --------------------------------------------------------------
xpaths_to_names <- function(xpaths)
{
  sapply(strsplit(xpaths, ":"), "[", 2L)
}

# get_nextcloud_urls -----------------------------------------------------------
get_nextcloud_urls <- function(user, ...)
{
  dictionary <- list(
    base = nextcloud_url(),
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
  url, user = nextcloud_user(), body = NULL, password = nextcloud_password()
)
{
  response <- httr::VERB(
    "PROPFIND", url, nextcloud_auth(user, password), body = body
  )

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

# nextcloud_url ----------------------------------------------------------------
nextcloud_url <- function()
{
  Sys.getenv("NEXTCLOUD_URL")
}
