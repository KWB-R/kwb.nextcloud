if (FALSE)
{
  urls <- kwb.nextcloud:::get_nextcloud_urls("hsonne", path = "projects")

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
  #user = kwb.nextcloud:::nextcloud_user()
  #password = kwb.nextcloud:::nextcloud_password()

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

  if (method == 1L) {

    parse_xml_content_1(content, path_offset = nchar(urls$user_files) + 2L)

  } else if (method == 2L) {

    parse_xml_content_2(content)
  }
}

# parse_xml_content_1 ----------------------------------------------------------
parse_xml_content_1 <- function(content, path_offset = 0L)
{
  x_all <- xml2::as_list(content)

  to_numeric <- function(xx) as.numeric(kwb.utils::defaultIfNULL(xx, "0"))

  get_file_info <- function(x) {
    prop <- x$propstat$prop
    href <- x$href[[1]]
    kwb.utils::noFactorDataFrame(
      id = gsub('"', '', prop$getetag[[1]]),
      #href = href,
      path = substr(href, path_offset + 1L, nchar(href)),
      #status = x$propstat$status[[1]],
      modified = prop$getlastmodified[[1]],
      bytes = to_numeric(prop$`quota-used-bytes`[[1]]),
      size = to_numeric(prop$getcontentlength[[1]])
    )
  }

  #x <- x_all$multistatus[[1]]
  do.call(rbind, lapply(unname(x_all$multistatus), get_file_info))
}

# parse_xml_content_2 ----------------------------------------------------------
parse_xml_content_2 <- function(content)
{
  extract_text <- function(xpath) {
    xml2::xml_text(xml2::xml_find_all(content, paste0("//", xpath)))
  }

  result <- do.call(data.frame, lapply(xpaths(), extract_text))

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
