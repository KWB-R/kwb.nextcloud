if (FALSE)
{
  path <- "proposals/bmbf_digital/Previous-projects/Budget/10_Filled_out_forms"
  path <- "departments"

  infos <- lapply(1:2, function(m) kwb.nextcloud:::list_files(path, method = m))

  View(infos[[1]])
  View(infos[[2]])

  inspect <- function(xx) writeLines(as.character(xx))

  inspect(x)
  inspect(y1)
  inspect(y2)
}

# list_files -------------------------------------------------------------------
list_files <- function(
  path, user = nextcloud_user(), password = nextcloud_password(), method = 1L
)
{
  stopifnot(method %in% 1:2)

  #kwb.utils::assignPackageObjects("kwb.nextcloud")
  #path = "proposals/bmbf_digital/Previous-projects/Budget/10_Filled_out_forms"
  #user = nextcloud_user();password = nextcloud_password();method=1L

  urls <- get_nextcloud_urls(user, path = path)

  body <- request_body_list_files()

  content <- parsed_propfind(urls$url_files, user, password, body = body)

  to_numeric <- function(xx) as.numeric(kwb.utils::defaultIfNULL(xx, "0"))

  if (method == 1L) {

    result <- parse_xml_content_1(content)

    result$href <- substr(
      x = result$href,
      start = nchar(urls$user_files) + nchar(dirname(path)) + 4L,
      stop = nchar(result$href)
    )

    result$lastmodified <- to_posix(x = result$lastmodified)
    result$etag <- gsub('"', "", result$etag)
    result$fileid <- to_numeric(result$fileid)
    result$size <- to_numeric(result$size)
    result$has_preview <- result$has_preview != "false"
    result$favorite <- to_numeric(result$favorite)
    result$comments_unread <- to_numeric(result$comments_unread)

    result

  } else if (method == 2L) {

    result <- parse_xml_content_2(content)

    result
  }

  structure(result, root = dirname(path))
}

# to_posix ---------------------------------------------------------------------
to_posix <- function(x)
{
  stopifnot(is.character(x), all(grepl("GMT$", x)))

  locale <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", locale))
  Sys.setlocale("LC_TIME", "C")

  as.POSIXct(x, format = "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT")
}

# parse_xml_content_1 ----------------------------------------------------------
parse_xml_content_1 <- function(content)
{
  x_all <- xml2::as_list(content)

  first_and_only <- function(x) {
    stopifnot(is.list(x))
    n <- length(x)
    if (n == 0L) {
      return("")
    }
    if (n == 1L) {
      x <- x[[1L]]
    }
    if (length(x) == 0L) {
      return("")
    }
    stopifnot(length(x) == 1L)
    x
  }

  get_file_info <- function(x) {

    #x <- responses[[1L]]

    p <- x$propstat$prop

    kwb.utils::noFactorDataFrame(
      href = first_and_only(x$href),
      status = first_and_only(x$propstat$status),
      lastmodified = first_and_only(p$getlastmodified),
      etag = first_and_only(p$getetag),
      resourcetype = first_and_only(p$resourcetype),
      fileid = first_and_only(p$fileid),
      permissions = first_and_only(p$permissions),
      size = first_and_only(p$size),
      has_preview = first_and_only(p$`has-preview`),
      favorite = first_and_only(p$favorite),
      comments_unread = first_and_only(p$`comments-unread`),
      owner_display_name = first_and_only(p$`owner-display-name`),
      share_types = first_and_only(p$`share-types`)
    )
  }

  #x <- x_all$multistatus[[1]]
  responses <- unname(x_all$multistatus)

  do.call(rbind, lapply(responses, get_file_info))
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
