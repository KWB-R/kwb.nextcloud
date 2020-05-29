if (FALSE)
{
  path <- "proposals/bmbf_digital/Previous-projects/Budget/10_Filled_out_forms"
  path <- "projects"
  path <- "projects/finale"
  path <- "/projects/finale///"

  info <- kwb.nextcloud:::list_files(path)
  View(info)

  infos <- lapply(1, function(m) kwb.nextcloud:::list_files(path, method = m))

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
  #kwb.utils::assignPackageObjects("kwb.nextcloud")
  #path = "proposals/bmbf_digital/Previous-projects/Budget"
  #user = nextcloud_user();password = nextcloud_password();method=1L

  path <- remove_leading_slashes(path)

  stopifnot(method %in% 1:2)

  urls <- get_nextcloud_urls(user, path = path)

  body <- request_body_list_files()
  #cat(body)

  content <- parsed_propfind(url = urls$url_files, user, password, body = body)

  to_numeric <- function(xx) as.numeric(kwb.utils::defaultIfNULL(xx, "0"))

  if (method == 1L) {

    result <- parse_xml_content_1(content)

    result$href_orig <- result$href

    #start <- nchar(urls$user_files) + nchar(dirname(path)) + 4L
    start <- min(nchar(result$href)) + 1L

    result$href <- substr(x = result$href, start, stop = nchar(result$href))

    result$getlastmodified <- to_posix(x = result$getlastmodified)
    result$getetag <- gsub('"', "", result$getetag)
    result$fileid <- to_numeric(result$fileid)
    result$size <- to_numeric(result$size)
    result$has.preview <- result$has.preview != "false"
    result$favorite <- to_numeric(result$favorite)
    result$comments.unread <- to_numeric(result$comments.unread)

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

  responses <- unname(x_all$multistatus)

  kwb.utils::safeRowBindAll(lapply(responses, parse_response))
}

# parse_response ---------------------------------------------------------------
parse_response <- function(response)
{
  #response <- responses[[1L]]
  elements <- names(response)

  stopifnot(all(elements %in% c("href", "propstat")))
  stopifnot(sum(elements == "href") == 1L)

  propstats <- lapply(response[elements == "propstat"], parse_propstat)

  for (i in seq_along(propstats)) {
    names_i <- names(propstats[[i]])
    is_status <- names_i == "status"
    names_i[is_status] <- paste0(names_i[is_status], ".", i)
    names(propstats[[i]]) <- names_i
  }

  cbind(
    kwb.utils::noFactorDataFrame(href = parse_href(href = response$href)),
    do.call(cbind, c(unname(propstats), list(stringsAsFactors = FALSE))),
    stringsAsFactors = FALSE
  )
}

# parse_href -------------------------------------------------------------------
parse_href <- function(href)
{
  stopifnot(is.list(href), length(href) == 1L)
  result <- href[[1L]]
  stopifnot(length(result) == 1L)
  result
}

# parse_propstat ---------------------------------------------------------------
parse_propstat <- function(propstat)
{
  #propstat <- propstats[[1]]

  stopifnot(identical(sort(names(propstat)), c("prop", "status")))

  cbind(
    parse_prop(prop = propstat$prop),
    status = parse_status(status = propstat$status),
    stringsAsFactors = FALSE
  )
}

# parse_status -----------------------------------------------------------------
parse_status <- function(status)
{
  stopifnot(is.list(status), length(status) == 1L)
  result <- status[[1]]
  stopifnot(is.character(result), length(result) == 1L)
  result
}

# parse_prop -------------------------------------------------------------------
parse_prop <- function(prop)
{
  stopifnot(is.list(prop))
  stopifnot(all(names(prop) %in% c(
    "comments-unread",
    "favorite",
    "fileid",
    "getcontentlength",
    "getcontenttype",
    "getetag",
    "getlastmodified",
    "has-preview",
    "owner-display-name",
    "permissions",
    "resourcetype",
    "share-types",
    "size"
  )))

  do.call(kwb.utils::noFactorDataFrame, lapply(prop, function(x) {
    if (length(x) == 0L) "" else as.character(x)
  }))
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

  if (httr::http_error(response)) {

    xml <- httr::content(response)

    stop(xml2::xml_text(xml2::xml_find_all(xml, "/d:error/s:message")))
  }

  httr::content(response, as = "parsed")
}

# nextcloud_auth ---------------------------------------------------------------
nextcloud_auth <- function(
  user = nextcloud_user(), password = nextcloud_password()
)
{
  httr::authenticate(user, password)
}
