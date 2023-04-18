# request_body_list_files ------------------------------------------------------
request_body_list_files <- function()
{
  property_strings <- get_property_info(as_data_frame = FALSE)

  property_elements <- lapply(property_strings, tag_xml)

  request_body(element_propfind(do.call(element_prop, property_elements)))
}

# get_property_info ------------------------------------------------------------

#' @importFrom utils read.table
#' @keywords internal
get_property_info <- function(as_data_frame = TRUE)
{
  # The following properties are supported (https://docs.nextcloud.com/server/
  # 15/developer_manual/client_apis/WebDAV/basic.html)
  #
  # {DAV:}getlastmodified
  # {DAV:}getetag
  # {DAV:}getcontenttype
  # {DAV:}resourcetype
  # {DAV:}getcontentlength
  #
  # {http://owncloud.org/ns}id The fileid namespaced by the instance id,
  #   globally unique
  # {http://owncloud.org/ns}fileid The unique id for the file within the
  #   instance
  # {http://owncloud.org/ns}favorite
  # {http://owncloud.org/ns}comments-href
  # {http://owncloud.org/ns}comments-count
  # {http://owncloud.org/ns}comments-unread
  # {http://owncloud.org/ns}owner-id The user id of the owner of a shared file
  # {http://owncloud.org/ns}owner-display-name The display name of the owner of
  #   a shared file
  # {http://owncloud.org/ns}share-types
  # {http://owncloud.org/ns}checksums
  # {http://owncloud.org/ns}size Unlike getcontentlength, this property also
  #   works for folders reporting the size of everything in the folder.
  #
  # {http://nextcloud.org/ns}has-preview

  # In the following, the properties are listed in alphabetical order within
  # each namespace
  property_info <- c(
    "namespace:name:priority:column",
    "d:getcontentlength:2:contentlength",
    "d:getcontenttype:2:contenttype",
    "d:getetag:1:etag",
    "d:getlastmodified:1:lastmodified",
    "d:resourcetype:2:resourcetype",
    "oc:checksums:2:checksum",
    "oc:comments-count:1:commentscount",
    "oc:comments-href:2:commentshref",
    "oc:comments-unread:2:commentsunread",
    "oc:favorite:2:favorite",
    "oc:fileid:1:fileid",
    "oc:id:2:id",
    "oc:owner-display-name:2:ownername",
    "oc:owner-id:1:ownerid",
    "oc:permissions:1:permissions",
    "oc:share-types:2:sharetypes",
    "oc:size:1:size",
    "nc:has-preview:2:haspreview"
  )

  result <- utils::read.table(
    text = property_info, sep = ":", header = TRUE, stringsAsFactors = FALSE
  )

  if (! as_data_frame) {
    return(kwb.utils::pasteColumns(result, names(result)[1:2], sep = ":"))
  }

  result
}

# request_body -----------------------------------------------------------------
request_body <- function(...)
{
  paste(c(xml_declaration(), ...), collapse = "\n")
}

# xml_declaration --------------------------------------------------------------
xml_declaration <- function()
{
  paste(
    "<?xml", key_value(.list = list(version = "1.0", encoding = "utf-8")), "?>"
  )
}

# key_value --------------------------------------------------------------------
key_value <- function(key, value, .list = NULL)
{
  if (! is.null(.list)) {

    keys <- names(.list)
    values <- unlist(.list)

    return(do.call(paste, lapply(seq_along(.list), function(i) {
      key_value(keys[i], values[i])
    })))
  }

  paste0(key, "=\"", value, "\"")
}

# element_propfind -------------------------------------------------------------
element_propfind <- function(
  ..., owncloud = TRUE, nextcloud = TRUE, depth = 0L
)
{
  attributes <- attributes_propfind(owncloud, nextcloud)

  element_xml("d:propfind", ..., attributes = attributes, depth = depth)
}

# attributes_propfind ----------------------------------------------------------
attributes_propfind <- function(owncloud = TRUE, nextcloud = TRUE)
{
  c(
    list("xmlns:d" = "DAV:"),
    if (owncloud) list("xmlns:oc" = "http://owncloud.org/ns"),
    if (nextcloud) list("xmlns:nc" = "http://nextcloud.org/ns")
  )
}

# element_xml ------------------------------------------------------------------
#' @importFrom kwb.utils indent
element_xml <- function(x, ..., attributes = list(), depth = 0L)
{
  strings <- do.call(c, args = c(
    list(tag_xml(x, attributes, close = FALSE)),
    lapply(list(...), kwb.utils::indent, 1L),
    list(tag_xml(x, close = 2L))
  ))

  kwb.utils::indent(strings, depth)
}

# tag_xml ----------------------------------------------------------------------

#' @importFrom kwb.utils indent
tag_xml <- function(x, attributes = NULL, close = TRUE, depth = 0L)
{
  # Closed must be FALSE if attributes are given
  stopifnot(is.null(attributes) || isFALSE(close))

  kwb.utils::indent(depth = depth, paste0(
    "<",
    if (close == 2L) "/",
    x,
    if (length(attributes)) paste0(" ", key_value(.list = attributes)),
    if (close == 1L) " /",
    ">"
  ))
}

# element_prop -----------------------------------------------------------------
element_prop <- function(..., attributes = NULL, depth = 0L)
{
  element_xml("d:prop", ..., attributes = attributes, depth = depth)
}

