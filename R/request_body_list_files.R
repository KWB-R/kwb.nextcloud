# request_body_list_files ------------------------------------------------------
request_body_list_files <- function()
{
  property_elements <- lapply(get_property_names(), kwb.nextcloud:::tag_xml)

  request_body(element_propfind(do.call(element_prop, property_elements)))
}

# get_property_names -----------------------------------------------------------
get_property_names <- function(as_data_frame = FALSE)
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
  property_pairs <- c(
    "namespace:name:priority",
    "d:getcontentlength:2",
    "d:getcontenttype:2",
    "d:getetag:1",
    "d:getlastmodified:1",
    "d:resourcetype:2",
    "oc:checksums:2",
    "oc:comments-count:1",
    "oc:comments-href:2",
    "oc:comments-unread:2",
    "oc:favorite:2",
    "oc:fileid:1",
    "oc:id:2",
    "oc:owner-display-name:2",
    "oc:owner-id:1",
    "oc:permissions:1",
    "oc:share-types:2",
    "oc:size:1",
    "nc:has-preview:2"
  )

  result <- read.table(text = property_pairs, sep = ":", header = TRUE)

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
element_xml <- function(x, ..., attributes = list(), depth = 0L)
{
  strings <- do.call(c, args = c(
    list(tag_xml(x, attributes, close = FALSE)),
    lapply(list(...), indent, 1L),
    list(tag_xml(x, close = 2L))
  ))

  indent(strings, depth)
}

# tag_xml ----------------------------------------------------------------------
tag_xml <- function(x, attributes = NULL, close = TRUE, depth = 0L)
{
  # Closed must be FALSE if attributes are given
  stopifnot(is.null(attributes) || isFALSE(close))

  string <- paste0(
    "<",
    if (close == 2L) "/",
    x,
    if (length(attributes)) paste0(" ", key_value(.list = attributes)),
    if (close == 1L) " /",
    ">"
  )

  indent(string, depth)
}

# element_prop -----------------------------------------------------------------
element_prop <- function(..., attributes = NULL, depth = 0L)
{
  element_xml("d:prop", ..., attributes = attributes, depth = depth)
}

