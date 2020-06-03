# request_body_list_files ------------------------------------------------------
request_body_list_files <- function()
{
  request_body(
    element_propfind(
      element_prop(
        tag_xml("d:getlastmodified"),
        tag_xml("d:getetag"),
        tag_xml("d:getcontenttype"),
        tag_xml("d:resourcetype"),
        tag_xml("oc:fileid"),
        tag_xml("oc:permissions"),
        tag_xml("oc:size"),
        tag_xml("d:getcontentlength"),
        tag_xml("nc:has-preview"),
        tag_xml("oc:favorite"),
        tag_xml("oc:comments-unread"),
        tag_xml("oc:owner-display-name"),
        tag_xml("oc:share-types")
      )
    )
  )
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

