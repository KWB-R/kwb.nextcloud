# attributes_propfind ----------------------------------------------------------
attributes_propfind <- function(owncloud = TRUE, nextcloud = TRUE)
{
  c(
    list("xmlns:d" = "DAV:"),
    if (owncloud) list("xmlns:oc" = "http://owncloud.org/ns"),
    if (nextcloud) list("xmlns:nc" = "http://nextcloud.org/ns")
  )
}

# element_prop -----------------------------------------------------------------
element_prop <- function(..., attributes = NULL, depth = 0L)
{
  element_xml("d:prop", ..., attributes = attributes, depth = depth)
}

# element_propfind -------------------------------------------------------------
element_propfind <- function(
  ..., owncloud = TRUE, nextcloud = TRUE, depth = 0L
)
{
  attributes <- attributes_propfind(owncloud, nextcloud)

  element_xml("d:propfind", ..., attributes = attributes, depth = depth)
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

# indent -----------------------------------------------------------------------
indent <- function(x, depth = 0L)
{
  if (depth > 0L) {
    paste0(kwb.utils::space(depth), x)
  } else {
    x
  }
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

# request_body -----------------------------------------------------------------
request_body <- function(...)
{
  paste(c(xml_declaration(), ...), collapse = "\n")
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

# xml_declaration --------------------------------------------------------------
xml_declaration <- function()
{
  paste(
    "<?xml", key_value(.list = list(version = "1.0", encoding = "utf-8")), "?>"
  )
}
