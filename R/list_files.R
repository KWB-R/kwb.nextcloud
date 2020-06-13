# list_files -------------------------------------------------------------------

#' List Files on the Nextcloud Server
#'
#' @param path path to the nextcloud folder to be listed
#' @param pattern an optional regular expression. Only file names which match
#'   the regular expression will be returned.
#' @param recursive if \code{TRUE} the contents of subfolders are listed as
#'   well, up to the given \code{max_depth}.
#' @param full_info if \code{TRUE} the full file information is returned as a
#'   data frame. If \code{FALSE} (the default) only the (relative) file paths
#'   are returned as a vector of character.
#' @param user user name, by default taken from the environment variable
#'   "NEXTCLOUD_USER".
#' @param auth authentication header as provided by
#'   \code{kwb.nextcloud:::nextcloud_auth}
#' @param max_depth maximum recursion depth if \code{recursive = TRUE}. By
#'   default \code{max_depth} is \code{NA} meaning that the function behaves
#'   "fully recursive".
#' @param \dots further arguments passed to
#'   \code{\link[kwb.utils]{listToDepth}}.
#' @importFrom kwb.utils listToDepth moveColumnsToFront renameColumns
#' @importFrom kwb.utils selectColumns toLookupList
#' @export
#' @return vector of character or data frame, each with attribute "root" being
#'   set to the value of \code{path}.
#'
list_files <- function(
  path = "",
  pattern = NULL,
  recursive = FALSE,
  full_info = FALSE,
  user = nextcloud_user(),
  auth = nextcloud_auth(),
  max_depth = NA,
  ...
)
{
  # Test for endless recursion:
  #kwb.nextcloud::list_files(recursive = TRUE)
  #kwb.utils::assignPackageObjects("kwb.nextcloud")
  #kwb.utils:::assignArgumentDefaults(list_files)

  file_info <- kwb.utils::listToDepth(
    path,
    max_depth = ifelse(recursive, max_depth, 0L),
    full_info = full_info,
    FUN = list_cloud_files,
    pattern = pattern,
    user = user,
    auth = auth
    , ...
  )

  result <- if (full_info) {

    result <- kwb.utils::moveColumnsToFront(file_info, c("file", "isdir"))

    rename_properties(result)

  } else {

    kwb.utils::selectColumns(file_info, "file")
  }


  structure(result, root = path)
}

# list_cloud_files -------------------------------------------------------------

#' @importFrom kwb.utils selectColumns
#' @keywords internal
list_cloud_files <- function(
  path = character(),
  full_info = FALSE,
  pattern = NULL,
  user = nextcloud_user(),
  auth = nextcloud_auth(),
  priority = 1L,
  parent_only = FALSE # TRUE -> return only the properties of the parent folder
)
{
  #kwb.utils::assignPackageObjects("kwb.nextcloud")
  #kwb.utils::assignArgumentDefaults(list_cloud_files);path=""

  if (length(path) == 0L) {

    # Return an empty result data frame as a template
    return(list_cloud_files(path = "", full_info)[FALSE, ])
  }

  path <- remove_leading_slashes(path)

  message("Listing ", path)

  content <- nextcloud_request(
    href = path_to_file_href(path, user),
    verb = "PROPFIND",
    auth = auth,
    body = request_body_list_files(),
    as = "parsed",
    headers = list(Depth = ifelse(parent_only, 0L, 1L))
  )

  # Parse XML content to data frame
  result <- parse_xml_content(content)

  # Helper functions
  get_column <- function(x) kwb.utils::selectColumns(result, x)
  remove_shortest_left <- function(x) substring(x, min(nchar(x)) + 1L)

  # Convert types from text to numeric/POSIXct
  result <- convert_types(result)

  # Provide columns as required by kwb.utils::listToDepth()
  result$file <- remove_shortest_left(get_column("href"))
  result$isdir <- get_column("resourcetype") == "list()"

  # Exclude the requested folder itself
  keep <- keep_row(
    parent_only,
    path = kwb.utils::selectColumns(result, "file"),
    isdir = kwb.utils::selectColumns(result, "isdir")
  )

  # Define the columns to keep
  columns <- determine_required_columns(
    full_info,
    all_names = names(result),
    priority = priority
  )

  structure(result[keep, columns], root = path)
}

# parse_xml_content ------------------------------------------------------------

#' @importFrom xml2 as_list
#' @importFrom kwb.utils safeRowBindAll
#' @keywords internal
parse_xml_content <- function(content)
{
  x_all <- xml2::as_list(content)

  responses <- unname(x_all$multistatus)

  kwb.utils::safeRowBindAll(lapply(responses, parse_response))
}

# parse_response ---------------------------------------------------------------
parse_response <- function(response)
{
  #response <- responses[[2L]]
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
  #propstat <- propstats[[3]]

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

#' @importFrom kwb.utils noFactorDataFrame stringList
#' @keywords internal
parse_prop <- function(prop, do_warn = FALSE)
{
  stopifnot(is.list(prop))

  property_info <- get_property_info()
  prop_names <- names(prop)
  unexpected <- ! prop_names %in% sort(unique(property_info$name))

  if (any(unexpected) && do_warn) warning(
    "Unexpected properties: ", kwb.utils::stringList(prop_names[unexpected])
  )

  do.call(kwb.utils::noFactorDataFrame, lapply(prop, function(x) {
    if (length(x) == 0L) "" else as.character(x)
  }))
}

# convert_types ----------------------------------------------------------------
convert_types <- function(result)
{
  get_column <- function(x) kwb.utils::selectColumns(result, x)
  to_numeric <- function(x) as.numeric(kwb.utils::defaultIfNULL(x, "0"))

  result$getlastmodified <- to_posix(get_column("getlastmodified"))
  result$getetag <- gsub('"', "", get_column("getetag"))
  result$fileid <- to_numeric(get_column("fileid"))
  result$size <- to_numeric(get_column("size"))
  result$has.preview <- get_column("has.preview") != "false"
  result$favorite <- to_numeric(get_column("favorite"))
  result$comments.unread <- to_numeric(get_column("comments.unread"))

  result
}

# keep_row ---------------------------------------------------------------------
keep_row <- function(parent_only, path, isdir, pattern = NULL)
{
  keep <- if (parent_only) {

    TRUE

  } else {

    nzchar(path)
  }

  if (! is.null(pattern)) {
    keep <- keep & (isdir | grepl(pattern, path))
  }

  keep
}

# determine_required_columns ---------------------------------------------------
determine_required_columns <- function(full_info, all_names, priority = NA)
{
  main_columns <- c("file", "isdir")

  if (! full_info) {
    return(main_columns)
  }

  if (is.na(priority)) {
    return(all_names)
  }

  prop_info <- get_property_info()

  columns <- prop_info$name[prop_info$priority <= priority]

  intersect(c(main_columns, columns, "href"), all_names)
}
