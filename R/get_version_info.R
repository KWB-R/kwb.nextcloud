# get_version_info ------------------------------------------------------------

#' Get Version Information on Files of Given IDs
#'
#' @param file_ids vector of integer representing file ids (values returned in
#'   column \code{fileid}) by \code{\link{list_files}}
#' @param user nextcloud user
#' @param ignore pattern matching columns to be excluded from the result. Set
#'   to \code{NULL} to see what columns are available.
#' @return data frame with one row per version. There seems to be only an entry
#'   if the corresponding file as more than one version.
#' @export
#'
get_version_info <- function(
  file_ids,
  user = nextcloud_user(),
  ignore = "^(status|quota|getcontent|resource|href)"
)
{
  #user <- "hsonne"

  results <- lapply(file_ids, function(fileid) {

    kwb.utils::catAndRun(
      paste("Getting version info for fileid =", fileid),
      get_one_version_info(user, fileid, ignore)
    )
  })

  kwb.utils::safeRowBindAll(kwb.utils::excludeNULL(results, dbg = FALSE))
}

# get_one_version_info ---------------------------------------------------------
get_one_version_info <- function(user, fileid, ignore)
{
  # Shortcut
  pull <- kwb.utils::selectColumns

  # API URL to ask for versions
  url <- get_nextcloud_urls(user, fileid = fileid)$url_versions

  result <- try({

    info <- parse_xml_content_1(parsed_propfind(url))

    info <- info[pull(info, "resourcetype") != "list()", ]

    if (! is.null(x <- info$getlastmodified)) {
      info$getlastmodified <- to_posix(x)
      info <- kwb.utils::orderBy(info, "getlastmodified")
    }

    if (! is.null(ignore)) {
      info <- info[, ! grepl(ignore, names(info)), drop = FALSE]
    }

    n_versions <- nrow(info)

    cbind.data.frame(
      fileid = rep(fileid, n_versions),
      version = seq_len(n_versions),
      info
    )
  })

  if (inherits(result, "try-error")) {
    return(NULL)
  }

  result
}
