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
  ignore = "^(status|quota|getcontent|resource)"
)
{
  #user <- "hsonne"

  results <- lapply(file_ids, function(fileid) {

    kwb.utils::catAndRun(
      paste("Getting version info for fileid =", fileid),
      get_one_version_info(fileid, ignore = ignore, user = user)
    )
  })

  kwb.utils::safeRowBindAll(kwb.utils::excludeNULL(results, dbg = FALSE))
}

# get_one_version_info ---------------------------------------------------------
get_one_version_info <- function(
  fileid, ignore = NULL, user = nextcloud_user, auth = nextcloud_auth()
)
{
  # Shortcut
  pull <- kwb.utils::selectColumns

  href <- fileid_to_version_href(fileid, user)

  result <- try({

    content <- nextcloud_request(
      href, verb = "PROPFIND", auth = auth, body = NULL, as = "parsed"
    )

    info <- parse_xml_content_1(content)

    info <- info[pull(info, "resourcetype") != "list()", ]

    if (! is.null(x <- pull(info, "getlastmodified"))) {

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
