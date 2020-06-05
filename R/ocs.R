# get_cloud_capabilities -------------------------------------------------------
get_cloud_capabilities <- function()
{
  href <- "ocs/v1.php/cloud/capabilities"

  httr::content(ocs_request(href))
}

# get_user_info ----------------------------------------------------------------
get_user_info <- function(username)
{
  href <- paste0("ocs/v1.php/cloud/users/", username)

  httr::content(ocs_request(href))
}

# get_direct_link --------------------------------------------------------------
get_direct_link <- function(
  path = NULL,
  fileid = lookup_fileid(path, user = nextcloud_user(), auth = auth),
  auth = nextcloud_auth()
)
{
  if (length(fileid) > 1L) {

    links <- lapply(fileid, get_direct_link, path = NULL)
    not_null <- ! sapply(links, is.null)

    return(kwb.utils::noFactorDataFrame(
      fileid = fileid[not_null],
      link = unlist(links[not_null])
    ))
  }

  href <- "ocs/v2.php/apps/dav/api/v1/direct"

  response <- ocs_request(
    href,
    verb = "POST",
    auth = auth,
    body = paste0("fileId=", fileid)
  )

  httr::content(response)$ocs$data$url
}

# ocs_request ------------------------------------------------------------------
ocs_request <- function(
  href, verb = "GET", auth = nextcloud_auth(), body = NULL, headers = NULL,
  as = "response"
)
{
  nextcloud_request(
    href = href,
    verb = verb,
    auth = auth,
    headers = c(
      list(
        "OCS-APIRequest" = "true",
        "Content-Type" = "application/x-www-form-urlencoded"
      ),
      headers
    ),
    body = body,
    as = as
  )
}
