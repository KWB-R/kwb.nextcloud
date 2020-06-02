# dav_path ---------------------------------------------------------------------
dav_path <- function()
{
  "/remote.php/dav"
}

# fileid_to_version_href -------------------------------------------------------
fileid_to_version_href <- function(fileid = "", user = nextcloud_user())
{
  file.path(dav_path(), "versions", user, "versions", fileid)
}

# href_to_url ------------------------------------------------------------------
href_to_url <- function(href)
{
  file.path(nextcloud_url(), href)
}

# path_to_file_href ------------------------------------------------------------
path_to_file_href <- function(path = "", user = nextcloud_user())
{
  file.path(dav_path(), "files", user, path)
}
