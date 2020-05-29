# nextcloud_user ---------------------------------------------------------------
nextcloud_user <- function()
{
  Sys.getenv("NEXTCLOUD_USER")
}

# nextcloud_password -----------------------------------------------------------
nextcloud_password <- function()
{
  Sys.getenv("NEXTCLOUD_PASSWORD")
}

# nextcloud_url ----------------------------------------------------------------
nextcloud_url <- function()
{
  Sys.getenv("NEXTCLOUD_URL")
}
