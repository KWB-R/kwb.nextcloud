# Test direct download ---------------------------------------------------------
if (FALSE)
{
  # Make a backup of a (better not too big) folder on the cloud
  backup_file <- kwb.nextcloud::backup_cloud_folder("projects/fakin")

  # Open the containing folder
  kwb.utils::hsOpenWindowsExplorer(dirname(backup_file))
}

# Test OCS API -----------------------------------------------------------------
if (FALSE)
{
  # What capabilities does the cloud offer?
  capabilities <- kwb.nextcloud:::get_cloud_capabilities()
  str(capabilities$ocs$data$capabilities)

  # Information about a user (yourself or any if you are admin)
  user_info <- kwb.nextcloud:::get_user_info("hsonne")
  str(user_info)

  # Get a link for anybody to download a file (not a folder!)
  kwb.nextcloud:::get_direct_link("documents") # error, only for files!
  kwb.nextcloud:::get_direct_link("documents/testfile.txt")

  # Or by fileid, also for many files at once
  path <- "projects/fakin/bmbf-schlussbericht"
  file_info <- kwb.nextcloud::list_files(path, full_info = TRUE)
  links <- kwb.nextcloud:::get_direct_link(fileid = file_info$fileid)
  links # data frame with columns "fileid" and "link"
}
