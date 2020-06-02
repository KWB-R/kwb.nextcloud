if (FALSE)
{
  info <- kwb.nextcloud:::list_files(
    path = "departments",
    pattern = "xlsx$",
    recursive = TRUE,
    max_depth = 2,
    full_info = TRUE
  )

  View(info)

  # Paths to files to be downloaded
  (paths <- file.path(kwb.utils::getAttribute(info, "root"), info$file))
  info$href_orig

  # Download files by href (important when downloading versions)
  downloaded_files <- kwb.nextcloud:::download_files(info$href_orig)

  # Download files by path
  downloaded_files_2 <- kwb.nextcloud:::download_files(paths = paths)

  kwb.utils::hsOpenWindowsExplorer(dirname(dirname(downloaded_files[1])))

  # Get information on available versions. There seems to be only an entry for
  # files that have more than one version
  version_info <- kwb.nextcloud:::get_version_info(info$fileid[! info$isdir])

  View(version_info)

  merge(info[, c("file", "fileid")], version_info, all.x = TRUE, by = "fileid")

  href <- version_info$href

  kwb.nextcloud:::download_files(href)
}

