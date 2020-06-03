if (FALSE)
{
  system.time(file_info <- kwb.nextcloud::list_files(
    path = "departments",
    pattern = "xlsx$",
    recursive = TRUE,
    max_depth = 2,
    full_info = TRUE
  ))

  View(file_info)

  # Download files by href (important when downloading versions)
  downloaded_files <- kwb.nextcloud::download_files(file_info$href)

  # Download files by path
  paths <- file.path(kwb.utils::getAttribute(file_info, "root"), file_info$file)
  downloaded_files_2 <- kwb.nextcloud::download_files(paths = paths)

  kwb.utils::hsOpenWindowsExplorer(dirname(dirname(downloaded_files[1])))

  # Get information on available versions. There seems to be only an entry for
  # files that have more than one version
  version_info <- kwb.nextcloud::get_version_info(
    file_info$fileid[! file_info$isdir]
  )

  View(version_info)

  merge(
    file_info[, c("file", "fileid")],
    version_info,
    all.x = TRUE,
    by = "fileid"
  )

  kwb.nextcloud::download_files(href = version_info$href)
}
