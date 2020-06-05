# backup_cloud_folder ----------------------------------------------------------

#' Backup a Cloud Folder to a Local Zip File
#'
#' Zip cloud folder and download to local zip file. Default file name:
#' \code{<backup_dir>/cloud-backup_<base_name>_<yyyy-mm-dd-HHMM>.zip}
#'
#' @param path path to folder on the cloud
#' @param target_file path to the target file. A default path is provided by
#'   \code{get_download_target_file(base_name, backup_dir)}
#' @param backup_dir directory into which to download the zip-file
#' @param base_name base name to be integrated into the name of the downloaded
#'   file
#' @inheritParams list_files
#' @importFrom utils write.csv2
#' @importFrom kwb.utils catAndRun createDirectory
#' @export
#'
backup_cloud_folder <- function(
  path,
  target_file = get_download_target_file(base_name, backup_dir),
  user = nextcloud_user(),
  auth = nextcloud_auth(),
  backup_dir = kwb.utils::createDirectory("~/../Downloads/nextcloud-backups"),
  base_name = basename(path)
)
{
  stopifnot(is.character(path), length(path) == 1L)
  stopifnot_folder(path, user, auth)

  # Get fully recursive file information
  file_info <- list_files(path, recursive = TRUE, full_info = TRUE)
  metadata_file <- gsub("\\.zip$", "_meta.csv", target_file)

  kwb.utils::catAndRun(
    paste("Writing metadata to", metadata_file),
    utils::write.csv2(file_info, metadata_file, row.names = FALSE)
  )

  # Download all contents to a zip file
  download_from_href(href = get_download_href(path), target_file, auth)

  structure(target_file, metadata = metadata_file)
}

# get_download_href ------------------------------------------------------------
get_download_href <- function(path)
{
  paste(
    "index.php/apps/files/ajax/download.php",
    get_download_parameter_string(path),
    sep = "?"
  )
}

# get_download_parameter_string ------------------------------------------------
get_download_parameter_string <- function(path)
{
  dir_file <- kwb.file::split_into_dir_and_file(path)

  get_url_parameter_string(
    dir = dir_file$directory,
    files = dir_file$file,
    downloadStartSecret = get_download_start_secret()
  )
}

# get_download_start_secret ----------------------------------------------------
get_download_start_secret <- function(n_char = 32L)
{
  charset <- c(LETTERS, letters, as.character(0:9))
  paste(collapse = "", sample(charset, n_char, replace = TRUE))
}

# get_url_parameter_string -----------------------------------------------------
get_url_parameter_string <- function(...)
{
  x <- list(...)

  paste0(names(x), "=", as.character(x), collapse = "&")
}

# get_download_target_file -----------------------------------------------------
get_download_target_file <- function(
  base_name = "", backup_dir = "~/../Downloads"
)
{
  backup_filename <- sprintf(
    "cloud-backup_%s_%s.zip", base_name, format(Sys.time(), "%Y-%m-%d-%H%M")
  )

  file.path(backup_dir, backup_filename)
}
