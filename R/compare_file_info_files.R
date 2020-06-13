# compare_file_info_files ------------------------------------------------------

#' Compare Two Files Containing File Information
#'
#' Compare two CSV files as retrieved by list_files(..., full_info = TRUE)
#'
#' @param file_1 path to first csv file
#' @param file_2 path to second csv file
#' @param \dots further arguments passed to \code{\link[utils]{read.csv2}}
#' @importFrom utils read.csv2
#' @export
#'
compare_file_info_files <- function(file_1, file_2, ...)
{
  read_info <- function(file) kwb.utils::selectColumns(
    utils::read.csv2(file, stringsAsFactors = FALSE, ...),
    c("fileid", "file", "etag", "lastmodified")
  )

  diff_file_infos(
    file_info_1 = read_info(file_1),
    file_info_2 = read_info(file_2)
  )
}

# diff_file_infos --------------------------------------------------------------
diff_file_infos <- function(file_info_1, file_info_2)
{
  given_and_not_equal <- function(a, b) {
    ! is.na(a) & ! is.na(b) & a != b
  }

  get_status <- function(file_1, file_2, etag_1, etag_2) {
    status <- rep("unchanged", length(file_1))
    status[is.na(file_1)] <- "added"
    status[is.na(file_2)] <- "deleted"
    status[given_and_not_equal(etag_1, etag_2)] <- "modified"
    status[given_and_not_equal(file_1, file_2)] <- "renamed"
    status
  }

  res <- merge(
    file_info_1,
    file_info_2,
    by = "fileid",
    suffixes = c("_1", "_2"),
    all = TRUE
  )

  res$status <- get_status(res$file_1, res$file_2, res$etag_1, res$etag_2)

  res <- kwb.utils::removeColumns(res, c("etag_1", "etag_2"))

  res <- kwb.utils::moveColumnsToFront(res, c(
    "fileid", "status", "file_1", "file_2"
  ))

  res[res$status != "unchanged", ]
}
