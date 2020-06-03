kwb.utils::assignPackageObjects("kwb.nextcloud")

file_info <- exclude_directories(list_files("documents", full_info = TRUE))

file_info$file

file <- file.path(tempdir(), "testfile.txt")

writeLines("Hi folks version 1!", file)
readLines(file) # target_path = "documents" by default
upload_file(file)

writeLines("Hi folks version 2!", file)
readLines(file)
upload_file(file, "documents")

writeLines("Hi folks version 3!", file)
readLines(file)
upload_file(file, "documents")

arguments <- list(
  file = "~/../Downloads/model-video/modelvideo_excelcursor.MOV",
  target_path = "documents"
)

do.call(upload_file, arguments)

# Show the versions
file_info <- exclude_directories(list_files(
  arguments$target_path, "MOV$", full_info = TRUE
))

stopifnot(identical(file_info$file, "modelvideo_excelcursor.MOV"))

version_info <- get_one_version_info(file_info$fileid)

# Download all versions
download_files(version_info$href)

# Delete a specific version? This seems to be not working
nextcloud_request(version_info$href[1], "DELETE") # -> error

# Delete the file
# I am using the string constant instead of "href" to see what I do!
nextcloud_request(
  "/remote.php/dav/files/hsonne/documents/test-2.txt",
  "DELETE"
)

nextcloud_request(
  "/remote.php/dav/files/hsonne/documents/modelvideo_excelcursor.MOV",
  "DELETE"
)

# File deleted? Yes!
list_files("documents")

# All versions deleted? Yes!!!
get_version_info(file_info$fileid) # -> error
