kwb.utils::assignPackageObjects("kwb.nextcloud")

file_info <- exclude_directories(
  list_files("documents", "txt$", full_info = TRUE)
)

nrow(file_info)

path <- "documents/test-1.txt"
href <- path_to_file_href(path)

nextcloud_request(href, "PUT", body = "Hi folks version 1!")
nextcloud_request(href, "PUT", body = "Hi folks version 2!")
nextcloud_request(href, "PUT", body = "Hi folks version 3!")

file_info <- exclude_directories(
  list_files(dirname(path), "txt$", full_info = TRUE)
)

stopifnot(identical(file_info$file, "test-1.txt"))

version_info <- get_version_info(file_info$fileid)

# Download all versions
download_files(version_info$href)

# Delete a specific version? This seems to be not working
nextcloud_request(version_info$href[1], "DELETE") # -> error

# Delete the file
# I am using the string constant instead of "href" to see what I do!
nextcloud_request("/remote.php/dav/files/hsonne/documents/test-1.txt", "DELETE")

# File deleted? Yes!
list_files("documents")

# All versions deleted? Yes!!!
get_version_info(file_info$fileid) # -> error
