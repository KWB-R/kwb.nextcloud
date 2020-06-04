# Make also private functions available
kwb.utils::assignPackageObjects("kwb.nextcloud")

# Are we the test user?
nextcloud_user()

# Ok, we seem to be the test user, let's mess around!

# List the files and folders on the main level
list_files()

# Get the file and folder properties
file_info <- list_files(full_info = TRUE)
file_info

# Show only the files
file_info <- exclude_directories(file_info)
file_info

# Relative paths to the files
file_info$file

# Download the files
download_files(file_info$href)

# Create a folder on the cloud
create_folder("testfolder-1")

# Did the folder arrive?
"testfolder-1/" %in% list_files()

# Path to a local temporary file
file <- file.path(tempdir(), "testfile.txt")

# Path to target folder on the cloud
target_path <- "testfolder-1"

# Upload first version of the file
writeLines("Hi folks version 1!", file)
readLines(file)
upload_file(file, target_path)

# Upload second version of the file
writeLines("Hi folks version 2!", file)
readLines(file)
upload_file(file, target_path)

# Upload third version of the file
writeLines("Hi folks version 3!", file)
readLines(file)
upload_file(file, target_path)

# Get the fileid of the uploaded file
file_info <- list_files(target_path, full_info = TRUE)

fileid <- file_info$fileid[file_info$file == "testfile.txt"]
fileid

# Show the versions of the file (identified by fileid)
version_info <- get_version_info(fileid)

# There are two versions (the current version is not shown!)
version_info

# Download all versions (unfortunately named by timestamp)
downloaded_files <- download_files(version_info$href)
downloaded_files

# Read the contents (should be version 1 and 2)
lapply(downloaded_files, readLines)

# Delete a specific version? This seems to be not working
nextcloud_request(version_info$href[1], "DELETE", really = really)

# Delete the file
# I am using the string constant instead of "href" to see what I do!
delete_file_or_folder("testfolder-1/testfile.txt", really = really)

# File deleted? Yes!
list_files("testfolder-1")

# All versions deleted? Yes!!!
get_version_info(fileid) # -> error

# Upload the testfile again
upload_file(file, target_path)

# Check for success
list_files(target_path)

# Now try to delete the whole directory
delete_file_or_folder("testfolder-1", really = really)

# Try to list again
list_files(target_path) # -> not found (any more)!

# List the upper level -> no more testfolder!
list_files()
