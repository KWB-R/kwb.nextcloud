# [kwb.nextcloud 0.4.0](https://github.com/KWB-R/kwb.nextcloud/releases/tag/v0.4.0) <small>2024-10-15</small>

* Add `file_exists()`
* Add `get_file_versions()`
* Fix bug in `path_to_file_href()` that occurred when a user name contained 
  spaces. Document and export this function.
* `download_files()`: Add argument `dbg`
* `get_version_info()`: Add argument `dbg`
* Deprecate `list_file_versions()`

# [kwb.nextcloud 0.3.0](https://github.com/KWB-R/kwb.nextcloud/releases/tag/v0.3.0) <small>2021-06-03</small>

* Add `move_file_or_folder()`: see [Moving files and folders](https://docs.nextcloud.com/server/latest/developer_manual/client_apis/WebDAV/basic.html#moving-files-and-folders-rfc4918)
* Fix to remove leading slashes in internal function `path_to_file_href()` 

# [kwb.nextcloud 0.2.0](https://github.com/KWB-R/kwb.nextcloud/releases/tag/v0.2.0) <small>2021-04-01</small>

* Fix bug in list_files(): endless recursion (#32)
* Add `compare_file_info_files()` (from R package kwb.budget) 
* CI: move from Travis and Appveyor to Github actions (#35)

# [kwb.nextcloud 0.1.0](https://github.com/KWB-R/kwb.nextcloud/releases/tag/v0.1.0) <small>2020-06-04</small>
Exported functions:

* create_folder()
* delete_file_or_folder(), TAKE CARE!
* download_files()
* get_version_info()
* list_files()
* upload_file()

# kwb.nextcloud 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.

* see https://style.tidyverse.org/news.html for writing a good `NEWS.md`


