### How to build an R package from scratch

usethis::create_package(".")
fs::file_delete(path = "DESCRIPTION")


# author <- list(name = "Hauke Sonnenberg",
#                orcid = "0000-0001-9134-2871",
#                url = "https://github.com/hsonne")

author <- list(name = "Michael Rustler",
                orcid = "0000-0003-0647-7726",
                url = "https://mrustl.de")

pkg <- list(name = "kwb.nextcloud",
            title = "R Package For Accessing Nextcloud Using WebDAV API",
            desc  = paste("R package to access file infos or download data from",
                          "an Nextcloud instance using the WebDav API",
                          "(https://docs.nextcloud.com/server/17/developer_manual/client_apis/WebDAV/)."))


kwb.pkgbuild::use_pkg(author,
                      pkg,
                      version = "0.0.0.9000",
                      stage = "experimental")


usethis::use_vignette("Tutorial")

### R functions


kwb.pkgbuild::use_autopkgdown("kwb.nextcloud")

kwb.pkgbuild::create_empty_branch_ghpages("kwb.nextcloud")
