[![Appveyor build Status](https://ci.appveyor.com/api/projects/status/github/KWB-R/kwb.nextcloud?branch=master&svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-nextcloud/branch/master)
[![Travis build Status](https://travis-ci.org/KWB-R/kwb.nextcloud.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.nextcloud)
[![codecov](https://codecov.io/github/KWB-R/kwb.nextcloud/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.nextcloud)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.nextcloud)]()

R package to access file infos or download data
from an Nextcloud instance using the WebDav API
(https://docs.nextcloud.com/server/17/developer_manual/client_apis/WebDAV/).

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.nextcloud' from GitHub
remotes::install_github("KWB-R/kwb.nextcloud")
```
