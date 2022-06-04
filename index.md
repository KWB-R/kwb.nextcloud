[![R-CMD-check](https://github.com/KWB-R/kwb.nextcloud/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.nextcloud/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.nextcloud/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.nextcloud/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.nextcloud/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.nextcloud)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.nextcloud)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.nextcloud)](https://kwb-r.r-universe.dev/)

R package to access file infos or download data
from an Nextcloud instance using the WebDav API
(https://docs.nextcloud.com/server/17/developer_manual/client_apis/WebDAV/).

## Installation

For installing the latest release of this R package run the following code below:

```r
# Enable repository from kwb-r
options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
  
# Download and install kwb.nextcloud in R
install.packages('kwb.nextcloud')

# Browse the kwb.nextcloud manual pages
help(package = 'kwb.nextcloud')
```
