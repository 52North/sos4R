
<!-- README.md is generated from README.Rmd. Please edit that file!
     Also see the README guidelines at https://wiki.52north.org/Documentation/GitHubProjectDocumentation -->

# sos4R

<!-- badges: start -->

[![cran
checks](https://cranchecks.info/badges/summary/sos4R)](https://cran.r-project.org/web/checks/check_results_sos4R.html)
[![Build
Status](https://travis-ci.org/52North/sos4R.png)](https://travis-ci.org/52North/sos4R)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/52North/sos4R?branch=master&svg=true)](https://ci.appveyor.com/project/52North/sos4R)
[![CircleCI build
status](https://circleci.com/gh/52North/sos4R.svg?style=svg)](https://circleci.com/gh/52North/sos4R)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Launch Rstudio
Binder](http://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/52North/sos4R/master?urlpath=rstudio)
<!-- badges: end -->

## Description

### R client for OGC SOS

**Easing the access to environmental time series data from an OGC Sensor
Observation Service**

sos4R is an extension package of the R environment for statistical
computing and visualization. sos4R is [available on
CRAN](https://CRAN.R-project.org/package=sos4R). The user can use it to
query data from standardized SOS instances (with specific consideration
of the OGC SOS 2.0 Hydrology Profile) using simple R function calls. It
provides a convenience layer for R users to integrate observation data
from SOS servers compliant with the SOS standard without any knowledge
about the underlying OGC Sensor Web Enablement standards.

### Features

**Key Technologies**

  - R Programming Language
  - OGC Sensor Observation Service 2.0
  - OGC WaterML 2.0
  - OGC Observations and Measurements 2.0

**Benefits**

  - Easier access to environmental time series data
  - Automatic generation of SOS requests
  - Result handling and parsing into standard R objects
  - Convenience API: Data Science oriented functions to load data

## Quick start

``` r
install.packages("sos4R")
library("sos4R")

# connect to SOS and request metadata
mySos <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/sos",
             binding = "KVP", useDCPs = FALSE, version = "2.0.0")

# retrieve available phenomena and sites
phenomena <- phenomena(sos = mySos, includeSiteId = TRUE, includeTemporalBBox = TRUE)
phenomena
siteList(sos = mySos)

# retrieve data
getData(sos = mySos,
        phenomena = phenomena[5,1],
        sites = phenomena[5,2],
        begin = as.POSIXct("2018-01-01"),
        end =   as.POSIXct("2018-01-3"))
```

**Install the development version**

You can install the current development version (= the next release for
CRAN) directly from GitHub:

``` r
#install.packages("remotes")
remotes::install_github("52North/sos4R", ref = "dev")
```

## User guide

The project website is at <https://52north.github.io/sos4R>. You can
also join the chat at [on Gitter](https://gitter.im/52North/sos4R) if
you have any questions.

## Demo

sos4R can used to download observation data or build user interfaces for
requesting and rendering time series observations. The screenshot below
shows a demo Shiny application, which you can run locally with

``` r
shiny::runApp(appDir = file.path(path.package("sos4R"), "shiny"))
```

![sos4R Shiny
app](https://blog.52north.org/wp-content/uploads/sites/2/2020/04/sos4r-vignette-10-egu-2020_shiny-app.jpg)

## Changelog

The latest changes, updates, bug fixes can be found in the package
changelog at <https://52north.github.io/sos4R/news/>.

## References

sos4R is used in the following projects.

  - MuDak-WRM, <https://www.mudak-wrm.kit.edu/>
  - NIWA, <https://niwa.co.nz/> (NIWA presented the project results at a
    dedicated [workshop at the FOSS4G SOTM
    Oceania 2019](https://2019.foss4g-oceania.org/schedule/2019-11-12?sessionId=NNWXKL))

## Credits

  - [@nuest](https://github.com/nuest)
  - [@edzer](https://github.com/edzer)
  - [@BenGraeler](https://github.com/BenGraeler)
  - [@bpross-52n](https://github.com/bpross-52n)
  - [@EHJ-52n](https://github.com/EHJ-52n)

sos4R is a project of [52°North Initiative for Geospatial Open Source
Software](https://52north.org).

[![](https://52north.org/wp-content/uploads/2016/06/logo-main.png)](https://52north.org)

### Funding

|                                                                                                                                                                                                                                                                                                                                                                                                Project/Logo                                                                                                                                                                                                                                                                                                                                                                                                | Description                                                                                                                                                                                                                                                                                                                                     |
| :--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------: | :---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| <a target="_blank" href="https://bmbf.de/"><img alt="BMBF" align="middle" width="100" src="https://raw.githubusercontent.com/52North/sos/develop/spring/views/src/main/webapp/static/images/funding/bmbf_logo_neu_eng.png"/></a><a target="_blank" href="https://www.fona.de/"><img alt="FONA" align="middle" width="100" src="https://raw.githubusercontent.com/52North/sos/develop/spring/views/src/main/webapp/static/images/funding/fona.png"/></a><a target="_blank" href="http://www.mudak-wrm.kit.edu/"><img alt="Multidisciplinary data acquisition as the key for a globally applicable water resource management (MuDak-WRM)" align="middle" width="100" src="https://raw.githubusercontent.com/52North/sos/develop/spring/views/src/main/webapp/static/images/funding/mudak_wrm_logo.png"/></a> | The development of this version of sos4R was supported by the <a target="_blank" href="https://www.bmbf.de/"> German Federal Ministry of Education and Research</a> research project <a target="_blank" href="http://www.mudak-wrm.kit.edu/">MuDak-WRM</a> (co-funded by the German Federal Ministry of Education and Research, programme FONA) |

### Contact

## Contribute

See file `DEV-README.md` for developer documentation or read the
developer documentation online at
<https://52north.github.io/sos4R/DEV-README.html>.

## License

This R extension package is licensed under [GPL
v2.0](https://tldrlegal.com/license/gnu-general-public-license-v2).

Documentation (e.g. vignette) is published under [CC
BY 4.0](http://creativecommons.org/licenses/by/4.0/).

sos4R is a project of [52°North Initiative for Geospatial Open Source
Software](https://52north.org).
