# sos4R - R client for OGC SOS

sos4R is an extension for the R environment for statistical computing and visualization. It allows to query data from standard conform SOS instances using simple R function calls and does no require any knowledge about the Sensor Web. It is easily extendible for new data models and opens the huge amount of analysis and visualization features of the R environment for the Sensor Web.

Build status: [![Build Status](https://travis-ci.org/52North/sos4R.png)](https://travis-ci.org/52North/sos4R)

## Documentation & Links

* Vignette: http://cran.r-project.org/web/packages/sos4R/vignettes/sos4R.pdf
* CRAN: http://cran.r-project.org/web/packages/sos4R/
* Wiki: https://wiki.52north.org/bin/view/Geostatistics/Sos4R
* Support mailing list/forum: http://geostatistics.forum.52north.org/
* Ohloh: https://www.ohloh.net/p/sos4R
* Blog: http://www.nordholmen.net/sos4r/
* Source code: https://github.com/52North/sos4R

## Development

Branch status: [![Build Status](https://travis-ci.org/52North/sos4R.png?branch=dev)](https://travis-ci.org/52North/sos4R)

sos4R is developed on GitHub using the [fork & pull development model](https://help.github.com/articles/using-pull-requests/#fork--pull). The [main repository's issue tracker](https://github.com/52North/sos4R/issues) is used to coordinate development.

The `master` branch represents the current version that is [published on CRAN](http://cran.r-project.org/package=sos4R), the `dev` branch is the current development version.

### Install the development version

You can install the current development version (= the next release for CRAN) directly from GitHub with the following commands.
```r
install.packages("devtools")
devtools::install_github("52North/sos4R", ref = "dev")
# To also install the vignettes run: devtools::install_github("52North/sos4R", build_vignettes = TRUE)
```

### Contributors

* [@nuest](https://github.com/nuest)
* [@edzer](https://github.com/edzer)

### Contact

Do you have a question that is not anwsered in the links above? Contact Daniel: d.nuest@52north.org

## License

This R extension package is licensed under [GPL v2.0](https://tldrlegal.com/license/gnu-general-public-license-v2).

Documentation (e.g. vignette) is published under [CC BY 4.0](http://creativecommons.org/licenses/by/4.0/).
