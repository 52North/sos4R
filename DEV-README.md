# sos4R developer documentation

<!-- TOC START min:2 max:4 link:true update:true -->
- [Requirements](#requirements)
- [Package structure](#package-structure)
- [Important Folders](#important-folders)
  - [/R](#r)
  - [/sandbox](#sandbox)
- [Building the vignette locally](#building-the-vignette-locally)
  - [The ``.goOnline`` option](#the-goonline-option)
- [Classes](#classes)
- [Tests](#tests)
- [Using docker](#using-docker)
- [Add features](#add-features)
  - [How to add a parser](#how-to-add-a-parser)
  - [How to add an encoder](#how-to-add-an-encoder)

<!-- TOC END -->

<!-- part of this page previously published at https://wiki.52north.org/Geostatistics/Sos4R -->

This file contains information for developers of the [R](http://r-project.org/) package `sos4R`.
Documentation for users can be found in the package's vignette (see `browseVignettes("sos4R")`).

## Requirements

* **System packages (e.g. on Ubuntu 18.04)**
    * r-base-dev
    * libxml2-dev
    * libgdal-dev
    * libproj-dev
    * libgeos-dev
    * texinfo
    * texinfo-doc-nonfree
    * texlive ([TinyTex is recommended!](https://yihui.org/tinytex/))
    * texlive-base
    * texlive-fonts-recommended
    * texlive-fonts-recommended-doc
    * texlive-generic-recommended
    * texlive-latex-base
    * texlive-latex-base-doc
    * texlive-latex-extra
    * texlive-latex-recommended-doc
    * texlive-pstricks
* **R packages** (see fields Depends and Suggests in `DESCRIPTION`)

## Contribute

Please get in touch with the [community contact](https://52north.org/research/rd-communities/geostatistics/)
of the geostatistics community and read the [Get Involved page](https://52north.org/software/get-involved/) if you want to *become a contributor* -- contributions are welcome!

You can also dive right in and join the chat room: [![Join the chat at https://gitter.im/52North/sos4R](https://badges.gitter.im/52North/sos4R.svg)](https://gitter.im/52North/sos4R)

------

## Development, versions, and branches

- Roadmap and release planning is done in [projects](https://github.com/52North/sos4R/projects).
- The **`master` branch** is up to date with the version on CRAN. Bugfixes should be based on this branch.
- Tasks are developed in [task or feature branches](https://trunkbaseddevelopment.com/short-lived-feature-branches/) based on the **`dev` branch**. Pull requests are sent from the features branches to the dev branch once they are complete.
- Feature ideas as issues tagged ["enhancement"](https://github.com/52North/sos4R/issues?q=is%3Aissue+is%3Aopen+label%3Aenhancement).
- Bugs are issues tagged as ["bugs"](https://github.com/52North/sos4R/issues?q=is%3Aissue+is%3Aopen+label%3Abug).
- The package uses [**semantic versioning**](https://semver.org/).
  - The current development version should start with `.9000` _after_ the patch version, see [R packages versioning](http://r-pkgs.had.co.nz/description.html#version).
  - The development version can be increased _extensively_, e.g. with each non-trivial commit.
  - The version must be changed in the files `DESCRIPTION` and `man/sos4R-package.Rd` (which is important for releases).
  - Bugfix versions can be used extensively (for several, or even just one bug) to show users that errors are resolved.
  - Minor versions shall include several major bug fixes or a considerable addition of tested and working functionality.
  - Code changes that possibly break existing code shall only be released with a major version change and should be discussed in the developer community beforehand.

Users may install the current development version or local branches using respective functions of [`devtools`](https://devtools.r-lib.org/).

------

## Documentation

### News

The latest changes for every version are documented in the file `NEWS.md` in
the package root directory.
Open a preview with `pkgdown::build_news()`.

### R function documentation

Package documentation is based on `.Rd` files (regarding switch to `roxygen` see [https://github.com/52North/sos4R/issues/21](#21)).
The file `NAMESPACE` is _not_ managed by `roxygen2` either.

### Update website

Knit `README.Rmd` to create `README.md`.

Run [pkgdown](https://pkgdown.r-lib.org/) from the package directory each time you release your package:

```r
pkgdown::build_site()
```

### Build vignettes

The vignettes should build as part of the docs website or package check process.
To build single vingettes manually you can use

```r
rmarkdown::render("vignettes/sos4R-vignette-01-quickstart.Rmd")
```

or

```r
pkgdown::build_article("sos4R-vignette-01-quickstart")
```

------

## Package structure

sos4R follows the regular R extension package structure.
General documentation about R package development can be found at the following two websites.

* R extension packages by Hadley: http://r-pkgs.had.co.nz/ (well written with tight coupling to devtools and RStudio)
* R extension manual: http://cran.r-project.org/doc/manuals/R-exts.html (generic, extensive)

### /R

The actual source files in the ``/R`` directory follow a naming schema:

* ``Class-[NamespacePrefix].R`` files contain class definitions, which are grouped by OGC specification namespace, like classes for XML types in `Class-GML.R`, but also generic classes as in `Class-SOS.R`.
  * [GML](https://en.wikipedia.org/wiki/Geography_Markup_Language)
  * [OGC](http://www.opengeospatial.org/ogcna)
  * [OM](https://en.wikipedia.org/wiki/Observations_and_Measurements) (and SAMS - spatial sampling, and SA - sampling)
  * [OWS](https://www.opengeospatial.org/standards/owc)
  * [SML](https://en.wikipedia.org/wiki/SensorML
  * SOS, additionally split up into versions
  * [SWE](https://en.wikipedia.org/wiki/Sensor_Web_Enablement)
  * [WML](https://en.wikipedia.org/wiki/WaterML)
* `constants.R` contains ... constants! No XML element name or parameters string shall be specified outside of this class; constants shall not be changed by the user. Please reconsider every time you write a character string in the code if it should not be a constant. Names of XML types and elements shall always be constants, attribute names can be constants. This file also contains the lists of supported features, like result models and connection methods. Use accessor functions where needed.
* `defaults.R` contains default settings and values, including parsers, encoders, etc., as well as accessor functions to the defaults
* `Generic-methods.R` contains _all_ generic method definitions _except_ the ones for accessor functions
* `[NamespacePrefix]-methods.R` contains methods and used (helper or delegate) functions grouped by OGC specification namespace for...
  * creation methods (constructor functions)
  * encoding methods
* `[NamespacePrefix]-methods-parsing.R` contains parsing methods grouped by OGC specification namespace; if there is a very limited number of parsing functions and the file is not very long, the parsing functions may also be in the file `[NamespacePrefix]-methods.R`.
* `[NamespacePrefix]-methods-coercion.R` contains coercion methods grouped by OGC specification namespace to map from sos4R's classes to R objects
* `[NamespacePrefix]-methods-accessor.R` and `[NamespacePrefix]-methods-util.R` contains accessor functions (which should be used over direct slot access) or utility functions (which are not well defined what they are in contrast to non-utility functions...) grouped by OGC specification namespace
* `SOS-methods.R` contains the functions for the operations, like getCapabilities(sos:SOS), and the function that actually does the sending of the request, sosRequest(...).
* `SOS-methods-accessor.R` contains accessor functions.
* `SOS-methods-util.R` contains a lot of convenience and accessor functions, and should be used to keep the file `SOS-methods.R` lucid.
* `SOS-methods-plotting.R` contains plotting functions.
* ``PrintShowStructureSummary-methods.R`` contains functions to override `print`, `str`, `summary` functions and the like; these may go in this file or also into the file where the respective class is defined.

### `/sandbox`

In addition to the regular directories, the ``/sandbox`` folder contains a wild list of R scripts with tests and demos etc.
Code in this directory is not exported from the package but is used during development to test during the implementation of (new) functionality.
Please consider using this extensively as a history to be able to resolve problems that occurred before and to document what is working and what not.
It is also recommended to run related tests again after (even minor) changes in the code.

* `testing.R` contains tests done during the development of certain functions and is a very good opportunity to (re-)check functionality and keep a history of functionality that is working (at some point in time at least...).
* `testing-SOSs(-2.0).R` contains tests of connections to different types of SOS and different SOS instances.

### Naming of functions, defaults, and constants

The following guidelines are a non extensive list of naming rules that were used within the package.
Please also browse through the code files before starting to develop new functions to get to know the structures that are already in place so that a good user experience can be ensured.

- **Defaults**: _sosDefaultXYZ_, e.g. `sosDefaultCharacterEncoding`
- **Defaults with accessor function:** Follows the same rules, but add a point to the actual variable so that it is not exported and start the function with a capital letter, e.g. `.sosDisabledParsers` and `SosDisabledParsers()`
- **Constants:** Constants for names of XML elements start with a lowercase character string of the namespace prefix, a unique name of the element (where parts like "type" and special characters may be left out, and other descripte elements may be added for clarity), and end with _Name_, e.g. `gmlEnvelopeName <- "Envelope"`, `ogcGeometryOperandLineStringName <- "gml:LineString"`, or `ogcTempOpTMEqualsName <- "TM_Equals"`.
- **Supported features:** The supported features, like connection methods and supported response modes, shall be accessible by functions starting with SosSupported, e.g. `SosSupportedConnectionMethods()`.
- **Accessor functions:** Shall start with _sos_, e.g. `sosOfferings(sos:SOS)`.
- **Functions to access default values**, especially lists and functions with merging feature: Shall start with _Sos_, e.g. `SosEncodingFunctions()`.

### Classes

Data models, i.e. requests and responses, are modelled as **S4 classes**. Documentation can be found at the following sites (and others):

* The S4 object system: http://adv-r.had.co.nz/S4.html (short, must read)
* A (Not So) Short Introduction to S4: https://cran.r-project.org/doc/contrib/Genolini-S4tutorialV0-5en.pdf (not so short)
* Classes and Methods in the S Language by Chambers: http://www.omegahat.org/RSMethods/Intro.pdf

## Troubleshooting

- "non-ASCII characters" in a file > find them with `tools::showNonASCIIfile()`

## Tests

Tests are implemented with [`testthat`](http://testthat.r-lib.org/).
Run them with

```r
devtools::test()
```

or by clicking the "Check" button in RStudio (which does more than just running the tests!).

## Using docker

In the [docker](docker/) folder, a Dockerfile is provided that can be used to
set-up an isolated container just for sos4R development. This includes all
required dependencies, RStudio as webapplication, and devtools. The current
size of the image is ~4.02GB.

1. Clone this repository.

1. Change to the `docker` subfolder in any terminal of your choice.

1. Perform the following command to build the image locally:<br />
   `docker build -t sos4r-rstudio-dev:$(date +%Y-%m-%d) .`.<br />
   *On windows*, you need to replace `$(date +%Y-%m-%d)` with something useful,
   like `2019-02-27`.

1. Start the image as new container using the following command:
```
docker run --name=sos4r-dev --env PASSWORD=r --publish 8787:8787 --volume /YOUR_PATH_TO/sos4R/:/home/rstudio/sos4R -d sos4r-rstudio-dev:2019-02-27
```
You can start and stop the container by its name `sos4r-dev`.

1. Point your browser to [http://localhost:8787/](http://localhost:8787/).

1. Login with **Username** `rstudio` and **Password** `r`.<br />
   If the password is not secure enough, please delete the container
  via `docker stop sos4r-dev; docker rm sos4r-dev` and re-run the
  above `docker run ...` with a different value for `PASSWORD`.

1. Open the sos4R project via menu `File` &rarr; `Open Project` &rarr; open folder `sos4R` &rarr; select `sos4R.Rproj`.

1. Start developing.

## Add features

See [Versions and Branches](#versions-and-branches) for information about the release flow.
In your new feature branch, implement the feature.
Add tests.

## Releases

A new release shall be uploaded to CRAN after testing and under the following procedure:

- Install the current version of the package from source
- Run the [tests](#tests) locally and resolve all problems
- Run a check locally, e.g. `devtools::check(document = FALSE)` and fix errors, warnings, and notes
- Update NEWS file based on latest commits, add missing changes/updates/notable things
- Update the `Date` field in the `DESCRIPTION` file to match the release date
- Create a PR from `dev` to `master` and check the CI status, fix all problems
  - https://travis-ci.org/52North/sos4R
  - https://ci.appveyor.com/project/52North/sos4r
  - If there are problems with the `devel` release of R, you can use a local Docker container to run the checks while fixing them (from the path of the parten folder of `sos4R`): `docker run --rm -it -v $(pwd)/sos4R:/home/sos4R rocker/geospatial:devel bash`, switch to `/home`, then run the commands below (`R CMD build` & `check`)
- Update version and date in `man/sos4R-package.Rd`
- Update version in `DESCRIPTION`
- Update version number in `NEWS.md`
- Run **checks** again and fix all problems
  - In the parent directory of the project: `R CMD build sos4R; R CMD check --as-cran sos4R_<version number>.tar.gz` should have no errors, warnings, or notes
  - `revdepcheck::revdep_check()` for checking reverse dependencies
  - `devtools::check_win_release()` and `devtools::check_win_devel()` for testing Windows using CRAN infrastructure
  - `rhub::check_for_cran(email = <...>)` for testing for CRAN submissions using RHub infrastructure
  - (optional) `rhub::check(email = <...>)` for running checks on different operating systems
- Regenerate the website with `pkgdown::build_site()` and re-knit `README.Rmd`, commit changes
- Push changes to `dev` branch
- Read and follow http://cran.r-project.org/web/packages/policies.html and http://r-pkgs.had.co.nz/release.html#release-check again, make necessary changes
- Merge the PR
- Update your local `master` to upstream's `master` branch
-_ Do the actual release_ with `devtools::release()` (which will ask you again if you did many of the steps before)
- Wait for the good news, check where the package is in the queue with `foghorn::cran_incoming(pkg = "sos4R")`
- When available on CRAN:
  - Create a git tag with the version number using the letter `v` followed by the version number (see above, must match `DESCRIPTION`), e.g. `v1.2.3`, and push it to the main repository
  - Create a release on GitHub based on the new tag, named just as the version tag, see the file `CRAN-RELEASE` for the precise commit
  - Remove the file `CRAN-RELEASE`
  - Checkout the `dev` branch
  - Merge `upstream/master`
  - Update the version in `DESCRIPTION` to a new development version, e.g. from `0.3.0` to `0.4.0.9000`
  - Email on 52N mailing list(s) (coordinate with 52N staff)
  - Tweet
