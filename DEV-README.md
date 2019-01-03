# sos4R developer documentation

This file contains information for developers of the [R](http://r-project.org/) package `sos4R`.
Documentation for usres can be found in the package's vignette (see `browseVignettes("sos4R")`).

<!-- part of this page previously published at https://wiki.52north.org/Geostatistics/Sos4R -->

## Contribute

Please get in touch with the [community contact](https://52north.org/research/rd-communities/geostatistics/)
of the geostatistics community and read the [Get Involved page](https://52north.org/software/get-involved/) if you want to *become a contributor* -- contributions are welcome!

Feature ideas (as issues tagged "enhancement"), roadmap (as milestones) and tasks (issues in the current milestone) as as well as bugs (tagges as "bugs") are managed on GitHub: https://github.com/52North/sos4R/issues.

## Versions and branches

The package uses [semantic versioning](https://semver.org/).
The current development version should start with `.9000` _after_ the patch version, see [R packages versioning](http://r-pkgs.had.co.nz/description.html#version).
The development version can be increased _extensively_, e.g. with each non-trivial commit.
The version must be changed in the files `DESCRIPTION` and `man/sos4R-package.Rd` (which is important for releases).

Bugfix releases can be used extensively (for several, or even just one bug) to show users that errors are resolved.
Minor versions shall include several major bug fixes or a considerable addition of tested and working functionality.
Code changes that possibly break existing code shall only be released with a major version change and should be discussed in the developer community beforehand.

You can install the current development version or local branches using respective functions of [`devtools`](https://devtools.r-lib.org/).

We use the ["fork & pull" development model](https://en.wikipedia.org/wiki/Fork_and_pull_model).
The **`master` branch** is up to date with the version on CRAN.
Bugfixes should be based on this branch.
If you want to develop further functions for the package always use the up-to-date version from the main repository's **`dev` branch** and preferably start a new branch in your fork.

## Releases

A new release shall be uploaded to CRAN after testing and under the following procedure:

- Run [tests](#tests)
- Update version and date in `man/sos4R-package.Rd`
- Update version in `DESCRIPTION`
- Create tag with version number
- Update NEWS file based on latest commits
- Read http://cran.r-project.org/web/packages/policies.html again
- Apply http://r-pkgs.had.co.nz/release.html#release-check
- When available on CRAN
  - Notice on 52N mailing list(s)
  - Publication on blog
  
The following code snippets can make sure everything runs smoothly.
On the command line:

```bash
R CMD check --as-cran
```

In R, use

```r
devtools::build_win()
revdepcheck::revdep_check()
```

You then should do the actual release with

```r
devtools::release()
```

## Documentation

### News

The latest changes for every version are documented in the file `NEWS.md` in
the package root directory.

### R function documentation

Package documentation is based on `.Rd` files (regarding switch to `roxygen` see [https://github.com/52North/sos4R/issues/21](#21)).
The file `NAMESPACE` is _not_ managed by `roxygen2` either.

### Update website

Knit `README.Rmd` to create `README.md`.

Run [pkgdown](https://pkgdown.r-lib.org/) from the package directory each time you release your package:

```r
pkgdown::build_site()
```

### The ``.goOnline`` option in vignettes

To reduce download times while building the vignette it can store the responses of the required requests in local files and create the vignette based on these files. By the default, data is downloaded from the 52°North testbed SOS, and this must be done at least once after checking out the project. For this you must set your working directory to ``sos4R/vignettes``:

```r
setwd("./vignettes")
```

Afterwards, you can deactivate download and thereby activate using the local files at the beginning of the vignette:

```r
# triggers whether data is downloaded and stored in /vignettes folder
.goOnline <- TRUE
.verbose <- FALSE
```

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
* `Generic-methods.R` contains _all_ generic method definitions
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
* ``PrintShowStructureSummary-methods.R`` contains just that

### /sandbox

In addition to the regular directories, the ``/sandbox`` folder contains a wild list of R scripts with tests and demos etc.
Code in this directory is not exported from the package but is used during development to test during the implementation of (new) functionality.
Please consider using this extensively as a history to be able to resolve problems that occurred before and to document what is working and what not.
It is also recommended to run related tests again after (even minor) changes in the code.

* `testing.R` contains tests done during the development of certain functions and is a very good opportunity to (re-)check functionality and keep a history of functionality that is working (at some point in time at least...).
* `testing-SOSs.R` contains tests of connections to different types of SOS and different SOS instances.
* `packaging.R` contains the functions for creating package documentation and should be used as the spot to collect useful functions and workflows around future documentation and the packaging process.

### Naming of functions, defaults, and constants

The following guidelines are a non extensive list of naming rules that were used within the package.
Please also browse through the code files before starting to develop new functions to get to know the structures that are already in place so that a good user experience can be ensured.

- **Defaults**: _sosDefaultXYZ_, e.g. `sosDefaultCharacterEncoding`
- **Defaults with accessor function:** Follows the same rules, but add a point to the actual variable so that it is not exported and start the function with a capital letter, e.g. `.sosDisabledParsers` and `SosDisabledParsers()`
- **Constants:** Constants for names of XML elements start with a lowercase character string of the namespace prefix, a unique name of the element (where parts like "type" and special characters may be left out, and other descripte elements may be added for clarity), and end with _Name_, e.g. `gmlEnvelopeName <- "Envelope"`, `ogcGeometryOperandLineStringName <- "gml:LineString"`, or `ogcTempOpTMEqualsName <- "TM_Equals"`.
- **Supported features:** The supported features, like connection methods and supported response modes, shall be accessible by functions starting with SosSupported, e.g. `SosSupportedConnectionMethods()`.
- **Accessor functions:** Shall start with _sos_, e.g. `sosOfferings(sos:SOS)`.
- **Functions to access default values**, especially lists and functions with merging feature: Shall start with _Sos_, e.g. `SosEncodingFunctions()`.

## Classes

Data models, i.e. requests and responses, are modelled as **S4 classes**. Documentation can be found at the following sites (and others):

* The S4 object system: http://adv-r.had.co.nz/S4.html (short, must read)
* A (Not So) Short Introduction to S4: https://cran.r-project.org/doc/contrib/Genolini-S4tutorialV0-5en.pdf (not so short)
* Classes and Methods in the S Language: http://www.omegahat.org/RSMethods/Intro.pdf (by Chambers!)

## Troubleshooting

- "non-ASCII characters" > find them with `tools::showNonASCIIfile()`

## Tests

Tests are implemented with [`testthat`](http://testthat.r-lib.org/).
Run them with

```r
devtools::test()
```

or by clicking the "Check" button in RStudio (which does more than just running the tests!).

## Add features

See [Versions and Branches](#versions-and-branches) for information about the release flow.
In your new feature branch, implement the feature.
Add tests.

### How to add a parser

* by element name
* by mime type

```r
SosParsingFunctions()
# add a local parser
SosParsingFunctions("TimeseriesKVP" = function() { ... })
```

### How to add an encoder

...
