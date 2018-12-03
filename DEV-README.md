# sos4R developer documentation

## Requirements

* **System packages (e.g. ubuntu 18.04)**
    * r-base-dev
    * libxml2-dev
    * libgdal1-dev
    * libproj-dev
    * libgeos-dev
    * texinfo
    * texinfo-doc-nonfree
    * texlive
    * texlive-base
    * texlive-fonts-recommended
    * texlive-fonts-recommended-doc
    * texlive-generic-recommended
    * texlive-latex-base
    * texlive-latex-base-doc
    * texlive-latex-extra
    * texlive-latex-recommended-doc
    * texlive-pstricks
* **R packages** (sync with `NAMESPACE`)
    * devtools
    * roxygen2
    * testthat
    * knitr
    * rgdal
    * XML
    * RCurl
    * sp
    * CATools
    * rmarkdown
    * maps
    * mapdata
    * maptools
    * XML
    * rgeos
    * spacetime
    * gstat
    * cshapes
    * exts


## Package structure

sos4R follows the regular R extension package structure. General documentation about R package development can be found at the following two websites.

* R extension packages by Hadley: http://r-pkgs.had.co.nz/ (well written with tight coupling to devtools and RStudio)
* R extension manual: http://cran.r-project.org/doc/manuals/R-exts.html (generic, extensive)

## /R

The actual source files in the ``/R`` directory follow a naming schema:

* ``Class-XXX.R`` contain class definitions, which are grouped by OGC specification namespace
  * GML
  * OGC
  * OM
  * OWS
  * SA
  * SML
  * SOS, additionally split up into versions
  * SWE
  * WaterML
* ``constants.R`` contains ... constants! No XML element name or parameters string shall be specified outside of this class
* ``defaults.R`` contains default settings and values, including parsers, encoders, etc.
* ``Generic-methods.R`` contains _all_ generic method definitions
* ``XXX-methods.R`` contains methods and used (helper or delegate) functions grouped by OGC specification namespace for...
  * creation methods
  * encoding methods
* ``XXX-methods-parsing.R`` contains parsing methods grouped by OGC specification namespace 
* ``XXX-methods-coercion.R`` contains coercion methods grouped by OGC specification namespace to map from sos4R's classes to R objects
* ``XXX-methods-accessor.R`` and ``XXX-methods-util.R`` contains accessor functions (which should be used over direct slot access) or utility functions (which are not well defined what they are in contrast to non-utility functions...) grouped by OGC specification namespace
* ``PrintShowStructureSummary-methods.R`` contains just that


## /sandbox

In addition to the regular directories, the ``/sandbox`` folder contains a wild list of R scripts with tests and demos etc.

## Building the vignette locally

To build the vignette locally, you can execute the following calls on the R command line:

```r
setwd("./vignettes")
Sweave(file = "sos4R.Rnw")
```

Or simply click on the "CompilePDF" button in RStudio

### The ``.goOnline`` option

To reduce download times while building the vignette it can store the responses of the required requests in local files and create the vignette based on these files. By the default, data is downloaded from the 52°North testbed SOS, and this must be done at least once after checking out the project. For this you must set your working directory to ``sos4R/vignettes``:

```r
setwd("./vignettes")
```

Afterwards, you can deactivate download and thereby activate using the local files at the beginning of the vignette:

```tex
<<options,echo=FALSE,results=hide,print=FALSE>>= 
.goOnline <- TRUE # triggers whether data is downloaded and stored in /vignettes folder
.verbose <- FALSE
@

```

## Classes

Data models, i.e. requests and responses, are modelled as **S4 classes**. Documentation can be found at the following sites (and others):

* The S4 object system: http://adv-r.had.co.nz/S4.html (short, must read)
* A (Not So) Short Introduction to S4: https://cran.r-project.org/doc/contrib/Genolini-S4tutorialV0-5en.pdf (not so short)
* Classes and Methods in the S Language: http://www.omegahat.org/RSMethods/Intro.pdf (by Chambers!)

## Tests



## Add features

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