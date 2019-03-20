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


## Requirements

* **System packages (e.g. ubuntu 18.04)**
    * r-base-dev
    * libxml2-dev
    * libgdal-dev
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
* **R packages** (see fields Depends and Suggests in `DESCRIPTION`)


## Package structure

sos4R follows the regular R extension package structure. General documentation about R package development can be found at the following two websites.

* R extension packages by Hadley: http://r-pkgs.had.co.nz/ (well written with tight coupling to devtools and RStudio)
* R extension manual: http://cran.r-project.org/doc/manuals/R-exts.html (generic, extensive)

## Important Folders

### /R

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


### /sandbox

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
