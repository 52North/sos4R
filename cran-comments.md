# Test environments

* local R 3.5.3
* Ubuntu 14.04 (Trusty) on Travis CI
* Windows on Appveyor
* R-Hub
* win-builder (using devtools)

## R CMD check results

No errors.
No warnings.

1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Daniel Nuest <daniel.nuest@uni-muenster.de>'

New submission

Package was archived on CRAN

Possibly mis-spelled words in DESCRIPTION:
  Geospatial (36:31)
  OGC (3:67, 36:54)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2018-06-17 as check problems were not
    corrected despite reminders.

## Other

The package was "archived on 2018-06-17 as check problems were not corrected despite reminders."
Issues could now finally be resolved with a complete overhaul of implementation (switching from XML to xml2) and we kindly request re-publication on CRAN.

Comments during manual inspection were handled with this resubmission.
