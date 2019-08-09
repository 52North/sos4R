# sos4R 0.3.1

- Bugfix release: don't set classes for `xml2` (requires version `1.2.2`, see #148)

# sos4R 0.3.0

- Switch from `XML` to `xml2`
- Added a `NEWS.md` in Markdown format to track changes to the package (moved content from `inst/NEWS` and `inst/CHANGES`)
- Overhauled vignettes, now Markdown format
- Development version of SOS 2.0 support (KVP only)
- Extended developer documentation in `DEV-README.md`
- Drop deprecated bindings "GET" and "POST", now only "KVP" and "POX" are supported
- Add AppVeyor CI configuration
- Use R Markdown based README (`README.Rmd`)
- Combination of `verbose` and `inspect`: full requests and responses are only printed with `inspect = TRUE`
- Drop support for non-standard "latest" time request 

# sos4R 0.2-12

- Added 'additionalKVPs' to SOS(...) to add specific key-value-pairs for KVP requests
- NEW starting support for SOS 2.0 (pre-ALPHA, no implementation yet just classes and empty methods)
- Added handling of multiple DCPs for the same operation type using a regex list dcpFilter and parameter 'useDCPs' as a switch
- Added handling for time instances to sosCreateTime
- Added feature of interest for GET URL, using sosKVPParamNameFoi for naming the parameter
- Added support for "latest" time request in GET using sosDefaultGetBindingParamLatest (e.g. "time=latest" or "latest")
- Fixed bug in getObservation, improved handling for saveOriginal (if not provided for getObservation(byId))
- Fixed usage of .find.package, which is deprecated, for loading the cheat sheet and news/changes file (now using find.package)
- Fixed bug in mimeType definition with subtypes which were incorrectly encoded by RCurl by using different quotation: "text/xml;subtype=&quot;sensorML/1.0.1&quot;" > changed to: 'text/xml;subtype="sensorML/1.0.1"' (and similar for OM) 
- Changed name of bindings from GET to KVP and POST to POX. Warnings added, started renaming of "connection method" stuff to "binding" to reflect more widely used terms.
- Changed all GET request parameters to constant names (sosKVPParamNames...)
- Added parsing of CompositePhenomenon
  
# sos4R 0.2-11

- Fixed line lengths of \example and \usage sections in .Rd files
- Fixed usage of .find.packages which is deprecated for compatibility with R 3.0.0
- Changed minimum R version to 2.15.0 since older ones will not be tested anymore, and increased version of important dependencies RCurl and XML
  
# sos4R 0.2-10

- Added function sosCapabilitiesUrl to generate a capabilities request URL in KVP encoding to include in Documentation, e.g. Sweave
- Fixed rgdal dependency by adding it to suggests list in DESCRIPTION
- Fixed pegel demo, now using the actual PegelOnline web service
- Fixed a NOTE about partial argument match in getObservation,SOS_1.0.0 for 'offering' to 'offeringId': had to use the correct name offeringId when calling .getObservation_1.0.0(...)
- EXTENDED documentation: added reference to cheat sheet in Vignette, added subsection "Quick Start" to Introduction
  
# sos4R 0.2-9

- Added error handling for plotting methods so that plot(sos) does not break if one offering cannot be coerced to spatial
- Added 'ug/m3' and 'http://www.opengis.net/def/property/OGC/0/FeatureOfInterest' to known list of fields > converted with sosConvertDouble and sosConvertString respectively
- Fixed weathersos demo
- Moved vignette to now suggested directory vignettes/, see http://cran.r-project.org/doc/manuals/R-exts.html#Writing-package-vignettes
- Changed DESCRIPTION file: moved xtable to Suggests - section, set ByteCompile: TRUE, added Authors@R (http://journal.r-project.org/archive/2012-1/RJournal_2012-1_Hornik~et~al.pdf), changed minumum R version to 2.14.0

- Updated cheat sheet and vignette: added link to Bugzilla
- Added minor improvements for sensor description parsing and coecion: sosCoordinates(SensorML) tries to handle 'easting'/'northing', 'longitude'/'latitude', and 'elevation'/'altitude' coordinates and subsequently plotting does not break if 'x'/'y' are not given.
- Added function to reset the parsers to default: sos <- SosResetParsingFunctions(sos)
  
# sos4R 0.2-7

- Added describeSensor can now handle a character vector of procedures, will return a list with the sensor descriptions
- Added note in verbose mode about multiple content types in .getObservation_1.0.0
- Added handling of OM mime subtype  
- Added demo with EO2Heaven SOS, called "eo2heaven", deals with data download and plotting and some analysis
  
# sos4R 0.2-6

- Changed documentation of changes from CHANGES to this NEWS file
- Added function sosNews() to access this file from an R session
- Changed syntax in cheat sheet, not using the | character for alternatives.
- Added file in sandbox: useR-2011.R
- Change in parseSosObservationOffering: does not break anymore if procedures (though mandatory!) are missing
- Change in encodeXML: Now a specific function for objects of class POSIXt exists so that only the encoding of time can be overridden by users, this is demonstrated in demo("southesk") and documented in the vignette.
- Change in encodeKVP: Specific function for objects of class POSIXt, see above.

# sos4R 0.2-5

- Changed name Nuest again in citation file, one directly using the umlaut, another using N\\uest.
- Fix in as.SensorML.SpatialPointsDataFrame, now using a default CRS (EPSG 4326), if it cannot be detected.
- Fixed bug in sosBoundedBy(SensorML) which was always returning empty matrix for bounding box
- Bugfix release, and to see if NOTEs about uncompacted PDFs can be fixed by the according option in R CMD build
- Fix in latex file, misplaced "&"

# sos4R 0.2-3

- Bug in sosGetCRS, returns NULL and prints error if the EPSG code is not recognized
- Fixed non-ASCII characters, Umlaute, and incomplete last lines (when using readLines(...)) throughout the package

# sos4R 0.2-2

- Added rudimentary support for KML responses (no parsing)
- Changed parser detection, now uses mime type, too. This changes parser exchangeability for known mime types!
- Added attribute to observations (if response is saved) with file name (sosAttributeFileName)
- Added %\VignetteDepends{xtable} to vignette and depedency xtable as well - got NOTEd for that in R 2.13 checks

# sos4R 0.2-1

- Fixed in sosGetCRS(...) which breaks if there is no bounding box given in an offering, broke even plot funktion.
- Extended saveOrginal parameter, now it also accepts file names to be used for the document to be saved.
- Extended parsing of SensorML, adding slots for all parsed elements
- Added parsing of exception reports to parseFile(...)
- Based vignette completely on dowloaded files for releases

# sos4R 0.2

- Bugfix in coordinate order in coercion function for SpatialPointsDataFrame
- Added function sosCreateTime(...)
- Removed require(rgdal) from sosGetCRS(...) and replaced it with a check and potential warning as done in sp:::CRS
- Replacing potentially critical characters from data.frame column names (function .cleanupColumnName(...))
- Added function sosUOM(...) to extract the unit of measurement from objects
- Added summary methods for OmObservation and OmObservationCollection

# sos4R 0.1-17

- Added saveOriginal to describeSensor
- Extended parseFile to handle sensor description documents
- Changed vignette to use saved sensor description from file
- Fixes with exports and documentation to adhere to changes for R 2.13.0

# sos4R 0.1-16

- Added file to create demo for Coastlab SOS
- Added sosTime() for signature "list"
- Fixed bug in .getObservation_1.0.0 when calculating length of referenced ObservationProperty
- Fixed bug in getObservation() when offering is of class "character"
- Improvements in verbose-messages
- Improved sosChanges() based on code in vignette()
- Added sosCheatSheet() function that open the cheat sheet PDF, included new cheat sheet in package
- Change in createTime...() functions: removed unneccessary format and parsing -- please observe if everything still works!
- Changed slot type in OmObservation@observedProperty from "SwePhenomenonProperty" to "SwePhenomenonPropertyOrNULL"
- Testing istSOS and fixing bugs

# sos4R 0.1-15

- Fixes in airquality demo due to new names of observed properties.
- Added packages used in demos to DESCRIPTION's Suggest field
- Fix because of missing dependency for cacheSchweave -> removed caching from vignette. 

# sos4R 0.1-14

- Minor improvements with metadata information for sosCoordinates(SensorML)
- Vignette extension for usage of sosGetCRS()
- Added sosBoundedBy() and sosGetCRS() for objects of class SensorML
- Work on demos (airquality, ioos, and others)

# sos4R 0.1-13

- Added sosGetCRS() for SosObservationOffering based on bounding box
- Added plot() for SosObservationOffering
- Added sosTitle() and sosAbstract()
- fix in bbox creation in sosBoundedBy(... bbox = TRUE)
- Added coordinate switching mechanism, works during parsing, required sos to be added to a bunch of parsing functions
- Added sosId(), sosName(), sosAbstract(), and sosCoordinates(), coercion to Spatial, and plot() for class SensorML
- Added sosChanges()
- Work on demo for South Esk Test Bed, southesk
- Work on demo for EEA AirQuality data, airquality
- Overhaul of all demos with plotting
- Added summary functions for SOS, SosObservationOffering
- Added coercion functions from OmMeasurement, OmObservation and OmObservationCollection to SpatialPointsDataFrame
- Starting demo austria, consequently some fixing in parsing of capabilities (making mandatory elements optional in parsing to avoid errors)
- Fixed printing bug for class SOS when capabilities are only partly requested
- Updates in Vignette for new features

# sos4R 0.1-11

- Changed title and citation
- added .Rbuildignore to exclude Eclipse specific files in build
- Maintenance release to re-trigger generation of vignette with v-swe running

# sos4R 0.1-10

2011-01-17

- Using cacheSweave in vignette
- Removed OgcComparisonOpsOrXMLOrNULL and added type checks to validity function instead
- Fixed error when retrieving no data as CSV

# sos4R 0.1-09

- Changed required R version to 2.11.1 as 2.10 causes error on install
- Finished vignette
- Added parsing based on MIME type (rudimentary)

# sos4R 0.1-08

- Added package vignette
- Added SosDefaults()
- Added SosSupportedOperations()
- Added sosGetCRS(...)
- Changed behaviour of parsing function: If a converter is missing, it does not stop but only warn. The output data.frame then lacks that attribute.
- Added coordinates parameter to sosResult()
- Added function sosName() for classes SosObservationOffering, OwsServiceProvider
- Added convert parameter to sosTime() for offerings and conversion for object of class GmlTimePeriod
- Added bbox parameter to sosBoundedBy() which returns an sp-like matrix
- Added sosResultModel() for SosObservationOffering, but the namespace is not correct in 52N SOS
- Added unimplemented operations to the list of operation names
- Changed sosResult() for OmMeasurement, now returns a data.frame
- Added saveOriginal parameter to getObservation(ById)()
- Added parsing documents from saved files with sosParse(...)
- Added support for result format "text/csv"
- Changed URL for requests to be based on DCPs from capabilities file (if present)
- Added more options to getCapabilities, which can also be passed on using the "..." argument when creating a SOS

# sos4R 0.1-07

- Added more units of measurement to default list based on http://aurora.regenstrief.org/~ucum/ucum.html
- Added sosDefaultColumnNameFeatureIdentifier as default and used it in sosCoordinates() and set it to "features" so that it automatically matches the field name in 52N SOSs
- Added column names for lat, lon and SRS name as defaults
- Added file for questions on the mailing list.
- Added SosSupportedServiceVersions()
- Fixed check for DescribeSensor
- Fix in .getObservation_1.0.0 if empty observation collection is returned (error summing up the result lengths)
- Added names function for OmObservation, OmMeasurement, and OmObservationCollection
- Added URLs/skeletons for new demos
- Added @boundedBy for OmObservationCollection

# sos4R 0.1-06

- added sosFeatureIds(...)
- added sosObservedProperties(...) for OM Classes
- added subsetting of observation collection by procedure, observed property and feature id
- improved toString(...) of OmObservation
- changed behaviour of sosResult(...) for OmObservationCollection, now binds the data.frames of the contained observations

# sos4R 0.1-05

2010-11-04

- Corrected calculation of length in .getObservation_1.0.0
- Fixed length.OmObservationCollection(x)
- Moved examples from presentation.R (deleted!) and testing-SOS.R to demo scripts
- Moved usecase.R into weathersos demo and deleted the file
- Added sosProcedures(...) for OmMeasurement, OmObservation and OmObservationCollection
- Added sosCoordinates(...)

# sos4R 0.1-04

- Fixed sosContents(...)
- Added handling for missing responseFormat (although it's not optional) in parsing of observation offerings
- Moved warning when parsing offerings with missing time or envelope
- Added sosBoundedBy(offering)
- Renamed sosEventTimePeriod(...) to sosTime(...)

# sos4R 0.1-03

- Some fixes in toString methods (errorenous recursive paste calls)
- Fixed bug in parsePoint, which returned a GmlDirectPosition
- Fixed potential bug in parseSosCapabilities and getCapabilities (occured if no version attribute is given)
- Fixed potential NULL access in parseSosObservationOffering(...)
- Renamed curlOpts in SOS(...) to curlOptions
- Added check for NULL for sampling time and feature of interest in parseObservation (also making parsing less restrictive as a workaround for not valid responses) and parsing of result time 
- Moved class Measure to GML (as it is not SWE)
- Added class OmObservationCollection with indexing functions [ and [[
- Renamed SosCapabilities_1.1.0 to SosCapabilities_1.0.0
- Introduced abstract class SOS and class SOS_1.0.0
- Added first coercion functions
- Named converters consistently by changing SosFieldConvertingFunctions(...) to  SosDataFieldConvertingFunctions(...)
- Changed sosFOIs(...) to sosFeaturesOfInterest(...) and added method for offering
- Changed default values of getObservation, now uses sosFeaturesOfInterest(sos) and sosEventTimePeriod(offering)
- Changed sosOffering(...) to sosOfferings(...)
- Changed slot types to "character" where reasonable in class SosObservationOffering, a whole lot of fixes for that...
- Added sosObservedProperties(SosObservationOffering)
- Accessor functions now operate based on the offering, not the allowed parameter values
- Renamed functions from SosSpec to SosDescribeSensor, SosGetObservation and SosGetObservationById and moved according files to SOS-class.R and SOS-methods.R

# sos4R 0.1-02

- Finished documentation

# sos4R 0.1-1

- Initial release of version 0.1-01
