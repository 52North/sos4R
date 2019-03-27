\name{Defaults}

\alias{sosDefault}
\alias{Defaults}
\alias{sosDefaultCharacterEncoding}
\alias{SosDefaultBinding}
\alias{SosDefaultDCPs}
\alias{sosDefaultDescribeSensorOutputFormat}
\alias{sosDefaultGetCapAcceptFormats}
\alias{sosDefaultGetCapOwsVersion}
\alias{sosDefaultGetCapSections}
\alias{sosDefaultGetObsResponseFormat}
\alias{sosDefaultSpatialOpPropertyName}
\alias{sosDefaultTempOpPropertyName}
\alias{sosDefaultTemporalOperator}
\alias{sosDefaultTemporalValueReference}
\alias{sosDefaultTimeFormat}
\alias{sosDefaultFilenameTimeFormat}
\alias{sosDefaultColumnNameFeatureIdentifier}
\alias{sosDefaultColumnNameLat}
\alias{sosDefaultColumnNameLon}
\alias{sosDefaultColumnNameSRS}
\alias{sosDefaultColorPalette}
\alias{sosDefaultReferenceFrameSensorDescription}
\alias{sosDefaultServiceVersion}

\alias{SosParsingFunctions}
\alias{SosEncodingFunctions}
\alias{SosDisabledParsers}
\alias{SosDataFieldConvertingFunctions}
\alias{SosExampleServices}
\alias{SosResetParsingFunctions}

\alias{SosDefaults}
\alias{SosDefaults2}
\alias{SosDefaultParsingOptions}


\title{Default Parameter Settings and Handling Functions}

\description{
These values are default parameters and handling functions for connections and requests to, as well as response processing of ansers from, Sensor Observation Services. These allow to simplify a SOS connection for the most common use cases and non-expert users.
}

\usage{
SosDefaultBinding()

SosParsingFunctions(..., include = character(0), exclude = character(0))
SosEncodingFunctions(..., include = character(0), exclude = character(0))
SosDataFieldConvertingFunctions(..., include = character(0), exclude = character(0))

SosDisabledParsers()

SosExampleServices()

SosDefaults()

SosResetParsingFunctions(sos)

SosDefaultDCPs()

SosDefaultParsingOptions()
}

\arguments{
  \item{\dots}{Named references to functions to be used for the respective element during parsing, encoding oder conversion, e.g. \code{"myUnit" = myUnitParser}.}
  \item{include}{A list of names of elements whose functions shall be included in the returned list, e.g. \code{include = c("GetObservation", "DescribeSensor")}. This inclusion is done \bold{after} replacing the default functions based on the \code{...} argument.}
  \item{exclude}{A list of names of elements whose functions shall be excluced in the returned list, e.g. \code{exclude = c("DescribeSensor")}. This exclusion is done \bold{after} replacing the default functions based on the \code{...} argument.}
  \item{sos}{An object of class \code{SOS}.}
}

\details{
The default values are strongly related to what is actually implemented in the package, but also often resemble the (hopefully) most common use cases.

Some defaults are accessed directly, others should be accessed using a function. The latter is required for cases where a runtime evaluation is needed, e.g. for default values of construction functions.

A special case are the functions to access the default functions for specific purposes, which are the parsing functions, the encoding functions and the field converting functions. See the examples on how to use them.

The function \code{SosDisabledParsers} can be used to use no parsing at all (despite the parsing for the capabilities response, which is required for establishing a connection to a SOS. This function is helpful to inspect the unprocessed responses from a service.

The function \code{SosResetParsingFunctions} can be used to replace the included parsing functions of a \code{SOS} object with the default ones. This is even useful for development of the default parsing functions.

\bold{The default parameter values are:}
\describe{
	\item{sosDefaultCharacterEncoding}{\verb{\Sexpr[results=verbatim,stage=render]{sosDefaultCharacterEncoding}}} % "UTF-8"
	\item{sosDefaultDescribeSensorOutputFormat}{\verb{\Sexpr[results=text,stage=render]{sosDefaultDescribeSensorOutputFormat}}} % SosSupportedResponseFormats()[2]
	\item{sosDefaultGetCapSections}{\verb{\Sexpr[results=text,stage=render]{sosDefaultGetCapSections}}} % c("All")
	\item{sosDefaultGetCapAcceptFormats}{\verb{\Sexpr[results=text,stage=render]{sosDefaultGetCapAcceptFormats}}} % c("text/xml")
	\item{sosDefaultGetCapOwsVersion}{\verb{\Sexpr[results=text,stage=render]{sosDefaultGetCapOwsVersion}}} % "1.1.0"
	\item{sosDefaultGetObsResponseFormat}{\verb{\Sexpr[results=text,stage=render]{sosDefaultGetObsResponseFormat}}} % SosSupportedResponseFormats()[1]
	\item{sosDefaultTimeFormat}{\verb{\Sexpr[results=text,stage=render]{sosDefaultTimeFormat}}} % "%Y-%m-%dT%H:%M:%OS"
	\item{sosDefaultFilenameTimeFormat}{\verb{\Sexpr[results=text,stage=render]{sosDefaultFilenameTimeFormat}}} %
	\item{sosDefaultTempOpPropertyName}{\verb{\Sexpr[results=text,stage=render]{sosDefaultTempOpPropertyName}}} % "om:samplingTime"
	\item{sosDefaultTemporalOperator}{\verb{\Sexpr[results=text,stage=render]{sosDefaultTemporalOperator}}} % SosSupportedTemporalOperators()[[ogcTempOpTMDuringName]]
	\item{sosDefaultSpatialOpPropertyName}{\verb{\Sexpr[results=text,stage=render]{sosDefaultSpatialOpPropertyName}}} % "urn:ogc:data:location"
}

The \bold{default parsing functions} can be replaced for a variety of XML elements, so that you only need to replace the parts of the parsing that really must be changed. Be aware that inclusion and exclusion are performed after merging the given functions with the defaults!

\bold{Example Services:} This list contains a few SOS instances that were tested (to different degress) with \pkg{sos4R}. The package authors do not maintain these services, so no guarantee can be given that these are usable.
}

\value{The default value of the respective setting or parameter. This can be a list, especially a named list of functions.}

\references{\code{\link{Constants}}}

\examples{
# simple default values
show(sosDefaultCharacterEncoding)
show(sosDefaultDescribeSensorOutputFormat)
show(sosDefaultGetCapAcceptFormats)
show(sosDefaultGetCapOwsVersion)
show(sosDefaultGetCapSections)
show(sosDefaultGetObsResponseFormat)
show(sosDefaultSpatialOpPropertyName)
show(sosDefaultTempOpPropertyName)
show(sosDefaultTemporalOperator)
show(sosDefaultTimeFormat)
SosDefaultBinding()

\dontrun{
# usage of defaults in construction method for SOS class
sos <- SOS("http://mysos.com/sos", binding = SosDefaultBinding(),
		timeFormat = sosDefaultTimeFormat)
}

# functions to disable all parsing
SosDisabledParsers()

# Replace a parsing function
myER <- function(xml) {
	return("EXCEPTION!!!11")
}
SosParsingFunctions("ExceptionReport" = myER)

# use inclusion and exclusion, important: even the just added function needs to
# be included manually!
SosParsingFunctions("ExceptionReport" = myER,
	include = c("GetObservation", "DescribeSensor", "ExceptionReport"))
SosParsingFunctions(exclude = c("GetObservation", "DescribeSensor"))

\dontrun{
# Replace an encoding function
myEncoding <- function(object, v) {
	return(str(object))
}

sos = SOS(url = "http://mysos.com/sos",
		encoders = SosEncodingFunctions("POST" = myPostEncoding))

# Use custom converting function and connection method. This mechanism works the
# same for encoders and decoders.
myConverters <- SosDataFieldConvertingFunctions(
	"myNumericUnit" = sosConvertDouble,
mySos <- SOS(sos.url, binding = "KVP", dataFieldConverters = myConverters)
sosDataFieldConverters(mySos)

# inspecting XML using dummy parsing function
sos = SOS(url = "http://mysos.com/sos", parsers = SosDisabledParsers)
describeSensor(sos, sosProcedures(sos)[[1]])
}

# a list of example services
SosExampleServices()

# a named list of all defaults
SosDefaults()

# replace the parsing functions with the default ones
\dontrun{
sos <- SosResetParsingFunctions(sos)
}

# parsing options used by xml2
SosDefaultParsingOptions()

}

\keyword{misc}
