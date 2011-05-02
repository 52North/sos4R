\name{SOS}
\Rdversion{1.1}

\docType{class}

\alias{SOS}
\alias{SOS-class}
\alias{show,SOS-method}
\alias{print,SOS-method}
\alias{toString,SOS-method}

\alias{SOS_1.0.0}
\alias{SOS_1.0.0-class}
\alias{show,SOS_1.0.0-method}
\alias{print,SOS_1.0.0-method}
\alias{toString,SOS_1.0.0-method}

\alias{sosCapabilitiesDocumentOriginal}
\alias{sosCapabilitiesDocumentOriginal,SOS-method}

\alias{sosCaps}
\alias{sosCaps-methods}
\alias{sosCaps,SOS-method}

\alias{sosContents}
\alias{sosContents-methods}
\alias{sosContents,SOS-method}

\alias{sosDataFieldConverters}
\alias{sosDataFieldConverters-methods}
\alias{sosDataFieldConverters,SOS-method}

\alias{sosTime}
\alias{sosTime-methods}
\alias{sosTime,SOS-method}
\alias{sosTime,list-method}

\alias{sosFilter_Capabilities}
\alias{sosFilter_Capabilities-methods}
\alias{sosFilter_Capabilities,SOS-method}

\alias{sosFeaturesOfInterest}
\alias{sosFeaturesOfInterest-methods}
\alias{sosFeaturesOfInterest,SOS-method}
\alias{sosFeaturesOfInterest,SOS,character-method}
\alias{sosFeaturesOfInterest,SosObservationOffering-method}

\alias{sosMethod}
\alias{sosMethod-methods}
\alias{sosMethod,SOS-method}
\alias{sosMethod,SOS_1.0.0-method}

\alias{sosObservedProperties}
\alias{sosObservedProperties-methods}
\alias{sosObservedProperties,SOS-method}
\alias{sosObservedProperties,SosObservationOffering-method}

\alias{sosOfferingIds}
\alias{sosOfferingIds-methods}
\alias{sosOfferingIds,SOS-method}

\alias{sosOfferings}
\alias{sosOfferings-methods}
\alias{sosOfferings,SOS-method}
\alias{sosOfferings,SOS,character-method}

\alias{sosOperation}
\alias{sosOperation-methods}
\alias{sosOperation,SOS,character-method}

\alias{sosOperationsMetadata}
\alias{sosOperationsMetadata-methods}
\alias{sosOperationsMetadata,SOS-method}

\alias{sosParsers}
\alias{sosParsers-methods}
\alias{sosParsers,SOS-method}

\alias{sosProcedures}
\alias{sosProcedures-methods}
\alias{sosProcedures,SOS-method}
\alias{sosProcedures,list-method}
\alias{sosProcedures,SosObservationOffering-method}

\alias{sosResponseFormats}
\alias{sosResponseFormats-methods}
\alias{sosResponseFormats,SOS-method}
\alias{sosResponseFormats,OwsOperation-method}
\alias{sosResponseFormats,SosObservationOffering-method}

\alias{sosResponseMode}
\alias{sosResponseMode-methods}
\alias{sosResponseMode,SOS-method}
\alias{sosResponseMode,OwsOperation-method}
\alias{sosResponseMode,SosObservationOffering-method}

\alias{sosResultModels}
\alias{sosResultModels-methods}
\alias{sosResultModels,SOS-method}
\alias{sosResultModels,OwsOperation-method}

\alias{sosResult,character-method}

\alias{sosServiceIdentification}
\alias{sosServiceIdentification-methods}
\alias{sosServiceIdentification,SOS-method}
\alias{sosServiceProvider}
\alias{sosServiceProvider-methods}
\alias{sosServiceProvider,SOS-method}
\alias{sosSrsName}
\alias{sosSrsName-methods}
\alias{sosSrsName,SOS-method}
\alias{sosTimeFormat}
\alias{sosTimeFormat-methods}
\alias{sosTimeFormat,SOS-method}
\alias{sosUrl}
\alias{sosUrl-methods}
\alias{sosUrl,SOS-method}
\alias{sosUrl,SOS_1.0.0-method}
\alias{sosVersion}
\alias{sosVersion-methods}
\alias{sosVersion,SOS-method}
\alias{sosExceptionCodeMeaning}
\alias{sosExceptionCodeMeaning,character-method}
\alias{sosBoundedBy}
\alias{sosBoundedBy-method}
\alias{sosBoundedBy,list-method}
\alias{sosCoordinates}
\alias{sosCoordinates-method}
\alias{sosCoordinates,list-method}
\alias{sosCoordinates,SosObservationOffering-method}

\alias{sosId}
\alias{sosId-method}
\alias{sosId,list-method}
\alias{sosSrsName}
\alias{sosSrsName-method}

\alias{sosFeatureIds}
\alias{sosFeatureIds-method}
\alias{sosFeatureIds,list-method}
\alias{sosObservedProperties,list-method}
\alias{sosFeaturesOfInterest,list-method}

\alias{sosGetCRS}
\alias{sosGetCRS-method}
\alias{sosGetCRS,SOS-method}
\alias{sosGetCRS,list-method}
\alias{sosGetCRS,character-method}
\alias{sosGetCRS,SosObservationOffering-method}

\alias{sosName}
\alias{sosName-method}
\alias{sosName,list-method}
\alias{sosName,OwsServiceProvider-method}
\alias{sosName,OwsGetCapabilities-method}
\alias{sosName,OwsOperation-method}
\alias{sosName,SosDescribeSensor-method}
\alias{sosName,SosGetObservation-method}
\alias{sosName,SosGetObservationById-method}

\alias{sosTitle}
\alias{sosTitle-method}
\alias{sosTitle,SOS-method}

\alias{sosAbstract}
\alias{sosAbstract-method}
\alias{sosAbstract,SOS-method}

\alias{sosEncoders}
\alias{sosEncoders,SOS-method}

\alias{sosOperations}
\alias{sosOperations,SOS-method}
\alias{sosOperations,OwsCapabilities-method}
\alias{sosOperations,SosCapabilities_1.0.0-method}

\alias{sosGetDCP}
\alias{sosGetDCP,SOS,character-method}

\alias{sosSwitchCoordinates}
\alias{sosSwitchCoordinates-method}
\alias{sosSwitchCoordinates,SOS-method}

\alias{plot.SOS}
\alias{plot,SOS,missing-method}
\alias{plot.SosObservationOffering}
\alias{plot,SosObservationOffering,missing-method}
\alias{as.SosObservationOffering.SpatialPolygons}

\alias{print.summary.SOS}
\alias{print.summary.SosObservationOffering}

\alias{summary.SosObservationOffering}
\alias{summary.SOS}

\alias{sosResult,data.frame-method}

\alias{sosUOM}
\alias{sosUOM,list-method}
\alias{sosUOM,data.frame-method}


\title{Class, and Construction and Accessor Functions for "SOS"}

\description{Base class of a connection to a Sensor Observation Service.}

\section{Objects from the Class}{
Objects can be created by calls to the construction function of the form \code{SOS(...)}.

Object from the class can be used in calls to function for metadata retrieval of sensors (\code{link{describeSensor-methods}}) and observation data queries (\code{link{getObservation-methods}} and \code{link{getObservationById-methods}})
}

\section{Slots}{
  \describe{
    \item{\code{url}:}{Object of class \code{"character"}: The endpoint of the service, e.g. \code{http://myUrl.org/SOS1/sos}.}
    \item{\code{method}:}{Object of class \code{"character"}: The method, or transport protocol, see \code{\link{SosSupportedConnectionMethods}} for available ones.}
    \item{\code{version}:}{Object of class \code{"character"}: The connected service's version, e.g. \code{"1.0.0"}.}
    \item{\code{capabilities}:}{Object of class \code{"OwsCapabilities"} ~~ }
    \item{\code{parsers}:}{Object of class \code{"list"} ~~ }
    \item{\code{encoders}:}{Object of class \code{"list"} ~~ }
    \item{\code{dataFieldConverters}:}{Object of class \code{"list"}: A list of named functions to be used by the parsing methods to convert data values to the correct \R type, see \code{\link{SosDataFieldConvertingFunctions}} for the default functions and how to add your own converters.}
    \item{\code{curlHandle}:}{Object of class \code{"CURLHandle"}: The curl options to be passed on to the methods \code{getURL} and \code{postForm} from the package \pkg{RCurl}.}
    \item{\code{curlOptions}:}{Object of class \code{"ANY"}: The curl handle to be passed on to the methods \code{getURL} and \code{postForm} from the package \pkg{RCurl}.}
    \item{\code{timeFormat}:}{Object of class \code{"character"}: The time format to be used or decoding and encoding time character strings to and from \code{POSIXt} classes.}
    \item{\code{verboseOutput}:}{Object of class \code{"logical"}: Trigger parameter for extensive debugging information on the console.}
    \item{\code{switchCoordinates}:}{Object of class \code{"logical"}: Trigger switching of lat/long during parsing stage.}
  }
}
\section{Methods}{
  \describe{
    \item{sosAbstract}{\code{signature(obj = "SOS")}: ... }
    \item{sosCapabilitiesDocumentOriginal}{\code{signature(sos = "SOS")}: To retrieve the full original service metadata document.}
    \item{sosCaps}{\code{signature(sos = "SOS")}: ... }
    \item{sosContents}{\code{signature(sos = "SOS")}: ... }
    \item{sosDataFieldConverters}{\code{signature(sos = "SOS")}: ... }
    \item{sosTime}{\code{signature(obj = "SOS")}: ... }
    \item{sosTime}{\code{signature(obj = "SOS")}: ... }
    \item{sosTitle}{\code{signature(obj = "SOS")}: ... }
    \item{sosOperations}{\code{signature(sos = "...")}: ... }
    \item{sosGetCRS}{\code{signature(obj = "character")}: ... }
    \item{sosGetDCP}{\code{signature(sos = "SOS", operation = "character", type = "character")}: Get the distributed computing platform URL for the given operation and method type. If type is missing, the function returns all available DCPs. }
    \item{sosEncoders}{\code{signature(sos = "SOS")}: ... }
    \item{sosFeatureIds}{\code{signature(obj = "list")}: ... }
    \item{sosFeaturesOfInterest}{\code{signature(obj = "list")}: ... }
    \item{sosGetCRS}{\code{signature(obj = "character")}: Get an object of class \code{sp:::CRS} for a given OGC URN depicting a reference system, like \code{urn:ogc:def:crs:EPSG:1000}.}
    \item{sosMethod}{\code{signature(sos = "SOS")}: ... }
    \item{sosName}{\code{signature(obj = "...")}: ... }
    \item{sosObservedProperties}{\code{signature(sos = "SOS")}: ... }
    \item{sosOffering}{\code{signature(sos = "SOS", offeringId = "character")}: ... }
    \item{sosOfferingIds}{\code{signature(sos = "SOS")}: ... }
    \item{sosOfferings}{\code{signature(sos = "SOS")}: ... }
    \item{sosOperation}{\code{signature(sos = "SOS", operationName = "character")}: ... }
    \item{sosOperationsMetadata}{\code{signature(sos = "SOS")}: ... }
    \item{sosParsers}{\code{signature(sos = "SOS")}: ... }
    \item{sosProcedures}{\code{signature(sos = "SOS")}: Accessor function for the procedures of a \code{\linkS4class{SOS}} (via list in capabilities of \code{GetObservation} operation) or a \code{\linkS4class{SosObservationOffering}}.}
    \item{sosResponseFormats}{\code{signature(sos = "SOS")}: TBD: add missing signatures ... }
    \item{sosResponseMode}{\code{signature(sos = "SOS")}: ... }
    \item{sosResultModels}{\code{signature(sos = "SOS")}: ... }
    \item{sosServiceIdentification}{\code{signature(sos = "SOS")}: ... }
    \item{sosServiceProvider}{\code{signature(sos = "SOS")}: ... }
    \item{sosSrsName}{\code{signature(sos = "SOS")}: ... }
    \item{sosSwitchCoordinates}{\code{signature(sos = "SOS")}: ... }
    \item{sosTimeFormat}{\code{signature(sos = "SOS")}: ... }
    \item{sosUrl}{\code{signature(sos = "SOS")}: ... }
    \item{sosVersion}{\code{signature(sos = "SOS")}: ... }
    \item{sosTime}{\code{signature(obj = "SOS")}: Accessor function for the event time period from the GetObservation operations metadata.}
    \item{sosFilter_Capabilities}{\code{signature(sos = "SOS")}: Accessor function for the filter capabilities of a SOS object.}
	 }
}

\usage{
SOS(url, method = SosDefaultConnectionMethod(), version = "1.0.0", parsers = SosParsingFunctions(), encoders = SosEncodingFunctions(), dataFieldConverters = SosDataFieldConvertingFunctions(), curlOptions = list(), curlHandle = getCurlHandle(), timeFormat = sosDefaultTimeFormat, verboseOutput = FALSE, switchCoordinates = FALSE, ...)

# accessor methods ...
}

\arguments{
  \item{url}{See the corresponding slot description.}
  \item{method}{See the corresponding slot description.}
  \item{version}{See the corresponding slot description.}
  \item{parsers}{See the corresponding slot description.}
  \item{encoders}{See the corresponding slot description.}
  \item{dataFieldConverters}{See the corresponding slot description.}
  \item{curlOptions}{See the corresponding slot description.}
  \item{curlHandle}{See the corresponding slot description.}
  \item{timeFormat}{See the corresponding slot description.}
  \item{verboseOutput}{See the corresponding slot description.}
  \item{switchCoordinates}{See the corresponding slot description.}
  \item{...}{Additional parameters that are passed on to the \code{\link{getObservation}} call that is done within this function.}
}

\value{The construction functions returns an object of class \code{\link{SOS-class}}.}

\details{
From the introduction of the specification document: \dQuote{The goal of SOS is to provide access to observations from sensors and sensor systems in a standard way that is consistent for all sensor systems including remote, in-situ, fixed and mobile sensors.}
}

\references{
Na, A., Priest, M. (Eds.), Sensor Observation Service, Open Geospatial Consortium Inc., OGC 06-009r6, Version: 1.0

The document is available for download at \url{http://www.opengeospatial.org/standards/sos}.
}

\author{Daniel Nuest \email{daniel.nuest@uni-muenster.de}}

%\section{Warning }{}

\seealso{See also creation function \code{\link{SOS}} and the package vignette for general description of use.}

\examples{
showClass("SOS")

\dontrun{
# create a SOS connection
mysos <- SOS(url = "http://mysos.org/sos")

# create a SOS connetion with a specific connection method and time format
mysos <- SOS(url = "http://mysos.org/sos",
    method = "GET", timeFormat = "%Y-%m-%d")

# turn on verbose output for all methods and functions
SOS(url = "http://mysos.org/sos", verboseOutput = TRUE)

# get the meaning of an exception code
sosExceptionCodeMeaning(ex@exceptionCode)

# create a CRS object from a URN CRS string
sosGetCRS("urn:ogc:def:crs:EPSG:4217")
}



}

\keyword{classes}
