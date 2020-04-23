\name{parse}

\alias{parse}

\alias{parseCategoryObservation}
\alias{parseComplexObservation}
\alias{parseComponent}
\alias{parseCompositePhenomenon}
\alias{parseCountObservation}
\alias{parseDataArray}
\alias{parseElementType}
\alias{parseEncoding}
\alias{parseFeatureCollection}
\alias{parseField}
\alias{parseFOI}
\alias{parseGeometryObservation}
\alias{parseMeasure}
\alias{parseMeasurement}
\alias{parseObservation}
\alias{parseObservationCollection}
\alias{parseOM}
\alias{parseOwsException}
\alias{parseOwsExceptionReport}
\alias{parseOwsOperation}
\alias{parseOwsRange}
\alias{parseOwsServiceIdentification}
\alias{parseOwsServiceProvider}
\alias{parsePhenomenonProperty}
\alias{parsePoint}
\alias{parsePosition}
\alias{parseResult}
\alias{parseSamplingPoint}
\alias{parseSensorML}
\alias{parseSosCapabilities}
\alias{parseSosFilter_Capabilities}
\alias{parseSosObservationOffering}
\alias{parseTemporalObservation}
\alias{parseTextBlock}
\alias{parseTimeGeometricPrimitiveFromParent}
\alias{parseTimeInstant}
\alias{parseTimeInstantProperty}
\alias{parseTimePeriod}
\alias{parseTimePosition}
\alias{parseTimeObject}
\alias{parseTruthObservation}
\alias{parseValues}
\alias{parseObservationProperty}

\alias{sosParse}
\alias{sosParse-methods}
\alias{sosParse,SOS_1.0.0,character-method}
\alias{sosParse,SOS_1.0.0,character,logical-method}

\alias{parseNoParsing}
\alias{parseCSV}
\alias{parseFile}
\alias{parseFile-method}
\alias{parseFile,SOS_1.0.0,character-method}
\alias{parseFile,SOS_versioned,character-method}

\alias{parseSweCoordinate}
\alias{parseSweCoordinate-method}
\alias{parseSweLocation}
\alias{parseSweLocation-method}
\alias{parseSwePosition}
\alias{parseSwePosition-method}
\alias{parseSweVector}
\alias{parseSweVector-method}

\alias{parseGetFeatureOfInterestResponse}
\alias{parseGetObservationResponse}
\alias{parseWmlMonitoringPoint}
\alias{parseObservation_2.0}
\alias{parseSamsShape}
\alias{parseSosCapabilities100}
\alias{parseSosCapabilities200}
\alias{parseSosObservationOffering_200}
\alias{parseSosObservedProperty}
\alias{parseSwesObservableProperty}
\alias{parseTextEncoding}
\alias{parseTime}

\alias{parseDescribeSensorResponse}
\alias{parseEnvelope}
\alias{parseGetObservationByIdResponse}

\alias{sosIsListFieldAvailable}
\alias{gmlGetReferencedNode}
\alias{gmlIsNodeReferenced}

\alias{sos4R_caches}

\title{Parsing Functions for XML Documents and Elements}

\description{The functions decode a given XML object to an \R representation, which can be an object of a specific class, a list, a named character vector, \ldots}

\usage{
parseCSV(obj, verbose = FALSE)
parseFile(sos, file, verbose = FALSE, ...)
parseCategoryObservation(obj, sos, verbose = FALSE)
parseComplexObservation(obj, sos, verbose = FALSE)
parseComponent(obj, sos, verbose = FALSE)
parseCompositePhenomenon(obj, sos, verbose = FALSE)
parseCountObservation(obj, sos, verbose = FALSE)
parseDataArray(obj, sos, verbose = FALSE)
parseElementType(obj, sos, verbose = FALSE)
parseEncoding(obj, sos, verbose = FALSE)
parseEnvelope(obj, sos, namespaces = xml2::xml_ns(obj), verbose = FALSE)
parseFeatureCollection(obj, sos)
parseField(obj, sos, verbose = FALSE)
parseFOI(obj, sos, verbose = FALSE)
parseGeometryObservation(obj, sos, verbose = FALSE)
parseMeasure(obj)
parseMeasurement(obj, sos, verbose = FALSE)
parseNoParsing(obj, sos)
parseObservation(obj, sos, verbose = FALSE)
parseObservationCollection(obj, sos, verbose)
parseOM(obj, sos, verbose = FALSE)
parseOwsException(obj, sos)
parseOwsExceptionReport(obj, sos, verbose = FALSE)
parseOwsOperation(obj, sos)
parseOwsRange(obj)
parseOwsServiceIdentification(obj, sos)
parseOwsServiceProvider(obj, sos)
parsePhenomenonProperty(obj, sos, verbose = FALSE)
parsePoint(obj, sos)
parsePosition(obj, sos)
parseResult(obj, sos, verbose = FALSE)
parseSamplingPoint(obj, sos)
parseSensorML(obj, sos, verbose = FALSE)
parseSosCapabilities(obj, sos)
parseSosFilter_Capabilities(obj, sos)
parseSosObservationOffering(obj, sos)
parseTemporalObservation(obj, sos, verbose = FALSE)
parseTextBlock(obj)
parseTimeGeometricPrimitiveFromParent(obj, sos)
parseTimeInstant(obj, sos)
parseTimeInstantProperty(obj, sos)
parseTimePeriod(obj, sos)
parseTimePosition(obj, sos)
parseTimeObject(obj, sos, verbose = FALSE)
parseTruthObservation(obj, sos, verbose = FALSE)
parseValues(values, fields, encoding, sos, verbose = FALSE)

sosIsListFieldAvailable(listField)

gmlGetReferencedNode(sos, doc, node, verbose = FALSE)
gmlIsNodeReferenced(sos, node)
}

\arguments{
  \item{obj}{The object to decode, normally objects of either \verb{xml_document}.}
  \item{sos}{An object of class \link{SOS-class}, which may be utilized/required by some parsing functions to access other parsing functions or encoding information.}
  \item{verbose}{A boolean value indication whether information is printed out to the console during the process - potentially a lot of output!}
  \item{namespaces}{A vector of namespace prefixes and definitions to use.}
  \item{values}{The values to be parsed in \code{parseValues(...)}.}
  \item{fields}{Field information  in \code{parseValues(...)}, a named list.}
  \item{file}{Name of the file to be parsed in \code{sosParse(...)}.}
  \item{encoding}{Encoding information in \code{parseValues(...)}, an object of class \code{SweTextBlock}.}
  \item{...}{Additional arguments that are parsed to \code{xml2::read_xml(..)}.}
  \item{listField}{Object checked to be of type \code{list} and at least one item long.}
  \item{doc}{XML document.}
  \item{node}{XML node.}
}

\details{
The naming of the functions follow the following rule: \code{parse[optional: namespace prefix][name of the XML element to be parsed]}

Not all parsing function that have a \code{SOS} object or \code{verbose} in their signature, but few actually use it at this points of development. Some of the parsing functions are \strong{exchangeable} when creating a new SOS connection. Please see the examples!

Functions parsing time rely on the slot \code{timeFormat} of \code{SOS} objects.

\code{parseOM} is a special function in the respect that it matches sub parsing function depending on an objects \code{xmlName} from the list of the given \code{SOS}'s parsing functions.

\code{parseNoParsing} is a convenience function that directly returns the object without any changes.

\code{sosParse} allows parsing of files for all elements that have a parsers registered with the given SOS.

The parsing uses an internal cache to store within-document references to other nodes.
}

\value{An objects of a specific class depending on the parsing method and the passed object, possibly even lists or named character vectors.}

\author{Daniel Nuest \email{daniel.nuest@uni-muenster.de}}

\section{Warning }{
Functions might result in error if parsed an object of the wrong type, because that is normally not checked.

Some of the functions are placeholders for future implementations!
}

\seealso{\code{\link{SosParsingFunctions}}, \code{\link{sosParsers-methods}}}

\examples{
# parsing a XML string to an exception report object
er.doc <- xml2::read_xml(paste0("<ows:ExceptionReport xmlns:ows=\"http://www.opengis.net/ows/1.1\"",
  " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" version=\"1.0.0\"",
  " xsi:schemaLocation=\"http://schemas.opengis.net/ows/1.1.0/owsExceptionReport.xsd\">",
  "<ows:Exception exceptionCode=\"VersionNegotiationFailed\" locator=\"AcceptVersions\">",
  "<ows:ExceptionText>The parameter 'AcceptVersions' does not contain the version",
  " of this SOS: '1.0.0'</ows:ExceptionText></ows:Exception></ows:ExceptionReport>"))
er.parsed <- parseOwsExceptionReport(er.doc, sos = SOS_Test())
print(er.parsed)
utils::str(er.parsed)

\dontrun{
# save and re-parse an observation from file
obsId <- getObservationById(sos = mySOS, observationId = "o_3508493",
		saveOriginal = TRUE)
.files <- list.files(getwd())
.startWithO_ <- .files \%in\% grep("o_", .files, value=TRUE)
.observationFiles <- subset(.files, .startWithO_)

obsId <- parseFile(sos = mySOS, file = .observationFiles[[1]])

# list caches
ls(get("sos4R_caches"))
}

}

\keyword{methods}
\keyword{misc}
