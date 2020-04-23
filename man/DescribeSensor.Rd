\name{DescribeSensor}
\Rdversion{1.1}

\docType{class}

\alias{DescribeSensor}
\alias{SosDescribeSensor}
\alias{SosDescribeSensor-class}
\alias{show,SosDescribeSensor-method}
\alias{print,SosDescribeSensor-method}
\alias{toString,SosDescribeSensor-method}

\alias{describeSensor}
\alias{describeSensor-methods}
\alias{describeSensor,SOS,character-method}
\alias{describeSensor,SOS_1.0.0,character-method}
\alias{describeSensor,SOS_2.0.0,character-method}

\title{Retrieve Sensor Descriptions from a SOS}

\description{
The DescribeSensor operation of a Sensor Observation Service can be used to retrieve metadata of procedures that are available from a SOS. This sensor description is normally encoded in \verb{SensorML}.

The method \code{describeSensor(..)} sends a DescribeSensor request for a description of the given procedure to the given Sensor Observation Service instance. If the procedure is a vector, the method will send multiple requests and combine the results in a list.

Please also consult the specification for details on possible contents of the request.

This functions should not be called directly, but instead using the function \code{\link{describeSensor}}.
}

\section{Objects from the Class}{Objects can be created by calling the construction function of the form \code{DescribeSensor(...)}. The contain the procedure identifier that is to be described by a service.}

\usage{
describeSensor(sos, procedure, outputFormat, validTime, verbose, inspect, saveOriginal)

# internal constructors:
# exactly one of outputFormat or procedureDescriptionFormat must be used
SosDescribeSensor(service, version, procedure, outputFormat = NA_character_,
                  procedureDescriptionFormat = NA_character_, validTime = NULL)
}

\section{Methods}{
	\describe{
		\item{\code{signature(sos = "SOS", procedure = "character")}}{Method requests a description of the given procedure(s) from the given SOS.}
	}
}

\arguments{
  \item{service}{The service attribute of the request, e.g. \samp{SOS}.}
  \item{sos}{The SOS connection to use.}
  \item{version}{The version attribute of the request, e.g. \samp{1.0.0}.}
  \item{procedure}{The value of the procedure elements in the request, e.g. \samp{urn:procedure:42}.}
  \item{outputFormat}{The value of the output format element in the request, e.g. \samp{text/xml;subtype="sensorML/1.0.1"}.}
  \item{procedureDescriptionFormat}{The desired description format for the procedure, e.g. \samp{http://www.opengis.net/sensorML/1.0.1}. For using facing function \code{describeSensor(..)} the value of \code{outputFormat} is used.}
  \item{validTime}{An object of class \code{GmlTimeObject}, either a time instant or time period for which the returned description shall be valid; use functions \code{sosCreateTimePeriod(..)} and \code{sosCreateTimeInstant(..)} to create this parameter.}
  \item{verbose}{A boolean value indicating whether debug information is printed out to the console during the execution.}
  \item{inspect}{A boolean value to enable printing of the sent request and received response to the console.}
  \item{saveOriginal}{Save the received document to the current working directory. If \code{TRUE} a filename is automatically generated, if a character string is given it is used as the filename.}
}

\value{The value of the construction function is an object of class \link{SosDescribeSensor-class}}

\section{Slots}{
  \describe{
    \item{\code{procedure}:}{Object of class \code{"character"}, the identifier of the procedure.}
    \item{\code{outputFormat}:}{Object of class \code{"character"}, the requested output format.}
    \item{\code{procedureDescriptionFormat}:}{Object of class \code{"character"}, the requested description format.}
    \item{\code{validTime}:}{Object of class \code{"GmlTimeObject"}, the time instant or period for which the description shall be valid.}
    \item{\code{service}:}{Object of class \code{"character"}, the service type, e.g. \code{"SOS"}.}
    \item{\code{request}:}{Object of class \code{"character"}, the name of the request, \code{"DescribeSensor"}.}
    \item{\code{version}:}{Object of class \code{"character"}, the service version, e.g. \code{"1.0.0"}}
  }
}

\section{Extends}{
Class \code{"\linkS4class{OwsServiceOperation}"}, directly.
}

\section{Methods}{
  \describe{
    \item{encodeRequestKVP}{\code{signature(obj = "DescribeSensor")}: Encode the information in the request as key-value-pairs for HTTP GET connections, see \code{\link{encodeRequestKVP-methods}}.}
    \item{encodeRequestSOAP}{\code{signature(obj = "DescribeSensor")}: Encode the information in the request as XML for SOAP connections, see \code{\link{encodeRequestSOAP-methods}}.}
    \item{encodeRequestXML}{\code{signature(obj = "DescribeSensor")}: Encode the information in the request as XML for HTTP POST connections, see \code{\link{encodeRequestXML-methods}}.}
    \item{show}{\code{signature(object = "DescribeSensor")}: Show a human readable excerpt of the contents of the object.}
	 }
}

\references{See OGC 06-009r6 section 8.4, or the XSD schema file at \url{http://schemas.opengis.net/sos/1.0.0/sosDescribeSensor.xsd}.}

\author{Daniel Nuest \email{daniel.nuest@uni-muenster.de}}

\seealso{See Also \code{\link{SensorML}} and \link{describeSensor}.}

\examples{
showClass("SosDescribeSensor")

# example for construction function
describeSensorRequest <- SosDescribeSensor(service = "SOS", version = "1.0.0",
	procedure = "urn:procedure:42", outputFormat = "text/xml")
print(describeSensorRequest)

# encode the request in XML
sos <- SOS_Test()
encodeRequestXML(describeSensorRequest, sos)
toString(encodeRequestXML(describeSensorRequest, sos))

# request a sensor description
mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/kvp",
             binding = "KVP")
mySensor <- describeSensor(sos = mySOS,
		procedure = sosProcedures(mySOS)[[1]],
		outputFormat = 'text/xml; subtype="sensorML/1.0.1"', # space is needed!
		)
}

\keyword{classes}
