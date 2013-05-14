\name{DescribeSensor}
\Rdversion{1.1}

\docType{class}

\alias{DescribeSensor}
\alias{SosDescribeSensor}
\alias{SosDescribeSensor-class}
\alias{show,SosDescribeSensor-method}
\alias{print,SosDescribeSensor-method}
\alias{toString,SosDescribeSensor-method}

\title{Class and Construction Function for "SosDescribeSensor"}

\description{
The DescribeSensor Operatiosn of a Sensor Observation Service can be used to retrieve metadata of procedures that are available from a SOS. This sensor description is normally encoded in \verb{SensorML}.

Please also consult the specification for details on possible contents of the request.

This functions should not be called directly, but instead using the function \code{\link{describeSensor}}.
}

\section{Objects from the Class}{Objects can be created by calling the construction function of the form \code{DescribeSensor(...)}. The contain the procedure identifier that is to be described by a service.}

\usage{
SosDescribeSensor(service, version, procedure, outputFormat)
}

\arguments{
  \item{service}{The service attribute of the request, e.g. \samp{SOS}.}
  \item{version}{The version attribute of the request, e.g. \samp{1.0.0}.}
  \item{procedure}{The value of the procedure elements in the request, e.g. \samp{urn:procedure:42}.}
  \item{outputFormat}{The value of the output format element in the request, e.g. \samp{text/xml;subtype="sensorML/1.0.1"}}
}

\value{The value of the construction function is an object of class \link{SosDescribeSensor-class}}

\section{Slots}{
  \describe{
    \item{\code{procedure}:}{Object of class \code{"character"}, the identifier of the procedure.}
    \item{\code{outputFormat}:}{Object of class \code{"character"}, the requested output format.}
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
    \item{checkRequest}{\code{signature(service = "SOS", operation = "DescribeSensor", verbose = "logical")}: Checks the contents of the request before sending it.}
    \item{encodeRequestKVP}{\code{signature(obj = "DescribeSensor")}: Encode the information in the request as key-value-pairs for HTTP GET connections, see \code{\link{encodeRequestKVP-methods}}.}
    \item{encodeRequestSOAP}{\code{signature(obj = "DescribeSensor")}: Encode the information in the request as XML for SOAP connections, see \code{\link{encodeRequestSOAP-methods}}.}
    \item{encodeRequestXML}{\code{signature(obj = "DescribeSensor")}: Encode the information in the request as XML for HTTP POST connections, see \code{\link{encodeRequestXML-methods}}.}
    \item{show}{\code{signature(object = "DescribeSensor")}: Show a human readable excerpt of the contents of the object.}
	 }
}

\references{See OGC 06-009r6 section 8.4, or the XSD schema file at \url{http://schemas.opengis.net/sos/1.0.0/sosDescribeSensor.xsd}.}

\author{Daniel Nuest \email{daniel.nuest@uni-muenster.de}}

%\note{}
%\section{Warning }{}

\seealso{See Also \code{\link{SensorML}} and \link{describeSensor}.}

\examples{
showClass("SosDescribeSensor")

# example for construction function
describeSensorRequest <- SosDescribeSensor(service = "SOS", version = "1.0.0",
	procedure = "urn:procedure:42", outputFormat = "text/xml")
print(describeSensorRequest)

# encode the request in XML
encodeRequestXML(describeSensorRequest)

}

\keyword{classes}
