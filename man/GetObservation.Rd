\name{GetObservation}
\Rdversion{1.1}

\docType{class}

\alias{GetObservation}
\alias{SosGetObservation}
\alias{SosGetObservation-class}
\alias{show,SosGetObservation-method}
\alias{toString,SosGetObservation-method}
\alias{print,SosGetObservation-method}
\alias{GetObservationById}
\alias{SosGetObservationById}
\alias{SosGetObservationById-class}
\alias{show,SosGetObservationById-method}
\alias{print,SosGetObservationById-method}
\alias{toString,SosGetObservationById-method}

\title{GetObservation and GetObservationById Request Objects}

\description{Classes (and their construction functions) to request observations from a Sensor Observation Service.}

\usage{
SosGetObservation(service, version, offering, observedProperty, responseFormat, srsName = as.character(NA), eventTime = list(NA), procedure = as.character(NA), featureOfInterest = NULL, result = NULL, resultModel = as.character(NA), responseMode = as.character(NA), BBOX = as.character(NA))
SosGetObservationById(service, version, observationId, responseFormat, srsName = as.character(NA), resultModel = as.character(NA), responseMode = as.character(NA))
}

\section{Objects from the Class}{
Objects can be created by calls to the construction functions of the form \code{SosGetObservationById(...)} or \code{SosGetObservationById(...)}.
}

\section{Slots}{
  \describe{
    \item{\code{BBOX}:}{Object of class \code{"character"}, specifies a bounding box for spatial filtering to be applied in GET requests (only), see \link{SosBindings}}
    \item{\code{eventTime}:}{Object of class \code{"list"}, specifies the time period(s) for which observations are requested.}
    \item{\code{featureOfInterest}:}{Object of class \code{"SosFeatureOfInterestOrNULL"}, specifies the feature for which observations are requested. This can either be represented by a reference to a feature ID advertised in the capabilities document or can be a spatial constraint}
    \item{\code{observationId}:}{Object of class \code{"character"}, the Id of the requested observation.}
    \item{\code{observedProperty}:}{Object of class \code{"list"}, specifies the phenomenon or phenomena for which observations are requested.}
    \item{\code{offering}:}{Object of class \code{"character"}, specifies the offering URI advertised in the GetCapabilities document.}
    \item{\code{procedure}:}{Object of class \code{"list"}, procedure parameter specifies the sensor system(s) for which observations are requested.}
    \item{\code{request}:}{Object of class \code{"character"}, the name of the request.}
    \item{\code{responseFormat}:}{Object of class \code{"character"}, specifies the desired resultFormat MIME content type for transport of the results.}
    \item{\code{responseMode}:}{Object of class \code{"character"}, specifies whether results are requested in-line, outof-band, as an attachment, or if this is a request for an observation template that will be used for subsequent calls to GetResult.}
    \item{\code{resultModel}:}{Object of class \code{"character"}, specifies the QName of the root element of an O&M Observation or element in the appropriate substitution group.}
    \item{\code{result}:}{Object of class \code{"ANY"}, provides a place to put in OGC filter expressions based on property values. This instructs the SOS to only return observations where the result matches this expression.}
    \item{\code{service}:}{Object of class \code{"character"}, service type identifier.}
    \item{\code{srsName}:}{Object of class \code{"character"}, defines the spatial reference system that should be used for any geometries that are returned in the response. This must be one of the advertised values in the offering specified in gml:srsName elements.}
    \item{\code{version}:}{Object of class \code{"character"}, specification version for operation.}
  }
}

\section{Extends}{
\bold{SosGetObservation:}
Class \code{"\linkS4class{OwsServiceOperation}"}, directly.

\bold{SosGetObservationById:}
Class \code{"\linkS4class{OwsServiceOperation}"}, directly.
}

\arguments{
  \item{service}{The \verb{service} attribute of the request, e.g. \samp{SOS}.}
  \item{version}{The \verb{version} attribute of the request, e.g. \samp{1.0.0}.}
  \item{observationId}{The value of the \verb{ObservationId} element in the request, e.g. \samp{o_12345}, which is to be optained. This could have been obtained by the client via a URL in a feed, alert, or some other notification.}
  \item{offering}{The \verb{offering} element value in the request, e.g. \samp{"temperatures"}. All other parameters are depending on the selected offering.}
  \item{observedProperty}{A list of values for \verb{observedProperty} elements in the request, e.g. \samp{"urn:property:AirTemperature"}. IDs of phenomena are advertised in capabilities document.}
  \item{responseFormat}{The \verb{responseFormat} element value in the request, e.g. \samp{text/xml;subtype="om/1.0.0"}. ID of the output format to be used for the requested data. The supported output formats are listed in the selected offering capabilities.}
  \item{srsName}{The \verb{srsName} attribute of the request, e.g. \samp{urn:ogc:def:crs:EPSG:4326}.}
  \item{eventTime}{A list of objects of class \link{SosEventTime-class} which are added as \verb{eventTime} elements to the request. Allows a client to request observations from a specific instant, multiple instances or periods of time in the past, present and future. The supported range is listed in the selected offering capabilities.}
  \item{procedure}{A list of procedure identifiers added to the request as \verb{procedure} elements.}
  \item{featureOfInterest}{An object of class \link{SosFeatureOfInterest} added to the request as the \verb{featureOfInterest} element, or \code{NULL}. Specifies target feature for which observations are requested.}
  \item{result}{An object of class \link{OgcComparisonOps-class} added to the request as \verb{result} element, or \code{NULL}, or any element that can be encoded using \code{encodeXML(...)} and then be added to an XML document with \code{addChildren(...)}. Filtering: Only report observations where the result matches this expression.}
  \item{resultModel}{The \verb{resultModel} element of the request, e.g. \samp{om:Measurement}, which is an identifier of the result model to be used for the requested data. The resultModel values supported by a service are listed in the contents section of the service metadata, identified as QName values.}
  \item{responseMode}{The \verb{responseMode} element of the request, e.g. \samp{inline}, which allows the client to request the form of the response.}
  \item{BBOX}{A bounding box to be used only with \verb{KVP} encoding in request via \verb{HTTP GET}, in the format \samp{minlon,minlat,maxlon,maxlat,srsURI?}, with the spatial reference system being optional. This element is ignored for \verb{POST} requests, use the parameter \code{featureOfInterest} instead, see \code{\link{SosBindings}}.}
}

\section{Methods}{
  \describe{
    \item{checkRequest}{\code{signature(service = "SOS", operation = "SosGetObservationById", verbose = "logical")} or \code{signature(service = "SOS", operation = "GetObservation", verbose = "logical")}: Check the request for validity and for compliance with the metadata available in from the given SOS.}
    \item{encodeRequestKVP}{\code{signature(obj = "SosGetObservationById")} or \code{signature(obj = "GetObservation")}: Transform the information to key-value pair format, see \code{\link{encodeKVP}}.}
    \item{encodeRequestXML}{\code{signature(obj = "SosGetObservationById")} or \code{signature(obj = "GetObservation")}: Transform the information to XML format, see \code{\link{encodeXML}}.}
    \item{encodeRequestSOAP}{\code{signature(obj = "SosGetObservation")}: Transform the information to XML format for SOAP requests.}
	 }
}

\details{
Please consult the specification for details on possible contents of the request.
}

\value{An object of class \link{SosGetObservation-class} or \link{SosGetObservationById-class} respectively.}

\references{
See OGC 06-009r6 section 8.4, or the XSD schema file at \url{http://schemas.opengis.net/sos/1.0.0/sosGetObservation.xsd}.

See OGC 06-009r6 section 10.1, or the XSD schema file at \url{http://schemas.opengis.net/sos/1.0.0/sosGetObservationById.xsd}.
}

\author{Daniel Nüst \email{daniel.nuest@uni-muenster.de}}

\seealso{\code{\link{SosGetObservation-class}}, \code{\link{SosGetObservationById-class}}}

\examples{
showClass("SosGetObservation")
showClass("SosGetObservationById")

observationRequest <- SosGetObservation(service = "SOS", version = "1.0.0", offering = "temperatures", observedProperty = list("urn:property:AirTemperature"), responseFormat = "text/xml;subtype=&quot;om/1.0.0&quot;")
print(observationRequest)

observationByIdRequest <- SosGetObservationById(service = "SOS", version = "1.0.0", observationId = "o_12345", responseFormat = "text/xml;subtype=&quot;om/1.0.0&quot;")
print(observationByIdRequest)

\dontrun{
sos <- SOS("http://mysos.net/sos")
encodeXML(observationByIdRequest, sos = sos)
}

}

\keyword{classes}
\keyword{utitlities}
