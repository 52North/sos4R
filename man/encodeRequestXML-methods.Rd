\name{encodeRequestXML-methods}

\docType{methods}

\alias{encodeRequestXML}
\alias{encodeRequestXML-methods}
\alias{encodeRequestXML,SosDescribeSensor-method}
\alias{encodeRequestXML,SosGetObservation-method}
\alias{encodeRequestXML,SosGetObservationById-method}
\alias{encodeRequestXML,OwsGetCapabilities-method}
\alias{encodeRequestXML,OwsGetCapabilities_1.1.0-method}
\alias{encodeRequestXML,OwsGetCapabilities_2.0.0-method}
\alias{encodeRequestXML,SosEventTime,SOS-method}
\alias{encodeRequestXML,SosFeatureOfInterest,SOS-method}
\alias{encodeRequestXML,SosGetFeatureOfInterest_2.0.0,SOS-method}
\alias{encodeRequestXML,SosGetFeatureOfInterest_2.0.0-method}
\alias{encodeRequestXML,SosGetObservation_2.0.0-method}
\alias{encodeRequestXML,XMLNode,SOS-method}

\title{Methods for Encoding Requests to SOS in XML Format}

\description{
These methods encode objects representing requests to a Sensor Observation Service into a XML format which can be used in the POST binding, see \code{\link{SosBindings}}.
}

\section{Methods}{
	\describe{
		\item{\code{signature(obj = "SosDescribeSensor")}}{Encode a DescribeSensor request.}
		\item{\code{signature(obj = "SosGetObservation")}}{Encode a GetObservation request.}
		\item{\code{signature(obj = "SosGetObservationById")}}{Encode a GetObservationById request.}
		\item{\code{signature(obj = "OwsGetCapabilities")}}{Encode a GetCapabilities request.}
	}
}

\references{Annex A (normative) \dQuote{SOS Schema} in Na, A., Priest, M. (Eds.), Sensor Observation Service, Open Geospatial Consortium Inc., OGC 06-009r6, Version: 1.0.}

\seealso{\code{\link{SosBindings}}, \code{\link{encodeXML}}}

\keyword{methods}
\keyword{XML}
