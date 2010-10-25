\name{encodeRequestSOAP-methods}

\docType{methods}

\alias{encodeRequestSOAP}
\alias{encodeRequestSOAP-methods}
\alias{encodeRequestSOAP,SosDescribeSensor-method}
\alias{encodeRequestSOAP,SosGetObservation-method}
\alias{encodeRequestSOAP,SosGetObservationById-method}
\alias{encodeRequestSOAP,OwsGetCapabilities-method}

\title{Methods for Encoding Requests to SOS in SOAP Format}

\description{
These methods encode objects representing requests to a Sensor Observation Service into a SOAP message format to be used in the SOAP binding (see \code{\link{SosBindings}}).
}

\section{Methods}{
	\describe{
		\item{\code{signature(obj = "SosDescribeSensor")}}{Encode a DescribeSensor operation.}
		\item{\code{signature(obj = "SosGetObservation")}}{Encode a GetObservation operation.}
		\item{\code{signature(obj = "SosGetObservationById")}}{Encode a GetObservationById operation.}
		\item{\code{signature(obj = "OwsGetCapabilities")}}{Encode a GetCapabilities operation.}
	}
}

\seealso{\code{\link{SosBindings}} ,\code{\link{encodeXML}}}

\keyword{methods}