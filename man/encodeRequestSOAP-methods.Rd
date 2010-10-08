\name{encodeRequestSOAP-methods}

\docType{methods}

\alias{encodeRequestSOAP}
\alias{encodeRequestSOAP-methods}
\alias{encodeRequestSOAP,DescribeSensor-method}
\alias{encodeRequestSOAP,GetObservation-method}
\alias{encodeRequestSOAP,GetObservationById-method}
\alias{encodeRequestSOAP,OwsGetCapabilities-method}

\title{Methods for Encoding Requests to SOS in SOAP Format}

\description{
These methods encode objects representing requests to a Sensor Observation Service into a SOAP message format to be used in the SOAP binding (see \code{\link{SosBindings}}).
}

\section{Methods}{
	\describe{
		\item{\code{signature(obj = "DescribeSensor")}}{Encode a DescribeSensor operation.}
		\item{\code{signature(obj = "GetObservation")}}{Encode a GetObservation operation.}
		\item{\code{signature(obj = "GetObservationById")}}{Encode a GetObservationById operation.}
		\item{\code{signature(obj = "OwsGetCapabilities")}}{Encode a GetCapabilities operation.}
	}
}

\seealso{\code{\link{SosBindings}} ,\code{\link{encodeXML}}}

\keyword{methods}