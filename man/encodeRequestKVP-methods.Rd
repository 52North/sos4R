\name{encodeRequestKVP-methods}

\docType{methods}

\alias{encodeRequestKVP}
\alias{encodeRequestKVP-methods}
\alias{encodeRequestKVP,SosDescribeSensor-method}
\alias{encodeRequestKVP,SosGetObservation-method}
\alias{encodeRequestKVP,SosGetObservationById-method}
\alias{encodeRequestKVP,OwsGetCapabilities-method}
\alias{encodeRequestKVP,OwsGetCapabilities_1.1.0-method}
\alias{encodeRequestKVP,OwsGetCapabilities_2.0.0-method}

% Version 2.0.0
\alias{encodeRequestKVP,SosGetFeatureOfInterest_2.0.0-method}
\alias{encodeRequestKVP,SosGetObservation_2.0.0-method}



\title{Methods for Encoding Requests to SOS in KVP Format}

\description{
These methods encode objects representing requests to a Sensor Observation Service into a key-value-pair format which can be used in the GET binding, see \link{SosBindings}.
}

\section{Methods}{
	\describe{
		\item{\code{signature(obj = "SosDescribeSensor")}}{Encode a DescribeSensor request.}
		\item{\code{signature(obj = "SosGetObservation")}}{Encode a GetObservation request.}
		\item{\code{signature(obj = "SosGetObservationById")}}{Encode GetObservationById request.}
		\item{\code{signature(obj = "OwsGetCapabilities")}}{Dispatching method, checks the version attribute and forwards the encoding to the appropriate method. This method should be called rather than calling the versioned methods directly!}
		\item{\code{signature(obj = "OwsGetCapabilities_1.1.0")}}{Encode GetCapabilities request with OWS version 1.1.0.}
		\item{\code{signature(obj = "OwsGetCapabilities_2.0.0")}}{Encode GetCapabilities request with OWS version 2.0.0.}
	}
}

\keyword{methods}
