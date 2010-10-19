\name{sosRequest-methods}
\docType{methods}

\alias{sosRequest}
\alias{sosRequest-methods}
\alias{sosRequest,SOS_1.0.0,OwsServiceOperation,logical,logical-method}

\title{Send Request to SOS}

\description{
This is the main request function for sending and receiving requests respectively documents from a Sensor Observation Service. It's intended for internal use.

Please use the methods for the SOS operations as long as possible: \code{\link{getCapabilities-methods}}, \code{\link{describeSensor-methods}}, \code{\link{getObservation-methods}}, and \code{\link{getObservationById-methods}}.
}

\section{Methods}{
	\describe{
		\item{\code{signature(sos = "SOS", request = "OwsServiceOperation", verbose = "logical", inspect = "logical")}}{Method sends the given operation to the given SOS connection. \code{verbose} activates extensive debugging to the console. \code{inspect} prints only the request and response documents to the console.}
	}
}

\keyword{methods}
