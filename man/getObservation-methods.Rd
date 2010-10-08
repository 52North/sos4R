\name{getObservation-methods}

\docType{methods}

\alias{getObservation}
\alias{getObservation-methods}
\alias{getObservation,SOS,SosObservationOfferingOrCharacter-method}
\alias{getObservationById}
\alias{getObservationById-methods}
\alias{getObservationById,SOS,character-method}

\title{Request Observations}

\description{
This method sends a GetObservation request to the given SOS.
}

\section{Methods}{
\describe{
	\item{\code{signature(sos = "SOS", offering = "SosObservationOfferingOrCharacter")}}{Request observation data from the given SOS for the given offering (either ID or an object of class \code{SosObservationOffering}).}
	\item{\code{signature(sos = "SOS", observationId = "character")}}{Request observation data from the given SOS for the given observation identifier.}
}
}

\details{
 It takes a variety of inputs, of which only the offering is mandatory for GetObservation request, and the observationId for GetObservationByIdRequests.
}

\examples{
# TBD
}

\keyword{methods}
