\name{getObservation-methods}

\docType{methods}

\alias{getObservation}
\alias{getObservation-methods}
\alias{getObservation,SOS,character-method}
\alias{getObservation,SOS,SosObservationOffering-method}
\alias{getObservation,SOS_1.0.0,character-method}
\alias{getObservation,SOS_1.0.0,SosObservationOffering-method}
\alias{getObservationById}
\alias{getObservationById-methods}
\alias{getObservationById,SOS,character-method}
\alias{getObservationById,SOS_1.0.0,character-method}

\title{Request Observations}

\description{
This method sends a GetObservation request to the given SOS.
}

\section{Methods}{
\describe{
	\item{\code{signature(sos = "SOS", offering = "SosObservationOffering")} or \code{signature(sos = "SOS", offering = "character")}}{Request observation data from the given SOS for the given offering (either character identifier or an object of class \code{SosObservationOffering}).}
	\item{\code{signature(sos = "SOS", observationId = "character")}}{Request observation data from the given SOS for the given observation identifier.}
}
}

\details{
 It takes a variety of inputs, of which only the offering is mandatory for GetObservation request, and the observationId for GetObservationByIdRequests.
}

\examples{
\dontrun{
# by identifier
sos <- SOS(...)
os.offerings <- sosOfferings(sos)
obsId <- getObservationById(sos = sos, observationId = "o_3508493")

# TODO

}
}

\keyword{methods}
