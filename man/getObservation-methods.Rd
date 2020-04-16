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

% Version 2.0.0
\alias{getObservation,SOS_2.0.0,SosObservationOffering_2.0.0-method}
\alias{getObservation,SOS_2.0.0,character-method}
\alias{getObservationById,SOS_2.0.0,character-method}


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
# request observations
mySOSpox <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/pox",
             binding = "POX", useDCPs = FALSE)

myOffering <- sosOfferings(mySOS)[["ws2500"]]
period <- sosCreateTimePeriod(sos = mySOS,
		begin = as.POSIXct("2015/11/01"),
		end = as.POSIXct("2015/11/02"))
eventTime <- sosCreateEventTimeList(period)

nov2015 <- getObservation(sos = mySOSpox,
                          offering = myOffering,
                          eventTime = eventTime)

# request observation by identifier and get the data
obsId <- getObservationById(sos = mySOSpox,
                            observationId = "http://www.52north.org/test/observation/1")
sosResult(obsId, coordinates = TRUE)
}
}

\keyword{methods}
