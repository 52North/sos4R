\name{OmObservationCollection}
\Rdversion{1.1}

\docType{class}

\alias{OmObservationCollection}
\alias{OmObservationCollection-class}
\alias{length,OmObservationCollection-method}
\alias{show,OmObservationCollection-method}
\alias{sosResult,OmObservationCollection-method}
\alias{print,OmObservationCollection-method}
\alias{toString,OmObservationCollection-method}

\title{Class "OmObservationCollection"}
\description{
Collection of arbitrary observations.
}

\section{Objects from the Class}{
Objects can be created by calls to the construction function of the form \code{OmObservationCollection(...)}.
}

\section{Slots}{
  \describe{
    \item{\code{members}:}{Object of class \code{"list"} containing objects of class \code{OmObservation} or \code{OmMeasurement}.}
  }
}

\section{Methods}{
  \describe{
    \item{length}{\code{signature(x = "OmObservationCollection")}: Returns the number of observations in the slot \code{members}.}
    \item{sosResult}{\code{signature(obj = "OmObservationCollection")}: Extract the result slots from the containted observations or measurements.}
	 }
}

\references{Cox, S. (Ed.), Observations and Measurements – Part 1 - Observation schema, Open Geospatial Consortium Inc., OGC 07-022r1, Version: 1.0}

\author{Daniel Nüst \email{daniel.nuest@uni-muenster.de}}

%\note{}

\seealso{
\code{\linkS4class{OmObservation}} or \code{\linkS4class{OmMeasurement}}.
}

\examples{
showClass("OmObservationCollection")

}

\keyword{classes}
