\name{SosGetDataAvailability_1.0.0-class}
\Rdversion{1.1}
\docType{class}

\alias{DataAvailabilityMember}
\alias{DataAvailabilityMember-class}
\alias{print,DataAvailabilityMember-method}
\alias{show,DataAvailabilityMember-method}
\alias{toString,DataAvailabilityMember-method}
\alias{SosGetDataAvailability_1.0.0}
\alias{SosGetDataAvailability_1.0.0-class}

\alias{SosGetDataAvailability_1.0.0-class}
\alias{encodeRequestKVP,SosGetDataAvailability_1.0.0-method}
\alias{sosName,SosGetDataAvailability_1.0.0-method}
\alias{toString,SosGetDataAvailability_1.0.0-method}
\alias{getDataAvailability,SOS_2.0.0-method}
\alias{getDataAvailability}
\alias{parseGetDataAvailabilityResponse}

\title{Class and construction function for \code{"GetDataAvailability"} operation}
\description{
See SOS 2.0 Hydrology profile specification, OGC 14-004r1, section 7.4, requirement 12
}
\section{Objects from the Class}{
Objects can be created by calls to construction functions:

\code{DataAvailabilityMember(...)}

\code{SosGetDataAvailability_1.0.0(...)}

}

\section{Slots}{
  \describe{
    \item{\code{procedure}:}{Object of class \code{"character"} ~~ }
    \item{\code{observedProperty}:}{Object of class \code{"character"} ~~ }
    \item{\code{featureOfInterest}:}{Object of class \code{"character"} ~~ }
    \item{\code{phenomenonTime}:}{Object of class \code{"GmlTimePeriod"} ~~ }
    \item{\code{procedures}:}{Object of class \code{"list"} ~~ }
    \item{\code{observedProperties}:}{Object of class \code{"list"} ~~ }
    \item{\code{featuresOfInterest}:}{Object of class \code{"list"} ~~ }
    \item{\code{offerings}:}{Object of class \code{"list"} ~~ }
    \item{\code{service}:}{Object of class \code{"character"} ~~ }
    \item{\code{request}:}{Object of class \code{"character"} ~~ }
    \item{\code{version}:}{Object of class \code{"character"} ~~ }
  }
}

\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "DataAvailabilityMember")}: ... }
    \item{show}{\code{signature(object = "DataAvailabilityMember")}: ... }
    \item{toString}{\code{signature(x = "DataAvailabilityMember")}: ... }
    \item{checkRequest}{\code{signature(service = "SOS_2.0.0", operation = "SosGetDataAvailability_1.0.0", verbose = "logical")}: ... }
    \item{encodeRequestKVP}{\code{signature(obj = "SosGetDataAvailability_1.0.0")}: ... }
    \item{sosName}{\code{signature(obj = "SosGetDataAvailability_1.0.0")}: ... }
    \item{toString}{\code{signature(x = "SosGetDataAvailability_1.0.0")}: ... }
	 }
}

\section{Extends}{
Class \code{SosGetDataAvailability_1.0.0} extends \code{"\linkS4class{OwsServiceOperation}"}, directly.
}

\references{
OGC 14-004r1, section 7.4, requirement 12
}

\author{Daniel Nuest \email{daniel.nuest@uni-muenster.de}}


\examples{
showClass("DataAvailabilityMember")
showClass("SosGetDataAvailability_1.0.0")
}

\keyword{classes}
