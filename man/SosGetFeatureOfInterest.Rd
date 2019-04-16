\name{SosGetFeatureOfInterest_2.0.0-class}

\Rdversion{1.1}

\docType{class}

\alias{SosGetFeatureOfInterest_2.0.0-class}
\alias{SosGetFeatureOfInterest_2.0.0}

\title{Class \code{"SosGetFeatureOfInterest_2.0.0"}}
\description{
Representation of a GetFeatureOfInterest operation request that needs to be sent to a Sensor Observation Service to retrieve the features of interest, i.e. the real world features that are observed and for which observations are taken.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SosGetFeatureOfInterest_2.0.0", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{featureOfInterest}:}{Object of class \code{"character"} ~~ }
    \item{\code{service}:}{Object of class \code{"character"} ~~ }
    \item{\code{request}:}{Object of class \code{"character"} ~~ }
    \item{\code{version}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{OwsServiceOperation}"}, directly.
}
\section{Methods}{
  \describe{
    \item{checkRequest}{\code{signature(service = "SOS_2.0.0", operation = "SosGetFeatureOfInterest_2.0.0", verbose = "logical")}: ... }
    \item{encodeRequestKVP}{\code{signature(obj = "SosGetFeatureOfInterest_2.0.0")}: ... }
    \item{sosName}{\code{signature(obj = "SosGetFeatureOfInterest_2.0.0")}: ... }
	 }
}

\examples{
showClass("SosGetFeatureOfInterest_2.0.0")
}
\keyword{classes}
