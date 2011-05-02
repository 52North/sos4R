\name{SosFeatureOfInterest-class}
\Rdversion{1.1}
\docType{class}

\alias{SosFeatureOfInterest-class}
\alias{show,SosFeatureOfInterest-method}
\alias{print,SosFeatureOfInterest-method}
\alias{toString,SosFeatureOfInterest-method}
\alias{SosFeatureOfInterest}
\alias{SosFeatureOfInterestOrNULL-class}

\title{Class and Construction Function for "SosFeatureOfInterest"}

\description{Element in a GetObservation request to a Sensor Observation service to constrain the observations to be returned regarding the observed feature.}

\section{Objects from the Class}{
Objects can be created by calls to the construction function of the form \code{SosFeatureOfInterest(...)}.

\code{SosFeatureOfInterestOrNULL} is a virtual class to model optional slots of containing elements: No objects may be created from it.
}

\usage{
SosFeatureOfInterest(objectIDs = list(NA), spatialOps = NULL)
}

\arguments{
  \item{objectIDs}{A list of character identifiers of features in a SOS.}
  \item{spatialOps}{An object of class \code{OgcSpatialOps} for spatial filtering.}
}

\section{Slots}{
  \describe{
    \item{\code{objectIDs}:}{Object of class \code{"list"}: Identifiers of features of interest.}
    \item{\code{spatialOps}:}{Object of class \code{"OgcSpatialOpsOrNULL"}: A spatial filtering of the result.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{SosFeatureOfInterestOrNULL}"}, directly.
}

\section{Methods}{
  \describe{
    \item{encodeXML}{\code{signature(obj = "SosFeatureOfInterest", sos = "ANY")}: Convert the object to a XML representation.}
	 }
}

\details{
Specifies the feature for which observations are requested. This can either be represented by a reference to a feature ID advertised in the capabilities document or can be a spatial constraint.
}

\references{See section 8.4.2 of the SOS specification: Na, A., Priest, M. (Eds.), Sensor Observation Service, Open Geospatial Consortium Inc., OGC 06-009r6, Version: 1.0}

\author{Daniel Nuest \email{daniel.nuest@uni-muenster.de}}

%\note{  }
%\section{Warning }{}

\seealso{
See also \code{\linkS4class{SosGetObservation}}, and the convenience creation function \code{\link{sosCreateFeatureOfInterest-methods}}.
}

\examples{
showClass("SosFeatureOfInterest")
showClass("SosFeatureOfInterestOrNULL")
}

\keyword{classes}
