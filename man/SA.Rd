\name{SA}
\Rdversion{1.1}

\docType{class}

\alias{sa}
\alias{sampling features}
\alias{SaSamplingPoint}
\alias{SaSamplingPoint-class}
\alias{show,SaSamplingPoint-method}
\alias{SaSamplingSurface}
\alias{SaSamplingSurface-class}
\alias{show,SaSamplingSurface-method}
\alias{print,SaSamplingPoint-method}
\alias{print,SaSamplingSurface-method}
\alias{toString,SaSamplingPoint-method}
\alias{toString,SaSamplingSurface-method}

\alias{SaSamplingPoint}
\alias{sosCoordinates,SaSamplingPoint-method}
\alias{sosFeatureIds,SaSamplingPoint-method}
\alias{sosFeaturesOfInterest,SaSamplingPoint-method}

\title{Classes of the Namespace sa}

\description{
Classes and construction functions for elements from the OGC specification \dQuote{Observations and Measurements - Part 2 - Sampling Features}.
}

\section{Objects from the Class}{
Objects can be created by calls to the construction functions of the form \code{SaSamplingPoint(...)}.
}

\section{Slots}{
  \describe{
    \item{\code{sampledFeatures}:}{Object of class \code{"list"} which contains the sampled features.}
    \item{\code{position}:}{Object of class \code{"GmlPointProperty"} which contains the position of a feature.}
    \item{\code{relatedObservation}:}{Object of class \code{"list"} which contains identifiers of related observations.}
    \item{\code{relatedSamplingFeature}:}{Object of class \code{"list"} which contains identifiers of related sampling features.}
    \item{\code{surveyDetails}:}{Object of class \code{"ANY"} which can contain information about survey details (which are currently no modeled in an \R class).}
    \item{\code{id}:}{The identifier of a sampling elements (object of class \code{"character"}).}
    \item{\code{shape}:}{Object of class \code{"ANY"} which could be used to model the shape of a sampling surface.}
    \item{\code{area}:}{Object of class \code{"ANY"} which could be used to model the area of a sampling surface.}
  }
}

\section{Extends}{
SaSamplingPoint and SaSamplingSurface:
Class \code{"\linkS4class{GmlFeature}"}, directly.
Class \code{"\linkS4class{GmlFeatureOrNULL}"}, by class "GmlFeature", distance 2.
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "SaSamplingPoint")}: ... }
    \item{show}{\code{signature(object = "SaSamplingSurface")}: ... }
    \item{sosCoordinates}{\code{signature(object = "SaSamplingPoint")}: access to the coordinates of the feature.}
    \item{sosFeatureIds}{\code{signature(object = "SaSamplingPoint")}: access to the identifier of the feature.}
    \item{sosFeaturesOfInterest}{\code{signature(object = "SaSamplingPoint")}: access to the feature of interest, i.e. the point itself.}
  }
}

\usage{
SaSamplingPoint(sampledFeatures, position, relatedObservation = list(NA),
                relatedSamplingFeature = list(NA), surveyDetails = NA, id = NA_character_)
}

\arguments{
  \item{sampledFeatures}{ ~~ }
  \item{position}{ ~~ }
  \item{relatedObservation}{ ~~ }
  \item{relatedSamplingFeature}{ ~~ }
  \item{surveyDetails}{ ~~ }
  \item{id}{ ~~ }
}

\value{Construction functions: An object of the respective class.}

\references{Simon Cox (Ed.), Observations and Measurements - Part 2 - Sampling Features, OGC 07-002r3}

\author{Daniel Nuest \email{daniel.nuest@uni-muenster.de}}

\examples{
showClass("SaSamplingPoint")

# create sampling point
SaSamplingPoint(sampledFeatures = list("feature1", "feature2"),
                position = GmlPointProperty(href = "http://link.to/point"))
}

\keyword{classes}
