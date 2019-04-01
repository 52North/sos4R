\name{MonitoringPoint-class}

\Rdversion{1.1}

\docType{class}

\alias{MonitoringPoint-class}
\alias{MonitoringPoint}
\alias{print,MonitoringPoint-method}
\alias{show,MonitoringPoint-method}
\alias{toString,MonitoringPoint-method}

\title{Class \code{"MonitoringPoint"}}
\description{
A monitoring point is the feature of interest defined for WaterML observations, i.e. a monitoring point represents the real world feature for which observations are taken. This may be, for example, the position of a stream flow sensor at a river.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MonitoringPoint", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{sampledFeatures}:}{Object of class \code{"list"} ~~ }
    \item{\code{id}:}{Object of class \code{"character"} ~~ }
    \item{\code{identifier}:}{Object of class \code{"list"} ~~ }
    \item{\code{names}:}{Object of class \code{"list"} ~~ }
    \item{\code{shape}:}{Object of class \code{"SamsShape"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{GmlFeature}"}, directly.
Class \code{"\linkS4class{FoiOrNULL}"}, directly.
Class \code{"\linkS4class{GmlFeatureOrNULL}"}, by class "GmlFeature", distance 2.
Class \code{"\linkS4class{GmlFeatureOrGmlFeaturePropertyOrNULL}"}, by class "GmlFeature", distance 2.
}
\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "MonitoringPoint")}: ... }
    \item{show}{\code{signature(object = "MonitoringPoint")}: ... }
    \item{toString}{\code{signature(x = "MonitoringPoint")}: ... }
	 }
}

\examples{
showClass("MonitoringPoint")
}
\keyword{classes}
