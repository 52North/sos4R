\name{MonitoringPoint-class}

\Rdversion{1.1}

\docType{class}

\alias{WmlMonitoringPoint-class}
\alias{WmlMonitoringPoint}
\alias{print,WmlMonitoringPoint-method}
\alias{show,WmlMonitoringPoint-method}
\alias{toString,WmlMonitoringPoint-method}

\alias{sosName,WmlMonitoringPoint-method}

\title{Class \code{"WmlMonitoringPoint"}}

\description{
A monitoring point is the feature of interest defined for WaterML observations, i.e. a monitoring point represents the real world feature for which observations are taken. This may be, for example, the position of a stream flow sensor at a river.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("WmlMonitoringPoint", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{id}:}{Object of class \code{"character"}, the GML id of the point.}
    \item{\code{identifier}:}{Object of class \code{"character"}, the identifier of the point.}
    \item{\code{names}:}{Object of class \code{"character"}, the names of the point.}
    \item{\code{sampledFeatures}:}{Object of class \code{"character"}, the identifiers of sampled features.}
    \item{\code{shape}:}{Object of class \code{"SamsShape"}, the shape of the point.}
    \item{\code{timeZone}:}{Object of class \code{"ANY"}, the time zone (if available).}
    \item{\code{verticalDatums}:}{Object of class \code{"AN"}, the vertical datum (if available).}
  }
}

\section{Extends}{
Class \code{"\linkS4class{GmlFeature}"}, directly.
Class \code{"\linkS4class{GmlFeatureOrNULL}"}, by class "GmlFeature", distance 2.
Class \code{"\linkS4class{GmlFeatureOrGmlFeaturePropertyOrNULL}"}, by class "GmlFeature", distance 2.
}

\section{Methods}{
  \describe{
    \item{sosName}{\code{signature(x = "WmlMonitoringPoint")}: access the name of the point.}
	 }
}

\examples{
showClass("WmlMonitoringPoint")
}
\keyword{classes}
