\name{SAMS}
\Rdversion{1.1}
\docType{class}

\alias{SamsSamplingFeature}
\alias{SamsSamplingFeature-class}

\alias{print,SamsSamplingFeature-method}
\alias{show,SamsSamplingFeature-method}
\alias{toString,SamsSamplingFeature-method}

\alias{sosCoordinates,SamsSamplingFeature-method}
\alias{sosFeatureIds,SamsSamplingFeature-method}
\alias{sosFeaturesOfInterest,SamsSamplingFeature-method}
\alias{sosName,SamsSamplingFeature-method}

\alias{parseSams200SamplingFeature}

\alias{SamsShape-class}
\alias{SamsShape}
\alias{SamsShapeOrNULL}
\alias{SamsShapeOrNULL-class}
\alias{print,SamsShape-method}
\alias{show,SamsShape-method}
\alias{toString,SamsShape-method}
\alias{sosCoordinates,SamsShape-method}

\title{Classes and creation functions for Sampling Features}

\description{
Sampling Feature classes.

\code{SamsShape} represents the geometry of a spatial sampling feature, that can be used as a feature of interest. Currently, only points are supported.
}

\section{Objects from the Class}{
Objects can be created by calls to the creation functions:

\code{SamsSamplingFeature(...)}

\code{SamsShape(...)}

}

\section{Slots}{
  \describe{
    \item{\code{identifier}:}{Object of class \code{"character"} ~~ }
    \item{\code{name}:}{Object of class \code{"character"} ~~ }
    \item{\code{type}:}{Object of class \code{"character"} ~~ }
    \item{\code{sampledFeature}:}{Object of class \code{"character"} ~~ }
    \item{\code{shape}:}{Object of class \code{"SamsShape"} ~~ }
    \item{\code{id}:}{Object of class \code{"character"} ~~ }
    \item{\code{point}:}{Object of class \code{"GmlPoint"} ~~ }
    \item{\code{id}:}{Object of class \code{"character"} ~~ }
    \item{\code{srsName}:}{Object of class \code{"character"} ~~ }
    \item{\code{srsDimension}:}{Object of class \code{"integer"} ~~ }
    \item{\code{axisLabels}:}{Object of class \code{"character"} ~~ }
    \item{\code{uomLabels}:}{Object of class \code{"character"} ~~ }
  }
}

\section{Extends}{
\code{SamsShape} extends \code{"\linkS4class{GmlGeometry}"}, directly.

\code{SamsSamplingFeature} extends \code{"\linkS4class{GmlFeature}"}, directly.
\code{SamsSamplingFeature} extends \code{"\linkS4class{GmlFeatureOrNULL}"}, by class "GmlFeature", distance 2.
\code{SamsSamplingFeature} extends \code{"\linkS4class{GmlFeatureOrGmlFeaturePropertyOrNULL}"}, by class "GmlFeature", distance 2.
}

\section{Methods}{
  \describe{
    \item{sosCoordinates}{\code{signature(obj = "SamsSamplingFeature")}: access to the coordinates of the feature.}
    \item{sosFeatureIds}{\code{signature(obj = "SamsSamplingFeature")}: access to the identifier of the feature.}
    \item{sosFeaturesOfInterest}{\code{signature(obj = "SamsSamplingFeature")}: access the feature itself.}
    \item{sosName}{\code{signature(obj = "SamsSamplingFeature")}: access a human-readable name of the feature.}
	 }
}

\references{
https://www.opengeospatial.org/standards/om
}

\author{
Daniel Nuest
}

\note{
Schema: http://schemas.opengis.net/sampling/2.0/samplingFeature.xsd
}

\examples{
showClass("SamsSamplingFeature")
showClass("SamsShape")
}

\keyword{classes}
