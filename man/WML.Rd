\name{WML}
\Rdversion{1.1}
\docType{class}

\alias{WmlTimeseries}
\alias{WmlTimeseries-class}

\alias{WmlMeasurementTimeseries}
\alias{WmlMeasurementTimeseries-class}

\alias{WmlMeasurementTimeseriesMetadata}
\alias{WmlMeasurementTimeseriesMetadata-class}

\alias{WmlDefaultTVPMeasurementMetadata}
\alias{WmlDefaultTVPMeasurementMetadata-class}

\alias{WmlInterpolationType}
\alias{WmlInterpolationType-class}

\alias{WmlMeasurementTVP}
\alias{WmlMeasurementTVP-class}

\alias{wmlMeasurementTimeseriesName}

\alias{parseWmlDefaultTVPMeasurementMetadata}
\alias{parseWmlInterpolationType}
\alias{parseWmlMeasurementTVP}
\alias{parseWmlMeasurementTimeseries}
\alias{parseWmlMeasurementTimeseriesMetadata}

\alias{sosResult,WmlMeasurementTVP-method}
\alias{sosResult,WmlMeasurementTimeseries-method}
\alias{sosTime,WmlMeasurementTVP-method}
\alias{sosUOM,WmlDefaultTVPMeasurementMetadata-method}
\alias{sosFeatureIds,WmlMonitoringPoint-method}

\title{Classes, creation, and parsing functions for WaterML}

\description{
WaterML classes.

\code{WmlTimeseries} (abstract)

\code{WmlMeasurementTimeseries}

\code{WmlMeasurementTimeseriesMetadata}

\code{WmlMeasurementTVP}

\code{WmlDefaultTVPMeasurementMetadata}

\code{WmlInterpolationType}

}

\section{Objects from the Class}{
Objects can be created by calls to the creation functions:

\code{WmlMeasurementTimeseries(...)}

\code{WmlMeasurementTimeseriesMetadata(temporalExtent)}

\code{WmlMeasurementTVP(...)}

\code{WmlDefaultTVPMeasurementMetadata(...)}

\code{WmlInterpolationType(...)}
}

\section{Slots}{
  \describe{
    \item{\code{sampledFeatures}:}{Object of class \code{"character"} ~~ }
    \item{\code{id}:}{Object of class \code{"character"} ~~ }
    \item{\code{identifier}:}{Object of class \code{"character"} ~~ }
    \item{\code{names}:}{Object of class \code{"character"} ~~ }
    \item{\code{shape}:}{Object of class \code{"SamsShape"} ~~ }
    \item{\code{timeZone}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{temporalExtent}:}{Object of class \code{"GmlPoint"} ~~ }
    \item{\code{time}:}{Object of class \code{"POSIXct"} in a TVP}
    \item{\code{value}:}{Object of class \code{"numeric"} in a TVP}
    \item{\code{href}:}{Object of class \code{"character"} ~~ }
    \item{\code{title}:}{Object of class \code{"character"} ~~ }
    \item{\code{interpolationType}:}{Object of class \code{"WmlInterpolationType"} ~~ }
    \item{\code{uom}:}{Object of class \code{"character"} ~~ }
    \item{\code{metadata}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{defaultPointMetadata}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{point}:}{Object of class \code{"ANY"} for \code{WmlTimeseries}, of class \code{"list"} for \code{WmlMeasurementTimeseries}}
  }
}

\section{Extends}{
\code{WmlMonitoringPoint} extends \code{"\linkS4class{GmlFeature}"}, directly.
\code{WmlMeasurementTimeseries} extends \code{"\linkS4class{WmlTimeseries}"}, directly.
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
https://www.ogc.org/standards/WaterML
}

\author{
Eike H. Juerrens, Daniel Nuest
}

\note{
Schemas: http://schemas.opengis.net/waterml/
}

\examples{
showClass("WmlMeasurementTimeseries")
}

\keyword{classes}
