\name{sosCreate}

\docType{methods}

\alias{sosCreate}

\alias{sosCreateBBOX}
\alias{sosCreateBBOX,numeric,numeric,numeric,numeric-method}
\alias{sosCreateBBoxMatrix}
\alias{sosCreateBBoxMatrix,numeric,numeric,numeric,numeric-method}
\alias{sosCreateEventTimeList}
\alias{sosCreateEventTimeList-methods}
\alias{sosCreateEventTimeList,GmlTimeGeometricPrimitive-method}
\alias{sosCreateEventTime}
\alias{sosCreateEventTime-methods}
\alias{sosCreateEventTime,GmlTimeGeometricPrimitive-method}
\alias{sosCreateFeatureOfInterest}
\alias{sosCreateFeatureOfInterest-methods}
\alias{sosCreateFeatureOfInterest,ANY-method}
\alias{sosCreateTimeInstant}
\alias{sosCreateTimeInstant-methods}
\alias{sosCreateTimeInstant,SOS,POSIXt-method}
\alias{sosCreateTimePeriod}
\alias{sosCreateTimePeriod-methods}
\alias{sosCreateTimePeriod,SOS,POSIXt,POSIXt-method}

\title{Convenience Functions for Request Parameter Creations}

\description{
These methods can be seen as convenience functions or shortcuts to regularly used parameters in GetObservation requests to a Sensor Observation Service. The remove some complexity and target the most common cases, but also limit flexibility.
}

\usage{
sosCreateBBOX(lowLat, lowLon, uppLat, uppLon, srsName, srsDimension = NA_integer_, axisLabels = NA_character_, uomLabels = NA_character_, propertyName = sosDefaultSpatialOpPropertyName)
sosCreateBBoxMatrix(lowLat, lowLon, uppLat, uppLon)
sosCreateFeatureOfInterest(objectIDs = list(NA), spatialOps = NULL, bbox = NULL, srsName = NA_character_)
sosCreateEventTime(time, operator)
sosCreateEventTimeList(time, operator)
sosCreateTimeInstant(sos, time, frame = as.character(NA), calendarEraName = as.character(NA), indeterminatePosition = as.character(NA))
sosCreateTimePeriod(sos, begin, end, frame = as.character(NA), calendarEraName = as.character(NA), indeterminatePosition = as.character(NA), duration = as.character(NA), timeInterval = NULL)
}

\arguments{
  \item{lowLat}{Minimum latitude for bounding box and boundinb box matrix.}
  \item{lowLon}{Minimum longitude for bounding box and boundinb box matrix.}
  \item{uppLat}{Maximum latitude for bounding box and boundinb box matrix.}
  \item{uppLon}{Maximum longitude for bounding box and boundinb box matrix.}
  \item{srsName}{Name of the spatial reference system for bounding box, e.g. \samp{"urn:ogc:def:crs:EPSG:4326"}.}
  \item{srsDimension}{Dimensions of the spatial reference system, e.g. \samp{2}.}
  \item{axisLabels}{Labels of the axes of a bounding box as an ordered character vector.}
  \item{uomLabels}{Unit of measurement labels as an ordered character vector for the axes in a bounding box, e.g. \samp{"deg"}.}
  \item{propertyName}{The spatial property name for the bounding box, e.g. \samp{"urn:ogc:data:location"}}
  \item{objectIDs}{Identifiers of a feature of interest list.}
  \item{spatialOps}{An object of class \code{\link{OgcSpatialOps-class}} which is inserted into the feature of interest element.}
  \item{bbox}{Shortcut to add a feature of interest with a \code{\link{GmlEnvelope-class}}, object must be a \code{matrix} as created by \code{sosCreateBBoxMatrix(...)}.}
  \item{time}{Object of class \code{"GmlTimeGeometricPrimitive"} for \code{sosCreateEventTimeList}, or an object of class \code{POSIXt} for \code{sosCreateTimePeriod}.}
  \item{operator}{The operator to be used for the time in \code{sosCreateEventTimeList}, e.g. \samp{"TM_During"}.}
  \item{sos}{An object of class \code{SOS-class} for which the element is created. The \code{SOS} might for example be required for formatting settings.}
  \item{frame}{Provides a URI reference that identifies a description of the reference system.}
  \item{calendarEraName}{The name of the calendar era.}
  \item{begin}{Object of class \code{POSIXt}.}
  \item{end}{Object of class \code{POSIXt}.}
  \item{indeterminatePosition}{Inexact temporal positions may be expressed using the optional indeterminatePosition parameter. This takes one of the following values: after, before, now, unknown.}
  \item{duration}{Duration of an interval using ISO 8601 syntax for temporal length.}
  \item{timeInterval}{An object of class \code{"GmlTimeIntervalOrNULL"} to be used in a \code{GmlTimePeriod-class}.}
}

\value{An object of the respective class, or a list in case of \code{sosEventTimeList}.}

\author{Daniel Nüst \email{daniel.nuest@uni-muenster.de}}

%\note{}

\seealso{These methods create object of the following classes: \code{\link{GmlTimeInstant-class}}, \code{\link{GmlTimePeriod-class}}, \code{\link{SosEventTime-class}}, \code{\link{SosFeatureOfInterest-class}}, \code{\link{OgcBBOX-class}}, \code{\link{matrix-class}}.}

\section{Methods}{
\describe{
	\item{\code{signature(time = "GmlTimeGeometricPrimitive")}}{Create sos:time based on the given \code{GmlTimeGeometricPrimitive}.}
	\item{\code{signature(sos = "SOS", time = "POSIXt")}}{Create sos:time with time instant based on the given time.}
	\item{\code{signature(sos = "SOS", begin = "POSIXt", end = "POSIXt")}}{Create sos:time with time interval based on the given begin and end times.}
}

}

\examples{

# create a feature of interest based on identifiers
foiIDs <- list("urn:ogc:object:feature:1", "urn:ogc:object:feature:2")
foiObj <- sosCreateFeatureOfInterest(objectIDs = foiIDs[1:2])
print(foiObj)

# create a bounding box matrix and use it to create a spatial feature of interest
bboxMatrix <- sosCreateBBoxMatrix(lowLat = 50.0, lowLon = 7.0, uppLat = 53.0, uppLon = 10.0)
foiBBox <- sosCreateFeatureOfInterest(bbox = bboxMatrix, srsName = "urn:ogc:def:crs:EPSG:6.8:4326")
print(foiBBox)

# create a foi with a bounding box
bbox <- sosCreateBBOX(lowLat = 50.0, lowLon = 7.0, uppLat = 53.0, uppLon = 10.0, srsName = "urn:ogc:def:crs:EPSG:6.8:4326", srsDimension = as.integer(2), axisLabels = "lat,lon", uomLabels = "deg,deg", propertyName = "bboxName")
foiBBox2 <- sosCreateFeatureOfInterest(spatialOps = bbox)
print(foiBBox2)

\dontrun{
# time examples TBD
}

}

\keyword{utilities}
\keyword{methods}
