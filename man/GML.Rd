\name{GmlDirectPosition-class}
\Rdversion{1.1}

\alias{GmlDirectPosition-class}
\alias{GmlDirectPositionOrNULL-class}
\alias{GmlEnvelope-class}
\alias{GmlFeature-class}
\alias{GmlFeatureCollection-class}
\alias{GmlFeatureOrNULL-class}
\alias{GmlFeatureProperty-class}
\alias{GmlFeatureOrGmlFeaturePropertyOrNULL-class}
\alias{GmlGeometry-class}
\alias{GmlLineString-class}
\alias{GmlPoint-class}
\alias{GmlPointOrNULL-class}
\alias{GmlPointProperty-class}
\alias{GmlPolygon-class}
\alias{GmlTimeGeometricPrimitive-class}
\alias{GmlTimeInstant-class}
\alias{GmlTimeInstantOrNULL-class}
\alias{GmlTimeInstantProperty-class}
\alias{GmlTimeInstantPropertyOrNULL-class}
\alias{GmlTimeInterval-class}
\alias{GmlTimeIntervalOrNULL-class}
\alias{GmlTimeObject-class}
\alias{GmlTimeObjectOrNULL-class}
\alias{GmlTimePeriod-class}
\alias{GmlTimePosition-class}
\alias{GmlTimePositionOrNULL-class}
\alias{GmlTimePrimitive-class}

\alias{show,GmlDirectPosition-method}
\alias{show,GmlEnvelope-method}
\alias{show,GmlFeatureCollection-method}
\alias{show,GmlFeatureProperty-method}
\alias{show,GmlGeometry-method}
\alias{show,GmlPoint-method}
\alias{show,GmlPointProperty-method}
\alias{show,GmlTimeInstant-method}
\alias{show,GmlTimeInterval-method}
\alias{show,GmlTimePeriod-method}
\alias{show,GmlTimePosition-method}

\alias{print,GmlDirectPosition-method}
\alias{print,GmlEnvelope-method}
\alias{print,GmlFeatureCollection-method}
\alias{print,GmlFeatureProperty-method}
\alias{print,GmlGeometry-method}
\alias{print,GmlPoint-method}
\alias{print,GmlPointProperty-method}
\alias{print,GmlTimeInstant-method}
\alias{print,GmlTimeInstantProperty-method}
\alias{print,GmlTimeInterval-method}
\alias{print,GmlTimePeriod-method}
\alias{print,GmlTimePosition-method}

\alias{toString,GmlDirectPosition-method}
\alias{toString,GmlEnvelope-method}
\alias{toString,GmlFeatureCollection-method}
\alias{toString,GmlFeatureProperty-method}
\alias{toString,GmlGeometry-method}
\alias{toString,GmlPoint-method}
\alias{toString,GmlPointProperty-method}
\alias{toString,GmlTimeInstant-method}
\alias{toString,GmlTimeInstantProperty-method}
\alias{toString,GmlTimeInterval-method}
\alias{toString,GmlTimePeriod-method}
\alias{toString,GmlTimePosition-method}

\alias{GmlDirectPosition}
\alias{GmlDirectPositionLatLon}
\alias{GmlEnvelope}
\alias{GmlFeatureCollection}
\alias{GmlPoint}
\alias{GmlPointProperty}
\alias{GmlFeatureProperty}
\alias{GmlTimeInstant}
\alias{GmlTimeInstantProperty}
\alias{GmlTimeInterval}
\alias{GmlTimePeriod}
\alias{GmlTimePosition}

\alias{GmlMeasure-class}
\alias{print,GmlMeasure-method}
\alias{toString,GmlMeasure-method}
\alias{GmlMeasure}
\alias{show,GmlMeasure-method}

\alias{sosCoordinates,GmlDirectPosition-method}
\alias{sosCoordinates,GmlFeatureCollection-method}
\alias{sosCoordinates,GmlFeatureProperty-method}
\alias{sosCoordinates,GmlPoint-method}
\alias{sosCoordinates,GmlPointProperty-method}

\alias{sosId,GmlFeature-method}
\alias{sosSrsName,GmlDirectPosition-method}
\alias{sosSrsName,GmlPoint-method}
\alias{sosFeatureIds,GmlFeatureCollection-method}
\alias{sosFeatureIds,GmlFeatureProperty-method}

\alias{sosFeaturesOfInterest,GmlFeatureCollection-method}

\alias{sosTime,GmlTimeInstant-method}
\alias{sosTime,GmlTimeInstantProperty-method}
\alias{sosTime,GmlTimePeriod-method}
\alias{sosTime,GmlTimePosition-method}

\alias{summary.GmlTimePeriod}
\alias{print.summary.GmlTimePeriod}

\alias{sosUOM,GmlMeasure-method}


\title{Classes and Construction Functions from the GML Namespace}

\description{
Classes for \code{GML} elements and their respective construction functions. See the referenced specification for details.
}

\section{Objects from this classes}{
Objects can be created by calling the according construction functions. e.g. in the form \code{GmlPoint(...)}.
}

\usage{
GmlDirectPosition(pos, srsName = as.character(NA), srsDimension = NA_integer_,
                  axisLabels = as.character(NA), uomLabels = as.character(NA))
GmlDirectPositionLatLon(lat, lon, srsName = as.character(NA),
                        srsDimension = NA_integer_,
                        axisLabels = as.character(NA),
                        uomLabels = as.character(NA))
GmlEnvelope(lowerCorner, upperCorner, srsName = as.character(NA),
            srsDimension = NA_integer_, axisLabels = as.character(NA),
            uomLabels = as.character(NA))
GmlFeatureCollection(featureMembers, id = as.character(NA))
GmlPoint(pos, id = as.character(NA), srsName = as.character(NA),
         srsDimension = NA_integer_, axisLabels = as.character(NA),
         uomLabels = as.character(NA))
GmlPointProperty(href = as.character(NA), point = NULL)
GmlFeatureProperty(href = as.character(NA), feature = NULL)
GmlTimeInstant(timePosition, id = as.character(NA), relatedTimes = list(NA),
               frame = as.character(NA))
GmlTimeInstantProperty(href = as.character(NA), time = NULL)
GmlTimeInterval(interval, unit, radix = NA, factor = NA)
GmlTimePeriod(begin = NULL, beginPosition = NULL, end = NULL,
              endPosition = NULL, duration = as.character(NA),
              timeInterval = NULL, id = as.character(NA),
              relatedTimes = list(NA), frame = as.character(NA))
GmlTimePosition(time, frame = as.character(NA),
                calendarEraName = as.character(NA),
                indeterminatePosition = as.character(NA))
GmlMeasure(value, uom)
}

\section{Extends}{
% TODO: Describe class hierarchy.

GmlFeature:
Class \code{"\linkS4class{GmlFeatureOrNULL}"}, directly.

GmlFeatureCollection:
Class \code{"\linkS4class{GmlFeature}"}, directly.
Class \code{"\linkS4class{GmlFeatureOrNULL}"}, by class "GmlFeature", distance 2.

GmlLineString:
Class \code{"\linkS4class{GmlGeometry}"}, directly.

GmlPoint:
Class \code{"\linkS4class{GmlGeometry}"}, directly.

GmlTimeGeometricPrimitive:
Class \code{"\linkS4class{GmlTimePrimitive}"}, directly.
Class \code{"\linkS4class{GmlTimeObject}"}, by class "GmlTimePrimitive", distance 2.
Class \code{"\linkS4class{GmlTimeObjectOrNULL}"}, by class "GmlTimePrimitive", distance 3.

GmlTimeInstant:
Class \code{"\linkS4class{GmlTimeGeometricPrimitive}"}, directly.
Class \code{"\linkS4class{GmlTimeInstantOrNULL}"}, directly.
Class \code{"\linkS4class{GmlTimePrimitive}"}, by class "GmlTimeGeometricPrimitive", distance 2.
Class \code{"\linkS4class{GmlTimeObject}"}, by class "GmlTimeGeometricPrimitive", distance 3.
Class \code{"\linkS4class{GmlTimeObjectOrNULL}"}, by class "GmlTimeGeometricPrimitive", distance 4.

GmlTimeInstantProperty:
Class \code{"\linkS4class{GmlTimeGeometricPrimitive}"}, directly.
Class \code{"\linkS4class{GmlTimeInstantPropertyOrNULL}"}, directly.
Class \code{"\linkS4class{GmlTimePrimitive}"}, by class "GmlTimeGeometricPrimitive", distance 2.
Class \code{"\linkS4class{GmlTimeObject}"}, by class "GmlTimeGeometricPrimitive", distance 3.
Class \code{"\linkS4class{GmlTimeObjectOrNULL}"}, by class "GmlTimeGeometricPrimitive", distance 4.

GmlTimeInterval:
Class \code{"\linkS4class{GmlTimeIntervalOrNULL}"}, directly.

GmlTimeObject:
Class \code{"\linkS4class{GmlTimeObjectOrNULL}"}, directly.

GmlTimePrimitive:
Class \code{"\linkS4class{GmlTimeObject}"}, directly.
Class \code{"\linkS4class{GmlTimeObjectOrNULL}"}, by class "GmlTimeObject", distance 2.
}

\section{Virtual Classes}{
No objects may be created from the following virtual classes: \code{GmlFeature}, \code{GmlTimeObject}, \code{GmlTimePrimitive}, \code{GmlTimeGeometricPrimitive}, \code{GmlGeometry}, all \code{...OrNULL} classes.
}

\section{Slots}{
  \describe{
    \item{\code{axisLabels}:}{Object of class \code{"character"}: An character vector of labels for all the axes of this CRS.}
    \item{\code{begin}:}{Object of class \code{"GmlTimeInstantPropertyOrNULL"}: An object of class \code{GmlTimeInstantProperty-class}, the start time of a \code{GmlTimePeriod-class}.}
    \item{\code{beginPosition}:}{Object of class \code{"GmlTimePositionOrNULL"}: An object of class \code{GmlTimePosition-class}, the start time of a \code{GmlTimePeriod-class}.}
    \item{\code{calendarEraName}:}{Object of class \code{"character"}: The name of the calendar era.}
    \item{\code{duration}:}{Object of class \code{"character"}: Duration of an interval using ISO 8601 syntax for temporal length.}
    \item{\code{end}:}{Object of class \code{"GmlTimeInstantPropertyOrNULL"}: An object of class \code{GmlTimeInstantProperty-class}, the end time of a \code{GmlTimePeriod-class}.}
    \item{\code{endPosition}:}{Object of class \code{"GmlTimePositionOrNULL"}: An object of class \code{GmlTimePosition-class}, the end time of a \code{GmlTimePeriod-class}.}
    \item{\code{exterior}:}{Object of class \code{"ANY"} for future use in GmlPolygon.}
    \item{\code{factor}:}{Object of class \code{"integer"}: Factor parameter for a \code{GmlTimeInterval-class}. The resolution of the time interval is to one radix ^(-factor) of the specified time unit.}
    \item{\code{feature}:}{Object of class \code{"GmlFeatureOrNULL"}: The directly contained feature in a \code{GmlFeature-class}.}
    \item{\code{featureMembers}:}{Object of class \code{"list"}: A list of \code{GmlFeature-class} in a \code{GmlFeatureCollection-class}.}
    \item{\code{frame}:}{Object of class \code{"character"}: Provides a URI reference that identifies a description of the reference system.}
    \item{\code{href}:}{Object of class \code{"character"}: Reference to a property.}
    \item{\code{id}:}{Object of class \code{"character"}: The identifier/id attribute (\code{gml:id}).}
    \item{\code{indeterminatePosition}:}{Object of class \code{"character"}: Inexact temporal positions may be expressed using the optional indeterminatePosition parameter. This takes one of the following values: after, before, now, unknown.}
    \item{\code{interior}:}{Object of class \code{"ANY"} for future use in GmlPolygon.}
    \item{\code{interval}:}{Object of class \code{"character"}: Interval parameter for a \code{GmlTimeInterval-class}.}
    \item{\code{lowerCorner}:}{Object of class \code{"GmlDirectPosition"}: Object of class \code{GmlDirectPosition-class}, the lower (left) corner of an \code{GmlEnvelope-class}.}
    \item{\code{point}:}{Object of class \code{"ANY"}: An object of class \code{GmlPoint} in a \code{GmlPointProperty-class}.}
    \item{\code{points}:}{Object of class \code{"list"} A list of positions for future use in GmlLineString.}
    \item{\code{posList}:}{Object of class \code{"ANY"}: A list of positions for future use in GmlLineString.}
    \item{\code{pos}:}{Object of class \code{"character"}: Character string containing the coordinates in a \code{GmlDirectPosition-class}, or a \code{GmlDirectPosition-class} in a \code{GmlPoint-class}.}
    \item{\code{poss}:}{Object of class \code{"list"}: A list of positions for future use in GmlLineString.}
    \item{\code{radix}:}{Object of class \code{"integer"}: Radix parameter for a \code{GmlTimeInterval-class}. The resolution of the time interval is to one radix ^(-factor) of the specified time unit.}
    \item{\code{relatedTimes}:}{Object of class \code{"list"}: List of related times.}
    \item{\code{srsDimension}:}{Object of class \code{"integer"}: Dimensions of the coordinate reference system as stated in the coordinate reference system definition., e.g. \samp{2}.}
    \item{\code{srsName}:}{Object of class \code{"character"}: Name of the spatial reference system for bounding box, e.g. \samp{"urn:ogc:def:crs:EPSG:4326"}. }
    \item{\code{timeInterval}:}{Object of class \code{"GmlTimeIntervalOrNULL"}: Time interval element in a \code{GmlTimePeriod-class}, an object of class \code{GmlTimeInterval-class}.}
    \item{\code{time}:}{Object of class \code{"GmlTimeInstantOrNULL"}: The actual time as an object of class \code{POSIXt} in a \code{GmlTimePosition-class}, or an object of class \code{GmlTimeInstant-class} in a \code{GmlTimeInstantProperty-class}.}
    \item{\code{timePosition}:}{Object of class \code{"GmlTimePosition"}: An object of class \code{GmlTimePosition-class} in a \code{GmlTimeInstant-class}.}
    \item{\code{unit}:}{Object of class \code{"character"}: Unit parameter for a \code{GmlTimeInterval-class}.}
    \item{\code{uomLabels}:}{Object of class \code{"character"}: Unit of measurement labels as an ordered character vector for the axes in a bounding box, e.g. \samp{"deg"}.}
    \item{\code{upperCorner}:}{Object of class \code{"GmlDirectPosition"}: Object of class \code{GmlDirectPosition-class}, the upper (right) corner of an \code{GmlEnvelope-class}.}\item{\code{uom}:}{Object of class \code{"character"}: The unit of measurement in a \code{OmMeasure} object.}
    \item{\code{uom}:}{Object of class \code{"character"}: The unit of measurement in a \code{GmlMeasure} object.}
    \item{\code{value}:}{Object of class \code{"numeric"}: The actual value in a \code{GmlMeasure} object.}
  }
}

\section{Methods}{
  \describe{
    \item{encodeXML}{\code{signature(obj = "GmlDirectPosition", sos = "SOS")} or \code{signature(obj = "GmlEnvelope", sos = "SOS")} and more: Convert the given element to an XML respresentaion, and XML \dQuote{encoding}.}
	 }
}

\arguments{
  \item{axisLabels}{See corresponding slot description.}
  \item{begin}{See corresponding slot description.}
  \item{beginPosition}{See corresponding slot description.}
  \item{calendarEraName}{See corresponding slot description.}
  \item{duration}{See corresponding slot description.}
  \item{end}{See corresponding slot description.}
  \item{endPosition}{See corresponding slot description.}
  \item{factor}{See corresponding slot description.}
  \item{feature}{See corresponding slot description.}
  \item{featureMembers}{See corresponding slot description.}
  \item{frame}{See corresponding slot description.}
  \item{href}{See corresponding slot description.}
  \item{id}{See corresponding slot description.}
  \item{indeterminatePosition}{See corresponding slot description.}
  \item{interval}{See corresponding slot description.}
  \item{lat}{Latitude coordinate parameter.}
  \item{lon}{Longitue coordinate parameter.}
  \item{lowerCorner}{See corresponding slot description.}
  \item{point}{See corresponding slot description.}
  \item{pos}{See corresponding slot description.}
  \item{radix}{See corresponding slot description.}
  \item{relatedTimes}{See corresponding slot description.}
  \item{srsDimension}{See corresponding slot description.}
  \item{srsName}{See corresponding slot description.}
  \item{time}{See corresponding slot description.}
  \item{timeInterval}{See corresponding slot description.}
  \item{timePosition}{See corresponding slot description.}
  \item{unit}{See corresponding slot description.}
  \item{uomLabels}{See corresponding slot description.}
  \item{upperCorner}{See corresponding slot description.}
  \item{value}{See slot description.}
  \item{uom}{See slot description.}
}

\value{The construction functions return an object of the respective class.}

\details{
The \verb{"...OrNULL"} classes are used to model optional slots.
}

\references{GML specification at \url{https://www.opengeospatial.org/standards/gml}.}

\author{Daniel Nuest \email{daniel.nuest@uni-muenster.de}}

\examples{
showClass("GmlDirectPosition")
showClass("GmlEnvelope")
showClass("GmlFeature")
showClass("GmlFeatureCollection")
showClass("GmlFeatureOrNULL")
showClass("GmlFeatureProperty")
showClass("GmlGeometry")
showClass("GmlLineString")
showClass("GmlPoint")
showClass("GmlPointProperty")
showClass("GmlPolygon")
showClass("GmlTimeGeometricPrimitive")
showClass("GmlTimeInstant")
showClass("GmlTimeInstantOrNULL")
showClass("GmlTimeInstantProperty")
showClass("GmlTimeInstantPropertyOrNULL")
showClass("GmlTimeInterval")
showClass("GmlTimeIntervalOrNULL")
showClass("GmlTimeObject")
showClass("GmlTimeObjectOrNULL")
showClass("GmlTimePeriod")
showClass("GmlTimePosition")
showClass("GmlTimePositionOrNULL")
showClass("GmlTimePrimitive")

# create direct position
pos1 <- GmlDirectPosition(pos = "7.0 52.0")
show(pos1)

# create envelope
env1 <- GmlEnvelope(upperCorner = pos1,
                    lowerCorner = GmlDirectPosition("6.0 51.0"))
print(env1)

# wrap elements in feature collection
GmlFeatureCollection(id = "001", featureMembers=list(pos1, env1))

# create point with ID
point1 <- GmlPoint(pos = pos1, id = "002")

# create point properties
GmlPointProperty(href = "http://link.to/point")
GmlPointProperty(point = point1)

# time interval of one day
GmlTimeInterval(interval = "1", unit = "d")

# referenced feature
GmlFeatureProperty(href = "http://link.to/feature")

# create a time position and wrap it into a time instant
timePos1 <- GmlTimePosition(time = as.POSIXct("2010-01-01"))

# create direct or referenced time instant
timeInst1 <- GmlTimeInstant(timePosition = timePos1)
timeInst1

GmlTimeInstantProperty(href = "http://link.to/timeInstant")

# create different variants of time periods
# one hour with time positions
GmlTimePeriod(beginPosition = timePos1,
              endPosition = GmlTimePosition(time = timePos1@time+3600))

# one week backwards from now
timePos <- GmlTimePosition(time = Sys.time()-(3600*24*7))
aWeekAgo <- GmlTimeInstantProperty(time = GmlTimeInstant(time = timePos))
timePos <- GmlTimePosition(time = Sys.time())
now <- GmlTimeInstantProperty(time = GmlTimeInstant(time = timePos))
GmlTimePeriod(begin = aWeekAgo, end = now)
}

\keyword{classes}
\keyword{utilities}
