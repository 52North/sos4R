############################################################################## #
# Copyright (C) 2019 by 52 North                                               #
# Initiative for Geospatial Open Source Software GmbH                          #
#                                                                              #
# Contact: Andreas Wytzisk                                                     #
# 52 North Initiative for Geospatial Open Source Software GmbH                 #
# Martin-Luther-King-Weg 24                                                    #
# 48155 Muenster, Germany                                                      #
# info@52north.org                                                             #
#                                                                              #
# This program is free software; you can redistribute and/or modify it under   #
# the terms of the GNU General Public License version 2 as published by the    #
# Free Software Foundation.                                                    #
#                                                                              #
# This program is distributed WITHOUT ANY WARRANTY; even without the implied   #
# WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU #
# General Public License for more details.                                     #
#                                                                              #
# You should have received a copy of the GNU General Public License along with #
# this program (see gpl-2.0.txt). If not, write to the Free Software           #
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or #
# visit the Free Software Foundation web page, http://www.fsf.org.             #
#                                                                              #
# Author: Benjamin Pross (b.pross@52north.org)                                 #
#         Eike Hinderk Jürrens (e.h.juerrens@52north.org)                      #
# Created: 2016-01-27                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

#
# parsing functions ----
#
parseWmlMonitoringPoint <- function(obj, sos, verbose = FALSE) {
  sampledFeaturesXml <- xml2::xml_find_all(x = obj, xpath = samSampledFeatureName, ns = sos@namespaces)
  sampledFeatures <- sapply(X = sampledFeaturesXml, FUN = function(feature) {
    link <- xml2::xml_attr(x = feature, attr = "xlink:href", ns = sos@namespaces)
    title <- xml2::xml_attr(x = feature, attr = "xlink:title", ns = sos@namespaces)
    names(link) <- title
    return(link)
  })
  id <- xml2::xml_attr(x = obj, attr = "gml:id", default = NA_character_, ns = sos@namespaces)
  names <- xml2::xml_text(xml2::xml_find_all(x = obj, xpath = gmlNameName, ns = sos@namespaces))
  identifier <- xml2::xml_text(xml2::xml_find_all(x = obj, xpath = gmlIdentifierName, ns = sos@namespaces))
  shape <- parseSamsShape(obj = xml2::xml_find_all(x = obj, xpath = samsShapeName, ns = sos@namespaces), sos = sos)
  verticalDatums <- xml2::xml_find_all(x = obj, xpath = wmlVerticalDatumName, ns = sos@namespaces)
  timeZone <-   xml2::xml_find_first(x = obj, xpath = wmlTimeZoneName, ns = sos@namespaces)

  mp <- WmlMonitoringPoint(sampledFeatures = sampledFeatures,
                           id = id,
                           identifier = identifier,
                           names = names,
                           shape = shape,
                           verticalDatums = verticalDatums,
                           timeZone = timeZone)

  return(mp)
}

#
# <wml2:metadata>
#   <wml2:MeasurementTimeseriesMetadata>
#     <wml2:temporalExtent>
#       <gml:TimePeriod xmlns:gml="http://www.opengis.net/gml/3.2" gml:id="phenomenonTime_1">
#         <gml:beginPosition>2019-01</gml:beginPosition>
#         <gml:endPosition>2019-06</gml:endPosition>
#       </gml:TimePeriod>
#     </wml2:temporalExtent>
#   </wml2:MeasurementTimeseriesMetadata>
# </wml2:metadata>
#
# <wml2:metadata>
#   <wml2:MeasurementTimeseriesMetadata>
#     <wml2:temporalExtent xlink:href="#referenced-timestamp" />
#   </wml2:MeasurementTimeseriesMetadata>
# </wml2:metadata>
#
parseWmlMeasurementTimeseriesMetadata <- function(obj, sos, verbose = FALSE) {
  temporalExtentNode <- xml2::xml_child(obj, search = "wml2:temporalExtent", ns = sos@namespaces)
  if (xml2::xml_has_attr(temporalExtentNode, "xlink:href", ns = sos@namespaces)) {
    href <- xml2::xml_attr(temporalExtentNode, "xlink:href", ns = sos@namespaces) # remove this line
    refNode <- gmlGetReferencedNode(sos = sos, doc = xml2::xml_root(obj), node = temporalExtentNode, verbose = verbose)
    stopifnot(!is.na(refNode), !is.null(refNode), is(refNode, "xml_node"))
    timePeriodNode <- xml2::xml_child(refNode, search = "gml:TimePeriod", ns = sos@namespaces)
    timeInstantNode <- xml2::xml_child(refNode, search = "gml:TimeInstant", ns = sos@namespaces)
  } else {
    timePeriodNode <- xml2::xml_child(obj, search = "wml2:temporalExtent/gml:TimePeriod", ns = sos@namespaces)
    timeInstantNode <- xml2::xml_child(obj, search = "wml2:temporalExtent/gml:TimeInstant", ns = sos@namespaces)
  }
  stopifnot(is(timePeriodNode, "xml_node") || is(timeInstantNode, "xml_node"))
  if (is(timePeriodNode, "xml_node")) {
    temporalExtent <- parseTimePeriod(timePeriodNode, sos)
  } else {
    temporalExtent <- parseTimeInstant(timeInstantNode, sos)
  }
  return(WmlMeasurementTimeseriesMetadata(temporalExtent = temporalExtent))
}

#
# <wml2:interpolationType xlink:href="http://www.opengis.net/def/timeseriesType/WaterML/2.0/continuous" xlink:title="Instantaneous"/>
#
parseWmlInterpolationType <- function(obj, sos, verbose = FALSE) {
  href <- xml2::xml_attr(obj, "xlink:href", ns = sos@namespaces)
  stopifnot(is(href, "character"), nchar(href) > 0)
  title <- xml2::xml_attr(obj, "xlink:title", ns = sos@namespaces)
  stopifnot(is(title, "character"), nchar(title) > 0)
  return(WmlInterpolationType(href = href, title = title))
}

#
# <wml2:defaultPointMetadata>
#   <wml2:DefaultTVPMeasurementMetadata>
#     <wml2:uom code="m^3/s"/>
#     <wml2:interpolationType xlink:href="http://www.opengis.net/def/timeseriesType/WaterML/2.0/continuous" xlink:title="Instantaneous"/>
#   </wml2:DefaultTVPMeasurementMetadata>
# </wml2:defaultPointMetadata>
#
parseWmlDefaultTVPMeasurementMetadata <- function(obj, sos, verbose = FALSE) {
  uomNode <- xml2::xml_find_first(obj, xpath = "wml2:uom")
  uom <- xml2::xml_attr(uomNode, "code", ns = sos@namespaces)
  stopifnot(is(uom, "character"), nchar(uom) > 0)
  interpolationTypeXml <- xml2::xml_find_first(obj, xpath = "wml2:interpolationType")
  interpolationType <- parseWmlInterpolationType(interpolationTypeXml, sos)
  stopifnot(isS4(interpolationType), is(interpolationType, "WmlInterpolationType"))
  return(WmlDefaultTVPMeasurementMetadata(uom = uom, interpolationType = interpolationType))
}

#
# <wml2:point>
#   <wml2:MeasurementTVP>
#     <wml2:time>2019-05-17T07:00:00.000+12:00</wml2:time>
#     <wml2:value>5.3</wml2:value>
#   </wml2:MeasurementTVP>
# </wml2:point>
#
parseWmlMeasurementTVP <- function(obj, sos, verbose = FALSE) {
  timeNode <- xml2::xml_child(obj, search = "wml2:time", ns = sos@namespaces)
  timeString <- xml2::xml_text(timeNode, trim = TRUE)
  time <- parsedate::parse_iso_8601(timeString)
  valueNode <- xml2::xml_child(obj, search = "wml2:value", ns = sos@namespaces)
  value <- xml2::xml_double(valueNode)
  return(WmlMeasurementTVP(time = time, value = value))
}

parseWmlMeasurementTimeseries <- function(obj, sos, verbose = FALSE) {
  id <- xml2::xml_attr(x = obj, attr = "gml:id", ns = sos@namespaces)
  #
  # metadata
  #
  metadataNode <- xml2::xml_child(obj, search = "wml2:metadata/wml2:MeasurementTimeseriesMetadata", ns = sos@namespaces)
  metadata <- parseWmlMeasurementTimeseriesMetadata(metadataNode, sos)
  stopifnot(isS4(metadata), is(metadata, "WmlMeasurementTimeseriesMetadata"))
  #
  # defaultPointMetadata
  #
  defaultPointMetadataNode <- xml2::xml_child(obj, search = "wml2:defaultPointMetadata/wml2:DefaultTVPMeasurementMetadata", ns = sos@namespaces)
  defaultPointMetadata <- parseWmlDefaultTVPMeasurementMetadata(defaultPointMetadataNode, sos)
  stopifnot(isS4(defaultPointMetadata), is(defaultPointMetadata, "WmlDefaultTVPMeasurementMetadata"))
  #
  # points
  #
  pointNodes <- xml2::xml_find_all(obj, xpath = "wml2:point/wml2:MeasurementTVP", ns = sos@namespaces)
  points <- sapply(pointNodes, parseWmlMeasurementTVP, sos)
  stopifnot(is(points, "list"), length(points) > 0, all(sapply(points, inherits, what = "WmlMeasurementTVP")))
  #
  mt <- WmlMeasurementTimeseries(id = id, metadata = metadata, defaultPointMetadata = defaultPointMetadata, points = points)
  return(mt)
}

#
# coercion methods ----
#
as.SpatialPoints.WmlMonitoringPoint <- function(from) {
  as(from@shape, "SpatialPoints")
}
setAs(from = "WmlMonitoringPoint", to = "SpatialPoints",
      def = function(from) {
        as.SpatialPoints.WmlMonitoringPoint(from)
      }
)
setAs(from = "WmlMonitoringPoint", to = "Spatial",
      def = function(from) {
        as.SpatialPoints.WmlMonitoringPoint(from)
      }
)

as.data.frame.WmlMeasurementTimeseries <- function(x, row.names, optional, ...) {
  # cols: timestamp, value <- should be renamed afterwards
  df <- data.frame("timestamp" = double(0), "value" = double(0), stringsAsFactors = FALSE)
  for (wmlMTVP in x@points) {
    df <- rbind(df, data.frame("timestamp" = wmlMTVP@time,
                                      "value" = wmlMTVP@value,
                                      stringsAsFactors = FALSE))
  }
  attr(df, "metadata") <- x@metadata
  attr(df, "defaultPointMetadata") <- x@defaultPointMetadata
  return(df)
}
setAs(from = "WmlMeasurementTimeseries", to = "data.frame",
      def = function(from) {
        as.data.frame.WmlMeasurementTimeseries(x = from)
      }
)

#
# accessor methods ----
#

#
# sosTime(WmlMeasurementTVP) ----
#
setMethod(f = "sosTime",
          signature = signature(obj = "WmlMeasurementTVP"),
          definition = function(obj) {
            return(as(obj@time, "POSIXct"))
          })

#
# sosResult(WmlMeasurementTVP) ----
#
setMethod(f = "sosResult",
          signature = signature(obj = "WmlMeasurementTVP"),
          definition = function(obj) {
            result <- sosResult(obj@value)
            return(result)
          })

#
# sosResult(WmlMeasurementTimeseries) ----
#
setMethod(f = "sosResult", signature = signature(obj = "WmlMeasurementTimeseries"),
          definition = function(obj, coordinates = FALSE) {
            result <- as(obj, "data.frame")

            if (!is.null(obj@defaultPointMetadata)
                && !is.null(obj@defaultPointMetadata@uom)) {
              uom <- obj@defaultPointMetadata@uom
              attributes(result) <- c(attributes(result), list("uom" = uom))
            }

            if (!is.null(obj@defaultPointMetadata@interpolationType)
                && !is.null(obj@defaultPointMetadata@interpolationType@href)) {
              interpolationType <- obj@defaultPointMetadata@interpolationType@href
              attributes(result) <- c(attributes(result), list("interpolationType" = interpolationType))
            }

            if (coordinates){
              warning("Coordinates not supported for this result class.")
            }

            return(result)
          })

#
# sosFeatureIds(WmlMonitoringPoint) ----
#
setMethod(f = "sosFeatureIds",
          signature = signature(obj = "WmlMonitoringPoint"),
          definition = function(obj) {
            return(obj@identifier)
          })
