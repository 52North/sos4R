################################################################################
# Copyright (C) 2015 by 52 North                                               #
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
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)                          #
# Created: 2010-09-08                                                          #
# Project: sos4R - visit the project web page: https://github.com/52North/sos4R #
#                                                                              #
################################################################################

#
# construction methods
#
GmlTimeInstant <- function(timePosition, id = as.character(NA),
                           relatedTimes = list(NA), frame = as.character(NA)) {
  new("GmlTimeInstant", timePosition = timePosition, id = id,
      relatedTimes = relatedTimes, frame = frame)
}

GmlTimePeriod <- function(begin = NULL, beginPosition = NULL, end = NULL,
                          endPosition = NULL, duration = as.character(NA), timeInterval = NULL,
                          id = as.character(NA), relatedTimes = list(NA),
                          frame = as.character(NA)) {
  new("GmlTimePeriod", begin = begin, beginPosition = beginPosition,
      end = end, endPosition = endPosition, duration = duration,
      timeInterval = timeInterval, id = id, relatedTimes = relatedTimes,
      frame = frame)
}

GmlTimePosition <- function(time, frame = as.character(NA),
                            calendarEraName = as.character(NA),
                            indeterminatePosition = as.character(NA)) {
  new("GmlTimePosition", time = time, frame = frame,
      calendarEraName = calendarEraName,
      indeterminatePosition = indeterminatePosition)
}

GmlTimeInterval <- function(interval, unit, radix = NA_integer_,
                            factor = NA_integer_) {
  new("GmlTimeInterval", interval = interval, unit = unit, radix = radix,
      factor = factor)
}

GmlFeatureProperty <- function(href = as.character(NA), feature = NULL) {
  new("GmlFeatureProperty", href = href, feature = feature)
}

GmlFeatureCollection <- function(featureMembers, id = as.character(NA)) {
  new("GmlFeatureCollection", featureMembers = featureMembers, id = id)
}

GmlDirectPosition <- function(pos, srsName = as.character(NA),
                              srsDimension = NA_integer_, axisLabels = as.character(NA),
                              uomLabels = as.character(NA)) {
  new("GmlDirectPosition", pos = pos, srsName = srsName,
      srsDimension = srsDimension, axisLabels = axisLabels,
      uomLabels = uomLabels)
}

GmlDirectPositionLatLon <- function(lat, lon, srsName = as.character(NA),
                                    srsDimension = NA_integer_, axisLabels = as.character(NA),
                                    uomLabels = as.character(NA)) {
  new("GmlDirectPosition", pos = paste(lat, lon, sep = " "),
      srsName = srsName, srsDimension = srsDimension,
      axisLabels = axisLabels, uomLabels = uomLabels)
}

GmlPoint <- function(pos, id = as.character(NA), srsName = as.character(NA),
                     srsDimension = NA_integer_, axisLabels = as.character(NA),
                     uomLabels = as.character(NA)) {
  new("GmlPoint", pos = pos, id = id, srsName = srsName,
      srsDimension = srsDimension, axisLabels = axisLabels,
      uomLabels = uomLabels)
}

GmlPointProperty <- function(href = as.character(NA), point = NULL) {
  new("GmlPointProperty", href = href, point = point)
}

GmlTimeInstantProperty <- function(href = as.character(NA), time = NULL) {
  new("GmlTimeInstantProperty", href = href, time = time)
}

GmlEnvelope <- function(lowerCorner, upperCorner, srsName = as.character(NA),
                        srsDimension = NA_integer_, axisLabels = as.character(NA),
                        uomLabels = as.character(NA)) {
  new("GmlEnvelope", lowerCorner = lowerCorner, upperCorner = upperCorner,
      srsName = srsName, srsDimension = srsDimension,
      axisLabels = axisLabels, uomLabels = uomLabels)
}

GmlMeasure <- function(value, uom) {
  new("GmlMeasure", value = value, uom = uom)
}


################################################################################
# encoding methods

setMethod(f = "encodeXML",
          signature = signature(obj = "GmlTimeInstantProperty", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose)
              cat("[encodeXML] GmlTimeInstantProperty with", toString(obj),
                  "\n")

            stop("Function encodeXML for GmlTimeInstantProperty not implemented yet!")
          }
)

setMethod(f = "encodeXML",
          signature = signature(obj = "GmlTimeInstant", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose)
              cat("[encodeXML] GmlTimeInstant with", toString(obj), "\n")

            .ti <- XML2::xmlNode(name = gmlTimeInstantName,
                           namespace = gmlNamespacePrefix)
            .time <- encodeXML(obj = obj@timePosition, sos = sos,
                               verbose = verbose)
            .ti$children[[1]] <- .time

            return(.ti)
          }
)

setMethod(f = "encodeXML",
          signature = signature(obj = "GmlTimePosition", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose)
              cat("[encodeXML] GmlTimePosition with", toString(obj), "\n")

            .tpos <- XML2::xmlNode(name = gmlTimePositionName,
                             namespace = gmlNamespacePrefix)

            if (!is.na(obj@frame)) {
              xml2::xml_set_attr(x = .tpos, attr = "frame", value = obj@frame)
            }
            if (!is.na(obj@calendarEraName)) {
              xml2::xml_set_attr(x = .tpos, attr = "calendarEraName", value = obj@calendarEraName)
            }
            if (!is.na(obj@indeterminatePosition)) {
              xml2::xml_set_attr(x = .tpos, attr = "indeterminatePosition", value = obj@indeterminatePosition)
            }

            xml2::xml_text(x = .tpos) <- encodeXML(obj = obj@time, sos = sos, verbose = verbose)

            return(.tpos)
          }
)

setMethod(f = "encodeXML",
          signature = signature(obj = "GmlTimePeriod", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose)
              cat("[encodeXML] GmlTimePeriod with", toString(obj), "\n")

            .tp <- XML2::xmlNode(name = gmlTimePeriodName,
                                 namespace = gmlNamespacePrefix)

            # switch cases: begin and end
            if (!is.null(obj@begin) && !is.null(obj@end)) {
              .begin <- XML2::xmlNode(name = gmlBeginName,
                                namespace = gmlNamespacePrefix)
              .begin$children[[1]] <- encodeXML(obj = obj@begin, sos = sos,
                                                verbose = verbose)
              .end <- XML2::xmlNode(name = gmlEndName,
                              namespace = gmlNamespacePrefix)
              .end$children[[1]] <- encodeXML(obj = obj@end, sos = sos,
                                              verbose = verbose)
              .tp <- XML::addChildren(node = .tp, kids = list(.begin, .end))
            }
            # beginPosition and endPosition
            else if (!is.null(obj@beginPosition) && !is.null(obj@endPosition)) {
              .beginPosition <- encodeXML(obj = obj@beginPosition, sos = sos,
                                          verbose = verbose)
              xml2::xml_name(x = .beginPosition) <- gmlBeginPositionName
              .endPosition <- encodeXML(obj = obj@endPosition, sos = sos,
                                        verbose = verbose)
              xml2::xml_name(x = .endPosition) <- gmlEndPositionName
              .tp <- XML::addChildren(node = .tp,
                                 kids = list(.beginPosition, .endPosition))
            }

            # time duration stuff: prefer duration over timeInterval
            if (!is.na(obj@duration) && !is.null(obj@timeInterval))
              warning("Can only add either duration or timeInterval to gml:TimePeriod, using durcation!")
            if (!is.na(obj@duration)) {
              .timeLength <- XML2::xmlNode(name = gmlTimeLengthName,
                                     namespace = gmlNamespacePrefix)
              .duration <-  XML2::xmlNode(name = gmlDurationName,
                                    namespace = gmlNamespacePrefix)
              xml2::xml_text(x = .duration) <- obj@duration
              .timeLength$children[[1]] <- .duration
              .tp <- XML::addChildren(node = .tp, kids = list(.timeLength))
            }
            else if (!is.null(obj@timeInterval)) {
              .timeLength <- XML2::xmlNode(name = gmlTimeLengthName,
                                     namespace = gmlNamespacePrefix)
              .timeInterval <-  XML2::xmlNode(name = gmlTimeIntervalName,
                                        namespace = gmlNamespacePrefix)

              xml2::xml_text(x = .timeInterval) <- obj@timeInterval@interval

              xml2::xml_set_attr(x = .timeInterval, attr = "unit", value = obj@timeInterval@unit)

              if (!is.na(obj@timeInterval@radix)) {
                xml2::xml_set_attr(x = .timeInterval, attr = "radix", value = obj@timeInterval@radix)
              }

              if (!is.na(obj@timeInterval@factor)) {
                xml2::xml_set_attr(x = .timeInterval, attr = "factor", value = obj@timeInterval@factor)
              }

              .timeLength$children[[1]] <- .timeInterval
              .tp <- XML::addChildren(node = .tp, kids = list(.timeLength))
            }

            return(.tp)
          }
)

setMethod(f = "encodeXML",
          signature = signature(obj = "GmlEnvelope", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] GmlEnvelope with", toString(obj), "\n")

            .env <- XML2::xmlNode(name = gmlEnvelopeName,
                            namespace = gmlNamespacePrefix)

            if (!is.na(obj@srsName)) {
              xml2::xml_set_attr(x = .env, attr = "srsName", value = obj@srsName)
            }

            if (!is.na(obj@srsDimension)) {
              xml2::xml_set_attr(x = .env, attr = "srsDimension", value = obj@srsDimension)
            }

            if (!is.na(obj@axisLabels)) {
              xml2::xml_set_attr(x = .env, attr = "axisLabels", value = obj@axisLabels)
            }

            if (!is.na(obj@uomLabels)) {
              xml2::xml_set_attr(x = .env, attr = "uomLabels", value = obj@uomLabels)
            }

            .lC <- encodeXML(obj = obj@lowerCorner, sos = sos)
            xml2::xml_name(x = .lC) <- gmlLowerCornerName
            .uC <- encodeXML(obj = obj@upperCorner, sos = sos)
            xml2::xml_name(x = .uC) <- gmlUpperCornerName

            .env$children[[1]] <- .lC
            .env$children[[2]] <- .uC

            return(.env)
          }
)

setMethod(f = "encodeXML",
          signature = signature(obj = "GmlDirectPosition", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose)
              cat("[encodeXML] GmlDirectPosition with", toString(obj), "\n")

            .pos <- XML2::xmlNode(name = gmlPosName,
                            namespace = gmlNamespacePrefix)
            xml2::xml_text(x = .pos) <- obj@pos

            if (!is.na(obj@srsName)) {
              xml2::xml_set_attr(x = .pos, attr = "srsName", value = obj@srsName)
            }

            if (!is.na(obj@srsDimension)) {
              xml2::xml_set_attr(x = .pos, attr = "srsDimension", value = obj@srsDimension)
            }

            if (!is.na(obj@axisLabels)) {
              xml2::xml_set_attr(x = .pos, attr = "axisLabels", value = obj@axisLabels)
            }

            if (!is.na(obj@uomLabels)) {
              xml2::xml_set_attr(x = .pos, attr = "uomLabels", value = obj@uomLabels)
            }

            return(.pos)
          }
)

setMethod(f = "encodeXML",
          signature = signature(obj = "GmlPoint", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] GmlPoint with", toString(obj), "\n")

            .point <- XML2::xmlNode(name = gmlPointName,
                              namespace = gmlNamespacePrefix)
            .pos <- encodeXML(obj = obj@pos, sos = sos)
            .point$children[[1]] <- .pos

            return(.point)
          }
)

setMethod(f = "encodeXML",
          signature = signature(obj = "GmlLineString", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] GmlPoint with", toString(obj), "\n")

            warning("FUNCTION NOT IMPLEMENTED!")
          }
)

setMethod(f = "encodeXML",
          signature = signature(obj = "GmlPolygon", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] GmlPoint with", toString(obj), "\n")

            warning("FUNCTION NOT IMPLEMENTED!")
          }
)

setMethod(f = "encodeXML",
          signature = signature(obj = "GmlPointProperty", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] GmlPoint with", toString(obj), "\n")

            warning("FUNCTION NOT IMPLEMENTED!")
          }
)
