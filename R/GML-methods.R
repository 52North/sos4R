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
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)                          #
# Created: 2010-09-08                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

#
# construction methods ----
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


#
# XML encoding methods ----
#

#
# encodeXML(GmlTimeInstantProperty, SOS) ----
#
setMethod(f = "encodeXML",
          signature = signature(obj = "GmlTimeInstantProperty", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] GmlTimeInstantProperty with", toString(obj), "\n")

            stop("Function encodeXML for GmlTimeInstantProperty not implemented yet!")
          }
)

.gmlUUID <- function() {
  paste0(gmlNamespacePrefix, ":id=\"id_", uuid::UUIDgenerate(), "\"")
}

#
# encodeXML(GmlTimeInstant, SOS) ----
#
setMethod(f = "encodeXML",
          signature = signature(obj = "GmlTimeInstant", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] GmlTimeInstant with", toString(obj), "\n")

            # FIXME: https://github.com/r-lib/xml2/issues/239
            #pos <- xml2::xml_new_root(gmlTimeInstantName,
            #                          xmlns = gmlNamespace)
            ti <- xml2::read_xml(paste0("<", gmlTimeInstantName, " xmlns:", gmlNamespacePrefix, "=\"",
                                        sos@namespaces[[gmlNamespacePrefix]], "\" ",
                                        .gmlUUID(),
                                        " />"))

            time <- encodeXML(obj = obj@timePosition, sos = sos, verbose = verbose)
            # FIXME: manually drop namespace from time position
            #if (xml2::xml_has_attr(x = time, attr = paste0("xmlns:", gmlNamespacePrefix))) {
            #  keptAttrs <- names(xml2::xml_attrs(x = time)) != paste0("xmlns:", gmlNamespacePrefix)
            #  xml2::xml_attrs(x = time) <- xml2::xml_attrs(x = time)[keptAttrs]
            #}

            xml2::xml_add_child(ti, time)

            return(ti)
          }
)

#
# encodeXML(GmlTimePosition, SOS) ----
#
setMethod(f = "encodeXML",
          signature = signature(obj = "GmlTimePosition", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose)
              cat("[encodeXML] GmlTimePosition with", toString(obj), "\n")

            # FIXME: https://github.com/r-lib/xml2/issues/239
            #pos <- xml2::xml_new_root(gmlTimePositionName,
            #                          xmlns = gmlNamespace)
            tpos <- xml2::read_xml(paste0("<", gmlTimePositionName, " xmlns:", gmlNamespacePrefix, "=\"",
                                          sos@namespaces[[gmlNamespacePrefix]], "\" ",
                                          .gmlUUID(),
                                          " />"))

            if (!is.na(obj@frame)) {
              xml2::xml_set_attr(x = tpos, attr = "frame", value = obj@frame)
            }
            if (!is.na(obj@calendarEraName)) {
              xml2::xml_set_attr(x = tpos, attr = "calendarEraName", value = obj@calendarEraName)
            }
            if (!is.na(obj@indeterminatePosition)) {
              xml2::xml_set_attr(x = tpos, attr = "indeterminatePosition", value = obj@indeterminatePosition)
            }

            # encode POSIXt object
            xml2::xml_text(x = tpos) <- encodeXML(obj = obj@time, sos = sos, verbose = verbose)

            return(tpos)
          }
)

#
# encodeXML(GmlTimePeriod, SOS) ----
#
setMethod(f = "encodeXML",
          signature = signature(obj = "GmlTimePeriod", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] GmlTimePeriod with", toString(obj), "\n")

            # FIXME: https://github.com/r-lib/xml2/issues/239
            #tperiod <- xml2::xml_new_root(gmlTimePeriodName,
            #                          xmlns = gmlNamespace)
            tperiod <- xml2::read_xml(paste0("<", gmlTimePeriodName,
                                             " xmlns:", gmlNamespacePrefix, "=\"", sos@namespaces[[gmlNamespacePrefix]], "\" ",
                                             .gmlUUID(),
                                             " />"))

            # switch cases:
            # 1. begin and end
            if (!is.null(obj@begin) && !is.null(obj@end)) {
              stop("Encoding of 'begin'/'end' time period not supported")
              #beginTime <- encodeXML(obj = obj@beginPosition, sos = sos, verbose = verbose)
              #beginPosition <- xml2::xml_add_child(tperiod, gmlBeginPositionName)
              #xml2::xml_add_child(beginPosition, beginTime)
              #endTime <- encodeXML(obj = obj@endPosition, sos = sos, verbose = verbose)
              #endPosition <- xml2::xml_add_child(tperiod, gmlEndPositionName)
              #xml2::xml_add_child(endPosition, endTime)
            }
            # 2. beginPosition and endPosition
            else if (!is.null(obj@beginPosition) && !is.null(obj@endPosition)) {
              if (verbose) cat("[encodeXML] GmlTimePeriod beginPosition/endPosition found\n")

              beginTimeString <- encodeXML(obj = obj@beginPosition@time, sos = sos, verbose = verbose)
              xml2::xml_add_child(tperiod, gmlBeginPositionName, beginTimeString)
              endTimeString <- encodeXML(obj = obj@endPosition@time, sos = sos, verbose = verbose)
              xml2::xml_add_child(tperiod, gmlEndPositionName, endTimeString)
            }

            # time duration stuff: prefer duration over timeInterval
            if (!is.na(obj@duration) && !is.null(obj@timeInterval))
              warning("Can only add either duration or timeInterval to gml:TimePeriod, using duration!")

            if (!is.na(obj@duration)) {
              if (verbose) cat("[encodeXML] GmlTimePeriod duration found\n")

              timeLength <- xml2::xml_add_child(tperiod, gmlTimeLengthName)
              duration <- xml2::xml_add_child(timeLength, gmlDurationName)
              xml2::xml_text(x = duration) <- obj@duration
            }
            else if (!is.null(obj@timeInterval)) {
              if (verbose) cat("[encodeXML] GmlTimePeriod time interval found\n")

              timeLength <- xml2::xml_add_child(tperiod, gmlTimeLengthName)
              timeInterval <- xml2::xml_add_child(timeLength, gmlTimeIntervalName)

              xml2::xml_text(x = timeInterval) <- obj@timeInterval@interval
              xml2::xml_set_attr(x = timeInterval, attr = "unit", value = obj@timeInterval@unit)

              if (!is.na(obj@timeInterval@radix)) {
                xml2::xml_set_attr(x = timeInterval, attr = "radix", value = obj@timeInterval@radix)
              }

              if (!is.na(obj@timeInterval@factor)) {
                xml2::xml_set_attr(x = timeInterval, attr = "factor", value = obj@timeInterval@factor)
              }

            }

            return(tperiod)
          }
)

#
# encodeXML(GmlEnvelope, SOS) ----
#
setMethod(f = "encodeXML",
          signature = signature(obj = "GmlEnvelope", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] GmlEnvelope with", toString(obj), "\n")

            # FIXME: https://github.com/r-lib/xml2/issues/239
            #pos <- xml2::xml_new_root(gmlEnvelopeName,
            #                          xmlns = gmlNamespace)
            if (sos@version == sos100_version) {
              env <- xml2::read_xml(paste0("<", gmlEnvelopeName, " xmlns:", gmlNamespacePrefix, "=\"",
                                           sos@namespaces[[gmlNamespacePrefix]], "\" ",
                                           " />"))
            } else {
              env <- xml2::read_xml(paste0("<", gmlEnvelopeName, " xmlns:", gmlNamespacePrefix, "=\"",
                                           sos@namespaces[[gmlNamespacePrefix]], "\" ",
                                           .gmlUUID(),
                                           " />"))
            }

            if (!is.na(obj@srsName)) {
              xml2::xml_set_attr(x = env, attr = "srsName", value = obj@srsName)
            }

            if (!is.na(obj@srsDimension)) {
              xml2::xml_set_attr(x = env, attr = "srsDimension", value = obj@srsDimension)
            }

            if (!is.na(obj@axisLabels)) {
              xml2::xml_set_attr(x = env, attr = "axisLabels", value = obj@axisLabels)
            }

            if (!is.na(obj@uomLabels)) {
              xml2::xml_set_attr(x = env, attr = "uomLabels", value = obj@uomLabels)
            }

            #lowerPos <- encodeXML(obj = obj@lowerCorner, sos = sos)
            lowerCorner <- xml2::xml_add_child(env, gmlLowerCornerName)
            xml2::xml_text(lowerCorner) <- obj@lowerCorner@pos

            #upperPos <- encodeXML(obj = obj@upperCorner, sos = sos)
            upperCorner <- xml2::xml_add_child(env, gmlUpperCornerName)
            xml2::xml_text(upperCorner) <- obj@upperCorner@pos

            if (verbose) cat("[encodeXML] Encoded: ", toString(env), "\n")
            return(env)
          }
)

#
# encodeXML(GmlDirectPosition, SOS) ----
#
setMethod(f = "encodeXML",
          signature = signature(obj = "GmlDirectPosition", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] GmlDirectPosition with", toString(obj), "\n")

            # FIXME: https://github.com/r-lib/xml2/issues/239
            #pos <- xml2::xml_new_root(gmlPosName,
            #                          obj@pos,
            #                          xmlns = gmlNamespace)
            pos <- xml2::read_xml(paste0("<", gmlPosName, " xmlns:", gmlNamespacePrefix, "=\"",
                                         sos@namespaces[[gmlNamespacePrefix]], "\" ",
                                         .gmlUUID(),
                                         " />"))
            xml2::xml_text(pos) <- obj@pos

            if (!is.na(obj@srsName)) {
              xml2::xml_set_attr(x = pos, attr = "srsName", value = obj@srsName)
            }

            if (!is.na(obj@srsDimension)) {
              xml2::xml_set_attr(x = pos, attr = "srsDimension", value = obj@srsDimension)
            }

            if (!is.na(obj@axisLabels)) {
              xml2::xml_set_attr(x = pos, attr = "axisLabels", value = obj@axisLabels)
            }

            if (!is.na(obj@uomLabels)) {
              xml2::xml_set_attr(x = pos, attr = "uomLabels", value = obj@uomLabels)
            }

            return(pos)
          }
)

#
# encodeXML(GmlPoint, SOS) ----
#
setMethod(f = "encodeXML",
          signature = signature(obj = "GmlPoint", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] GmlPoint with", toString(obj), "\n")

            # FIXME: https://github.com/r-lib/xml2/issues/239
            #point <- xml2::xml_new_root(gmlPointName,
            #                            gml = gmlNameName)
            point <- xml2::read_xml(paste0("<", gmlPointName, " xmlns:", gmlNamespacePrefix, "=\"",
                                           sos@namespaces[[gmlNamespacePrefix]], "\" ",
                                           .gmlUUID(),
                                           " />"))

            position <- encodeXML(obj = obj@pos, sos = sos)
            xml2::xml_add_child(point, position)

            return(point)
          }
)

#
# encodeXML(GmlLineString, SOS) ----
#
setMethod(f = "encodeXML",
          signature = signature(obj = "GmlLineString", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] GmlPoint with", toString(obj), "\n")

            warning("Encoding for GmlLineString NOT IMPLEMENTED!")
          }
)

#
# encodeXML(GmlPolygon, SOS) ----
#
setMethod(f = "encodeXML",
          signature = signature(obj = "GmlPolygon", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] GmlPoint with", toString(obj), "\n")

            warning("Encoding for GmlPolygon NOT IMPLEMENTED!")
          }
)

#
# encodeXML(GmlPointProperty, SOS) ----
#
setMethod(f = "encodeXML",
          signature = signature(obj = "GmlPointProperty", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] GmlPoint with", toString(obj), "\n")

            warning("Encoding for GmlPointProperty NOT IMPLEMENTED!")
          }
)

#
# KVP encoding methods ----
#
#
# encodeKVP(GmlTimeInstant, SOS) ----
#
setMethod(f = "encodeKVP",
          signature = signature(obj = "GmlTimeInstant", sos = "SOS"),
          definition = function(obj, sos, verbose = FALSE) {
            if (verbose) cat("[encodeKVP] ", class(obj), "\n")
            time <- encodeKVP(obj = obj@timePosition, sos = sos, verbose = verbose)
            return(time)
          }
)
#
# encodeKVP(GmlTimePosition, SOS) ----
#
setMethod(f = "encodeKVP",
          signature = signature(obj = "GmlTimePosition", sos = "SOS"),
          definition = function(obj, sos, verbose = FALSE) {
            if (verbose) cat("[encodeKVP] ", class(obj), "\n")
            time <- encodeKVP(obj = obj@time, sos = sos, verbose = verbose)
            return(time)
          }
)
#
# encodeKVP(GmlTimePeriod, SOS) ----
#
setMethod(f = "encodeKVP",
          signature = signature(obj = "GmlTimePeriod", sos = "SOS"),
          definition = function(obj, sos, verbose = FALSE) {
            if (verbose) cat("[encodeKVP] ", class(obj), "\n")

            if (!is.null(obj@begin) && !is.null(obj@end)) {
              stop("Encoding of 'begin'/'end' time period not supported")
            }

            if (!is.null(obj@beginPosition) && !is.null(obj@endPosition)) {
              if (verbose) cat("[encodeXML] GmlTimePeriod beginPosition/endPosition found, SOS version:", sos@version, " \n")

              if (sos@version == sos100_version)
                separator = "::"
              else if (sos@version == sos200_version)
                separator = "/"
              else stop("Unsupported SOS version, don't know separator.")

              beginTimeString <- encodeKVP(obj = obj@beginPosition@time, sos = sos, verbose = verbose)
              endTimeString <- encodeKVP(obj = obj@endPosition@time, sos = sos, verbose = verbose)
              return(paste0(beginTimeString, separator, endTimeString))
            }

            stop("Unsupport GmlTimePeriod: ", toString(obj))
          }
)

