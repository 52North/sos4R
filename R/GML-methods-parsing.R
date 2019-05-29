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
# Created: 2010-09-15                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

#
# position parsing ----
#
parsePosition <- function(obj, sos) {
  .position <- NULL

  # has href attribute?
  href <- xml2::xml_attr(x = obj, attr = "href")
  if (!is.na(href)) {
    # position is referenced
    position <- GmlPointProperty(href = href)
  }
  else {
    # must be point
    pointXml <- xml2::xml_child(x = obj, search = gmlPointName, ns = sos@namespaces)
    position <- GmlPointProperty(point = parsePoint(pointXml, sos = sos))
  }

  return(position)
}

parsePoint <- function(obj, sos) {
  point <- NA
  pos <- xml2::xml_child(x = obj, search = gmlPosName, ns = SosAllNamespaces(sos@version))
  posString <- xml2::xml_text(x = pos)
  id <- xml2::xml_attr(x = obj, attr = "id")

  if (sosSwitchCoordinates(sos)) {
    warning("Switching coordinates in Point!")
    orig <- strsplit(x = posString, split = " ")
    posString <- paste(orig[[1]][[2]], orig[[1]][[1]])
  }

  # optional attributes:
  srsName <- xml2::xml_attr(x = pos, attr = "srsName")
  srsDimension <- xml2::xml_attr(x = pos, attr = "srsDimension", default = NA_integer_)
  axisLabels <- xml2::xml_attr(x = pos, attr = "axisLabels")
  uomLabels <- xml2::xml_attr(x = pos, attr = "uomLabels")

  pos <- GmlDirectPosition(pos = posString,
                           srsName = srsName,
                           srsDimension = as.integer(srsDimension),
                           axisLabels = axisLabels,
                           uomLabels = uomLabels)
  point <- GmlPoint(id = id, pos = pos)

  return(point)
}

#
# time parsing ----
#
parseTimeInstant <- function(obj, sos) {
  .timePosXML <- xml2::xml_find_first(x = obj, xpath = gmlTimePositionName,
                                      ns = sos@namespaces)
  .timePos <- parseTimePosition(obj = .timePosXML, sos = sos)

  # optionals
  .id = xml2::xml_attr(x = obj, attr = "id", default = NA_character_)
  .frame = xml2::xml_attr(x = obj, attr = "frame", default = NA_character_)
  .relatedTimes <- xml2::xml_find_all(x = obj, xpath = gmlRelatedTimeName)
  if (length(.relatedTimes) < 1)
    .relatedTimes <- list()

  .ti <- GmlTimeInstant(timePosition = .timePos,
                        id = .id,
                        relatedTimes = .relatedTimes,
                        frame = .frame)
  return(.ti)
}

parseTimeInstantProperty <- function(obj, sos) {
  .timeProp <- NULL

  # check if reference or inline phenomenon
  .href <- xml2::xml_attr(x = obj, attr = "href")
  if (!is.na(.href)) {
    .timeProp <- GmlTimeInstantProperty(href = .href)
  }
  else {
    .time <- parseTimeInstant(obj = xml2::xml_child(x = obj), sos = sos)
    .timeProp <- GmlTimeInstantProperty(time = .time)
  }

  return(.timeProp)
}

parseTimePosition <- function(obj, sos) {
  .indeterminatePosition <- xml2::xml_attr(x = obj, attr = "indeterminatePosition", default = NA_character_)
  if (is.na(.indeterminatePosition))
    .time <- sosConvertTime(xml2::xml_text(x = obj), sos = sos)
  else .time <- parsedate::parse_iso_8601(NA)

  # optional:
  .frame <- xml2::xml_attr(x = obj, attr = "frame", default = NA_character_)
  .calendarEraName <- xml2::xml_attr(x = obj, attr = "calendarEraName", default = NA_character_)

  .timePosition <- GmlTimePosition(time = .time, frame = .frame,
                                   calendarEraName = .calendarEraName,
                                   indeterminatePosition = .indeterminatePosition)
  return(.timePosition)
}

parseTimePeriod <- function(obj, sos) {
  .timeObject <- NULL

  # optionals
  .id = xml2::xml_attr(x = obj, attr = "id", default = NA_character_)
  .frame = xml2::xml_attr(x = obj, attr = "frame", default = NA_character_)
  .relatedTimes <- xml2::xml_find_all(x = obj, xpath = gmlRelatedTimeName, ns = sos@namespaces)
  if (length(.relatedTimes) < 1)
    .relatedTimes <- list()

  # TODO parse gml:timeLength
  .duration <- NA_character_
  .timeInterval <- NULL

  # begin and end
  if (!is.na(xml2::xml_child(x = obj, search = gmlBeginName, ns = sos@namespaces)) ||
      !is.na(xml2::xml_child(x = obj, search = gmlEndName, ns = sos@namespaces))) {
    .begin <- parseTimeInstantProperty(obj = xml2::xml_child(x = obj,
                                                             search = gmlBeginName,
                                                             ns = sos@namespaces),
                                       sos = sos)
    .end <- parseTimeInstantProperty(xml2::xml_child(x = obj, search = gmlEndName, ns = sos@namespaces),
                                     sos = sos)

    .timeObject <- GmlTimePeriod(begin = .begin, end = .end, duration = .duration,
                                 timeInterval = .timeInterval, id = .id,
                                 relatedTimes = .relatedTimes, frame = .frame)
  }
  # beginPosition and endPosition
  else if (!is.na(xml2::xml_child(x = obj, search = gmlBeginPositionName))
           || !is.na(xml2::xml_child(x = obj, search = gmlEndPositionName))) {
    .beginPosition <- parseTimePosition(
      obj = xml2::xml_child(x = obj, search = gmlBeginPositionName),
      sos = sos)
    .endPosition <- parseTimePosition(
      obj = xml2::xml_child(x = obj, search = gmlEndPositionName),
      sos = sos)

    .timeObject <- GmlTimePeriod(beginPosition = .beginPosition,
                                 endPosition = .endPosition, duration = .duration,
                                 timeInterval = .timeInterval, id = .id,
                                 relatedTimes = .relatedTimes, frame = .frame)
  }

  return(.timeObject)
}

parseTimeGeometricPrimitiveFromParent <- function(obj, sos) {
  tiXML <- xml2::xml_find_first(x = obj, xpath = gmlTimeInstantName)
  tpXML <- xml2::xml_find_first(x = obj, xpath = gmlTimePeriodName)
  timeObject <- NULL
  if (!is.na(tiXML)) {
    timeObject <- parseTimeInstant(obj = tiXML, sos = sos)
  }
  else if (!is.na(tpXML)) {
    timeObject <- parseTimePeriod(obj = tpXML, sos = sos)
  }
  else {
    timeObject <- GmlTimeInstant(
      timePosition = GmlTimePosition(
        time = parsedate::parse_iso_8601(NA)))
  }

  return(timeObject)
}

#
# feature collection parsing ----
#
parseFeatureCollection <- function(obj, sos) {
  .members <- xml2::xml_find_all(x = obj, xpath = gmlFeatureMemberName, ns = sos@namespaces)

  .id <- xml2::xml_attr(x = obj, attr = "id", default = NA_character_)

  if (length(.members) > 0) {
    .members <- lapply(.members, .parseFeatureMember, sos = sos)

    .fc <- GmlFeatureCollection(featureMembers = .members, id = .id)
    return(.fc)
  }
  else {
    warning("gml:FeatureCollection could not be parsed, only gml:featureMember elements can be handled.")
    return(NULL)
  }
}
.parseFeatureMember <- function(obj, sos) {
  .member <- xml2::xml_child(x = obj)
  .name <- xml2::xml_name(x = .member, ns = sos@namespaces)

  if (.name == saSamplingPointName) {
    .sp <- parseSamplingPoint(.member, sos = sos)
    .member.parsed <- GmlFeatureProperty(feature = .sp)
  }
  else if (.name == gmlFeatureCollectionName) {
    .member.parsed <- parseFeatureCollection(.member, sos = sos)
  }
  else {
    warning("No handling for given gml:featureMember available, only sa:SamplingPoint is supported!")
  }
  return(.member.parsed)
}

#
# value parsing ----
#
parseMeasure <- function(obj) {
  .value <- as.numeric(xml2::xml_text(x = obj))
  .uom <- xml2::xml_attr(x = obj, attr = "uom", default = NA_character_)
  .result <- GmlMeasure(.value, .uom)
  return(.result)
}


#
# spatial parsing ----
#
parseEnvelope <- function(obj, sos, namespaces = xml2::xml_ns(obj), verbose = FALSE) {
  env <- list(
    srsName = xml2::xml_attr(x = obj, attr = "srsName"),
    lowerCorner = xml2::xml_text(x = xml2::xml_child(x = obj,
                                                     search = gmlLowerCornerName,
                                                     ns = namespaces)),
    upperCorner = xml2::xml_text(x = xml2::xml_child(x = obj,
                                                     search = gmlUpperCornerName,
                                                     ns = namespaces))
  )

  if (verbose) cat("[parseObservationCollection] Parsed envelope:", toString(env), "\n")

  if (sosSwitchCoordinates(sos)) {
    warning("Switching coordinates in envelope!")
    .origLC <- strsplit(x = env[["lowerCorner"]], split = " ")
    .lC <- paste(.origLC[[1]][[2]], .origLC[[1]][[1]])
    .origUC <- strsplit(x = env[["upperCorner"]], split = " ")
    .uC <- paste(.origUC[[1]][[2]], .origUC[[1]][[1]])
    env <- list(srsName = xml2::xml_attr(x = obj, attr = "srsName"),
                lowerCorner = .lC, upperCorner = .uC)
  }

  return(env)
}
