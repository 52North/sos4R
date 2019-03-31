################################################################################
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
################################################################################

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
    pointXml <- xml2::xml_child(x = obj, search = gmlPointName, ns = SosAllNamespaces())
    position <- GmlPointProperty(point = parsePoint(pointXml, sos = sos))
  }

  return(position)
}

parsePoint <- function(obj, sos) {
  point <- NA
  pos <- xml2::xml_child(x = obj, search = gmlPosName)
  posString <- xml2::xml_text(x = pos)

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
  point <- GmlPoint(pos = pos)

  return(point)
}

#
# time parsing ----
#
parseTimeInstant <- function(obj, format) {
  .timePosXML <- xml2::xml_find_first(x = obj, xpath = gmlTimePositionName,
                                      ns = SosAllNamespaces())
  .timePos <- parseTimePosition(obj = .timePosXML, format = format)

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

parseTimeInstantProperty <- function(obj, format) {
  .timeProp <- NULL

  # check if reference or inline phenomenon
  .href <- xml2::xml_attr(x = obj, attr = "href")
  if (!is.na(.href)) {
    .timeProp <- GmlTimeInstantProperty(href = .href)
  }
  else {
    .time <- parseTimeInstant(obj = xml2::xml_child(x = obj), format = format)
    .timeProp <- GmlTimeInstantProperty(time = .time)
  }

  return(.timeProp)
}

parseTimePosition <- function(obj, format) {
  .time <- strptime(xml2::xml_text(x = obj), format)

  # optional:
  .frame <- xml2::xml_attr(x = obj, attr = "frame", default = NA_character_)
  .calendarEraName <- xml2::xml_attr(x = obj, attr = "calendarEraName", default = NA_character_)
  .indeterminatePosition <- xml2::xml_attr(x = obj, attr = "indeterminatePosition", default = NA_character_)

  .timePosition <- GmlTimePosition(time = .time, frame = .frame,
                                   calendarEraName = .calendarEraName,
                                   indeterminatePosition = .indeterminatePosition)
  return(.timePosition)
}

parseTimePeriod <- function(obj, format) {
  .timeObject <- NULL

  # optionals
  .id = xml2::xml_attr(x = obj, attr = "id", default = NA_character_)
  .frame = xml2::xml_attr(x = obj, attr = "frame", default = NA_character_)
  .relatedTimes <- xml2::xml_find_all(x = obj, xpath = gmlRelatedTimeName, ns = SosAllNamespaces())
  if (length(.relatedTimes) < 1)
    .relatedTimes <- list()

  # TODO parse gml:timeLength
  .duration <- NA_character_
  .timeInterval <- NULL

  # begin and end
  if (!is.na(xml2::xml_child(x = obj, search = gmlBeginName, ns = SosAllNamespaces())) ||
      !is.na(xml2::xml_child(x = obj, search = gmlEndName, ns = SosAllNamespaces()))) {
    .begin <- parseTimeInstantProperty(obj = xml2::xml_child(x = obj,
                                                             search = gmlBeginName,
                                                             ns = SosAllNamespaces()),
                                       format = format)
    .end <- parseTimeInstantProperty(xml2::xml_child(x = obj, search = gmlEndName, ns = SosAllNamespaces()), format = format)

    .timeObject <- GmlTimePeriod(begin = .begin, end = .end, duration = .duration,
                                 timeInterval = .timeInterval, id = .id,
                                 relatedTimes = .relatedTimes, frame = .frame)
  }
  # beginPosition and endPosition
  else if (!is.na(xml2::xml_child(x = obj, search = gmlBeginPositionName))
           || !is.na(xml2::xml_child(x = obj, search = gmlEndPositionName))) {
    .beginPosition <- parseTimePosition(
      obj = xml2::xml_child(x = obj, search = gmlBeginPositionName),
      format = format)
    .endPosition <- parseTimePosition(
      obj = xml2::xml_child(x = obj, search = gmlEndPositionName),
      format = format)

    .timeObject <- GmlTimePeriod(beginPosition = .beginPosition,
                                 endPosition = .endPosition, duration = .duration,
                                 timeInterval = .timeInterval, id = .id,
                                 relatedTimes = .relatedTimes, frame = .frame)
  }

  return(.timeObject)
}

#
#
#
parseTimeGeometricPrimitiveFromParent <- function(obj, format) {
  .tiXML <- xml2::xml_find_first(x = obj, xpath = gmlTimeInstantName)
  .tpXML <- xml2::xml_find_first(x = obj, xpath = gmlTimePeriodName)
  .timeObject <- NULL
  if (!is.na(.tiXML)) {
    .timeObject <- parseTimeInstant(obj = .tiXML, format = format)
  }
  else if (!is.na(.tpXML)) {
    .timeObject <- parseTimePeriod(obj = .tpXML, format = format)
  }
  else {
    #		warning(paste("Could not create time from given samplingTime,",
    #						" require gml:TimeInstant or gml:TimePeriod as children."))
    .timeObject <- GmlTimeInstant(timePosition = GmlTimePosition(
      time = as.POSIXct(x = NA)))
  }

  return(.timeObject)
}

#
#
#
parseFeatureCollection <- function(obj, sos) {
  .members <- xml2::xml_find_all(x = obj, xpath = gmlFeatureMemberName, ns = SosAllNamespaces())

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
  .name <- xml2::xml_name(x = .member, ns = SosAllNamespaces())

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
#
#
parseMeasure <- function(obj) {
  .value <- as.numeric(xml2::xml_text(x = obj))
  .uom <- xml2::xml_attr(x = obj, attr = "uom", default = NA_character_)
  .result <- GmlMeasure(.value, .uom)
  return(.result)
}

