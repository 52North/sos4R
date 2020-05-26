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
# Created: 2010-06-18                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

#
# Function parses om:OM_Observation elements from sos:observationData and sos:observation elements.
#
parseObservation_2.0 <- function(obj, sos, verbose = FALSE, retrieveFOI = TRUE) {
  if (verbose) cat("[parseObservation_2.0] Parsing", xml2::xml_name(obj), "\n")

  if (verbose) cat("[parseObservation_2.0] Clearing caches\n")
  assign(x = "featureCache", value = list(), envir = get("sos4R_caches"))
  assign(x = "timeObjectCache", value = list(), envir = get("sos4R_caches"))

  observationXml <- xml2::xml_child(x = obj, search = om20OM_Observation, ns = sos@namespaces)

  id <- xml2::xml_attr(x = observationXml, attr = "id", default = NA_character_)
  if (verbose) cat("[parseObservation_2.0]", id, "\n")

  # 52N SOS only returns om:Observation with procedure ids xlink:href
  procedure <- xml2::xml_attr(x = xml2::xml_child(x = observationXml, search = omProcedureName, ns = sos@namespaces), attr = "href",
                           default = NA_character_)

  observedProperty <- parsePhenomenonProperty(xml2::xml_child(x = observationXml, search = omObservedPropertyName, ns = sos@namespaces),
                                              verbose = verbose)

  phenomenonTimeXml <- xml2::xml_child(x = observationXml, search = om20PhenomenonTimeName, ns = sos@namespaces)
  if (!is.na(phenomenonTimeXml)) {
    phenomenonTime <- parseTimeObject(obj = phenomenonTimeXml,
                          sos = sos,
                          verbose = verbose)
  } else {
    warning("om:phenomenonTime is mandatory in om:Observation, but is missing!")
    phenomenonTime <- NULL
  }

  featureOfInterestXml <- xml2::xml_child(x = observationXml, search = omFeatureOfInterestName, ns = sos@namespaces)
  if (!is.na(featureOfInterestXml)) {
    featureOfInterest <- parseFOI(obj = featureOfInterestXml,
                                  sos = sos,
                                  verbose = verbose)
    if (!is.null(featureOfInterest@href)) {
      if (verbose) cat("[parseObservation_2.0] resolving referenced featureOfInterest\n")

      featureCache <- get(x = "featureCache", envir = get("sos4R_caches"))
      if (startsWith("#", featureOfInterest@href)) {
        cachedFeature <- featureCache[[substring(text = featureOfInterest@href, first = 2)]]
      } else {
        cachedFeature <- featureCache[[featureOfInterest@href]]
      }

      if (is.null(cachedFeature)) {
        if (retrieveFOI) {
          if (verbose) cat("[parseObservation_2.0] referenced featureOfInterest is not cached, retrieving... \n")
          foiList <- sos4R::getFeatureOfInterest(sos = sos, featureOfInterest = featureOfInterest@href, verbose = verbose)
          # getFeatureOfInterest returns a list, but a request including a featureOfInterest id
          # should return only one featureOfInterest
          if (length(foiList) == 1) {
            featureOfInterest <- foiList[[1]]

            featureCache <- get(x = "featureCache", envir = get("sos4R_caches"))

            if (!is.na(featureOfInterest@href)) {
              featureCache[[featureOfInterest@href]] <- featureOfInterest
            }
            if (!is.na(featureOfInterest@feature@identifier)) {
              featureCache[[featureOfInterest@feature@identifier]] <- featureOfInterest
            }
            # in-document use gml:id =: id as href
            if (!is.na(featureOfInterest@feature@id)) {
              featureCache[[featureOfInterest@feature@id]] <- featureOfInterest
            }

            assign(x = "featureCache", value = featureCache, envir = get("sos4R_caches"))

            if (verbose) cat("[parseObservation_2.0] Retrieved FOI: ", toString(featureOfInterest), "\n")
          } else {
            stop("Retrieved multiple FOIs for ", featureOfInterest@href, " - cannot resolve FOI for observation.")
          }
        } else {
          if (verbose) cat("[parseObservation_2.0] Retrieval of FOI disabled.\n")
        }
      } else {
        featureOfInterest <- cachedFeature
      }
    }
  } else {
    warning("om:featureOfInterest is mandatory in om:Observation, but is missing!")
    featureOfInterest <- NULL
  }

  # result parser is exchangeable
  resultParsingFunction <- sosParsers(sos)[[omResultName]]
  result <- resultParsingFunction(xml2::xml_child(x = observationXml, search = omResultName, ns = sos@namespaces),
                                  sos = sos,
                                  verbose = verbose)

  # optional elements
  resultTimeXml <- xml2::xml_child(x = observationXml, search = omResultTimeName, ns = sos@namespaces)
  if (!is.na(resultTimeXml)) {
    resultTime <- parseTimeObject(obj = resultTimeXml,
                          sos = sos,
                          verbose = verbose)
  }
  else {
    resultTime <- NULL
  }

  # TODO optionals elements for OmObservation
  #.metadata
  #.resultQuality
  #.parameter
  #.metadata

  obs <- OmOM_Observation(phenomenonTime = phenomenonTime,
                           resultTime = resultTime,
                           procedure = procedure,
                           observedProperty = observedProperty,
                           featureOfInterest = featureOfInterest,
                           result = result)

  if (verbose) cat("[parseObservation_2.0] Clearing caches\n")
  assign(x = "featureCache", value = list(), envir = get("sos4R_caches"))
  assign(x = "timeObjectCache", value = list(), envir = get("sos4R_caches"))

  return(obs)
}

#
# create according GmlTimeObject from om:samplingTime/om:resultTime/om:phenomenonTime
#
parseTimeObject <- function(obj, sos, verbose = FALSE) {
  if (verbose) cat("[parseTimeObject]\n")

  tiXML <- xml2::xml_find_first(x = obj, xpath = gmlTimeInstantName)
  tpXML <- xml2::xml_find_first(x = obj, xpath = gmlTimePeriodName)
  timeReference <- xml2::xml_attr(x = obj, attr = "href")
  timeObject <- NULL

  timeObjectCache <- get(x = "timeObjectCache", envir = get("sos4R_caches"))

  if (!is.na(tiXML)) {
    if (verbose) cat("[parseTimeObject] time instant.\n")
    timeObject <- parseTimeInstant(obj = tiXML, sos = sos)
    timeObjectCache[[timeObject@id]] <- timeObject
  }
  else if (!is.na(tpXML)) {
    if (verbose) cat("[parseTimeObject] time period.\n")
    timeObject <- parseTimePeriod(obj = tpXML, sos = sos)
    timeObjectCache[[timeObject@id]] <- timeObject
  }
  else if (!is.na(timeReference)) {
    if (verbose) cat("[parseTimeObject] referenced time.\n")
    timeObject <- timeObjectCache[[substring(timeReference, 2)]]
    if (is.null(timeObject)) {
      stop(paste0("XML document invalid. Time reference '", timeReference ,"' not in document."))
    }
  }
  else {
    warning("Could not create GmlTimeObject from given O&M time object.
            Require gml:TimeInstant or gml:TimePeriod as children.")
    timeObject <- GmlTimeInstant(timePosition = GmlTimePosition(
      time = parsedate::parse_iso_8601(NA)))
  }

  assign(x = "timeObjectCache", value = timeObjectCache, envir = get("sos4R_caches"))

  return(timeObject)
}
