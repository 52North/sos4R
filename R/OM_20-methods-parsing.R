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
# Created: 2010-06-18                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
################################################################################

#
# Function extracts om:OM_Observation elements from sos:observationData elements.
#
parseObservation_2.0 <- function(obj, sos, featureCache, verbose = FALSE) {

  obj <- xml2::xml_child(x = obj, search = om20OM_Observation, ns = SosAllNamespaces())

  .id <- xml2::xml_attr(x = obj, attr = "id",
                    default = NA_character_)
  if(verbose) cat("[parseObservation]", .id, "\n")

  #TODO adjust the following OM 1.0 parsing functionality

  # 52N SOS only returns om:Observation with procedure ids xlink:href
  .procedure <- xml2::xml_attr(x = xml2::xml_child(x = obj, search = omProcedureName, ns = SosAllNamespaces()), attr = "href",
                           default = NA_character_)

  .observedProperty <- parsePhenomenonProperty(xml2::xml_child(x = obj, search = omObservedPropertyName, ns = SosAllNamespaces()),
                                               verbose = verbose)

  timeObjectMap <- list()

  if(!is.na(xml2::xml_child(x = obj, search = om20PhenomenonTimeName, ns = SosAllNamespaces()))) {
    .pt <- parseTimeObject(obj = xml2::xml_child(x = obj, search = om20PhenomenonTimeName, ns = SosAllNamespaces()),
                 format = sosTimeFormat(sos = sos),
                 timeObjectMap = timeObjectMap,
                 verbose = verbose)
    .phenomenonTime <- .pt[[1]]
    timeObjectMap <- .pt[[2]]
  } else {
    warning("om:phenomenonTime is mandatory in om:Observation, but is missing!")
    .phenomenonTime <- NULL
  }

  if(!is.na(xml2::xml_child(x = obj, search = omFeatureOfInterestName, ns = SosAllNamespaces()))) {
    .featureOfInterest <- parseFOI(xml2::xml_child(x = obj, search = omFeatureOfInterestName, ns = SosAllNamespaces()),
                                   sos = sos, verbose = verbose)
    if(!is.null(.featureOfInterest@href)){
      if(verbose) cat("[trying to get referenced featureOfInterest]\n")

      .featureOfInterestIdentifier <- .featureOfInterest@href
      # TODO what about in-document references
      .featureOfInterest <- featureCache[[.featureOfInterestIdentifier]]
      if (is.null(.featureOfInterest)) {
        foiList <- getFeatureOfInterest(sos = sos, featureOfInterest = .featureOfInterestIdentifier)
        # getFeatureOfInterest returns a list, but a request including a featureOfInterest id
        #  should return only one featureOfInterest
        if(length(foiList) > 0){
          .featureOfInterest <- foiList[[1]]
          featureCache[[.featureOfInterestIdentifier]] <- .featureOfInterest
        }
      }
    }

  } else {
    warning("om:featureOfInterest is mandatory in om:Observation, but is missing!")
    .featureOfInterest <- NULL
  }

  # result parser is exchangeable
  .resultParsingFunction <- sosParsers(sos)[[omResultName]]
  .result <- .resultParsingFunction(xml2::xml_child(x = obj, search = omResultName, ns = SosAllNamespaces()), sos, verbose)

  # optional elements
  if(!is.na(xml2::xml_child(x = obj, search = omResultTimeName, ns = SosAllNamespaces()))) {
    .pt <- parseTimeObject(obj = xml2::xml_child(x = obj, search = omResultTimeName, ns = SosAllNamespaces()),
                           format = sosTimeFormat(sos = sos),
                           timeObjectMap = timeObjectMap,
                           verbose = verbose)
    .resultTime <- .pt[[1]]
    timeObjectMap <- .pt[[2]]
  }
  else {
    .resultTime <- NULL
  }

  # TODO optionals elements for OmObservation
  #.metadata
  #.resultQuality
  #.parameter
  #.metadata

  .obs <- OmOM_Observation(phenomenonTime = .phenomenonTime,
                           resultTime = .resultTime,
                           procedure = .procedure,
                           observedProperty = .observedProperty,
                           featureOfInterest = .featureOfInterest,
                           result = .result)

  return(.obs)
}

#
# create according GmlTimeObject from om:samplingTime/om:resultTime/om:phenomenonTime
#
parseTimeObject <- function(obj, format, timeObjectMap = list(), verbose = FALSE) {
  if (verbose) cat("[parseTimeObject]\n")

  .tiXML <- xml2::xml_find_first(x = obj, xpath = gmlTimeInstantName)
  .tpXML <- xml2::xml_find_first(x = obj, xpath = gmlTimePeriodName)
  .timeReference <- xml2::xml_attr(x = obj, attr = "href")
  .timeObject <- NULL

  if (!is.na(.tiXML)) {
    if (verbose) cat("[parseTimeObject] time instant.\n")
    .timeObject <- parseTimeInstant(obj = .tiXML, format = format)
    timeObjectMap[[.timeObject@id]] <- .timeObject
  }
  else if (!is.na(.tpXML)) {
    if (verbose) cat("[parseTimeObject] time period.\n")
    .timeObject <- parseTimePeriod(obj = .tpXML, format = format)
    timeObjectMap[[.timeObject@id]] <- .timeObject
  }
  else if (!is.na(.timeReference)) {
    if (verbose) cat("[parseTimeObject] referenced time.\n")
    .timeObject <- timeObjectMap[[substring(.timeReference, 2)]]
    if (is.null(.timeObject)) {
      stop(paste0("XML document invalid. Time reference '", .timeReference ,"' not in document."))
    }
  }
  else {
    warning("Could not create GmlTimeObject from given O&M time object.
            Require gml:TimeInstant or gml:TimePeriod as children.")
    .timeObject <- GmlTimeInstant(timePosition = GmlTimePosition(
      time = as.POSIXct(x = NA)))
  }

  return(list(.timeObject, timeObjectMap))
}
