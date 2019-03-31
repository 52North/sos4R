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
# Dispatch function for all exchangeable parsers for OM elements.
#
parseOM <- function(obj, sos, verbose = FALSE) {
  .om <- NULL

  # switch submethods based on name (needs prefix)
  .name <- xml2::xml_name(x = obj, ns = SosAllNamespaces())

  .parsingFunction <- sosParsers(sos)[[.name]]
  if (!is.null(.parsingFunction)) {
    if (verbose) cat("[parseOM] Matched name for parser is", .name, "\n")
    .om <- .parsingFunction(obj = obj, sos = sos, verbose = verbose)
    if (verbose) cat("[parseOM] Done parsing\n")
  }
  else {
    warning(paste("[parseOM] No parsing function for given element", .name))
  }

  return(.om)
}

#
# Function extracts om:Obervation or om:Measurement from om:member.
#
parseObservationProperty <- function(obj, sos, verbose = FALSE) {
  # a member can only have one child element
  if (xml2::xml_length(x = obj) >= 1) {
    .child <- xml2::xml_child(obj)
    if (verbose) cat("[parseObservationProperty] Parsing child of member:",
                     xml2::xml_name(x = .child, ns = SosAllNamespaces()), "\n")
    .mResult <- parseOM(.child, sos, verbose)
  }
  else {
    # no child, try href attribute
    if (verbose) cat("[parseObservationProperty] Member has no direct child!\n")

    .href <- xml2::xml_attr(x = obj, attr = "href", default = NA_character_)
    if (!is.na(.href)) {
      warning("Only reference was returned:", .href)
      .mResult <- OmObservationProperty(href = .href)
    }
    else {
      warning("No observation found!")
      .mResult <- OmObservationProperty()
    }
  }

  return(.mResult)
}

#
# om:Measurement
#
parseMeasurement <- function(obj, sos, verbose = FALSE) {
  if (verbose) cat("[parseMeasurement]\n")

  .samplingTimeXml <- xml2::xml_child(x = obj,
                                      search = omSamplingTimeName,
                                      ns = SosAllNamespaces())
  .samplingTime <- parseTime(obj = .samplingTimeXml,
                             format = sosTimeFormat(sos),
                             verbose = verbose)

  # 52N SOS only returns om:Measurements (!) with procedure ids and observed
  # properties in xlink:href
  .procedure <- xml2::xml_attr(x = xml2::xml_child(x = obj, search = omProcedureName, ns = SosAllNamespaces()), attr = "href")
  .observedProperty <- SwePhenomenonProperty(
    href = xml2::xml_attr(x = xml2::xml_child(x = obj, search = omObservedPropertyName, ns = SosAllNamespaces()), attr = "href"))

  .featureOfInterest <- parseFOI(xml2::xml_child(x = obj, search = omFeatureOfInterestName, ns = SosAllNamespaces()), sos = sos,
                                 verbose = verbose)

  # must be GmlMeasure
  .result <- parseMeasure(xml2::xml_child(x = obj, search = omResultName, ns = SosAllNamespaces()))

  # TODO optionals elements for OmMeasurement
  #.metadata
  #.resultTime
  #.resultQuality
  #.parameter

  .measurement <- OmMeasurement(samplingTime = .samplingTime,
                                procedure = .procedure, observedProperty = .observedProperty,
                                featureOfInterest = .featureOfInterest, result = .result)

  return(.measurement)
}

#
# om:Observation
#
parseObservation <- function(obj, sos, verbose = FALSE) {
  .id <- xml2::xml_attr(x = obj, attr = "id", default = NA_character_)
  if (verbose) cat("[parseObservation]", .id, "\n")

  # 52N SOS only returns om:Observation with procedure ids xlink:href
  .procedure <- xml2::xml_attr(x = xml2::xml_child(x = obj,
                                                   search = omProcedureName,
                                                   ns = SosAllNamespaces()),
                               attr = "href",
                               default = NA_character_)

  .observedProperty <- parsePhenomenonProperty(xml2::xml_child(x = obj, search = omObservedPropertyName, ns = SosAllNamespaces()),
                                               verbose = verbose)

  if (!is.na(xml2::xml_child(x = obj, search = omSamplingTimeName, ns = SosAllNamespaces()))) {
    .samplingTime <- parseTime(obj = xml2::xml_child(x = obj,
                                                     search = omSamplingTimeName,
                                                     ns = SosAllNamespaces()),
                               format = sosTimeFormat(sos = sos), verbose = verbose)
  } else {
    warning("om:samplingTime is mandatory in om:Observation, but is missing!")
    .samplingTime <- NULL
  }

  if (!is.na(xml2::xml_child(x = obj, search = omFeatureOfInterestName, ns = SosAllNamespaces()))) {
    .featureOfInterest <- parseFOI(xml2::xml_child(x = obj, search = omFeatureOfInterestName, ns = SosAllNamespaces()),
                                   sos = sos, verbose = verbose)
  } else {
    warning("om:featureOfInterest is mandatory in om:Observation, but is missing!")
    .featureOfInterest <- NULL
  }

  # result parser is exchangeable
  .resultParsingFunction <- sosParsers(sos)[[omResultName]]
  .result <- .resultParsingFunction(xml2::xml_child(x = obj, search = omResultName, ns = SosAllNamespaces()), sos, verbose)

  # optional elements
  if (!is.na(xml2::xml_child(x = obj, search = omResultTimeName, ns = SosAllNamespaces()))) {
    .resultTime <- parseTime(obj = xml2::xml_child(x = obj, search = omResultTimeName, ns = SosAllNamespaces()),
                             format = sosTimeFormat(sos = sos), verbose = verbose)
  }
  else {
    .resultTime <- NULL
  }

  # TODO optionals elements for OmObservation
  #.metadata
  #.resultQuality
  #.parameter
  #.metadata

  .obs <- OmObservation(samplingTime = .samplingTime,
                        procedure = .procedure,
                        observedProperty = .observedProperty,
                        featureOfInterest = .featureOfInterest,
                        result = .result)

  return(.obs)
}

#
#
#
parseObservationCollection <- function(obj, sos, verbose = FALSE) {
  .members <- xml2::xml_find_all(x = obj, xpath = omMemberName, ns = SosAllNamespaces())

  if (verbose) cat("[parseObservationCollection] with ", length(.members), "element(s).\n")

  .env <- xml2::xml_child(x = obj, search = paste0(gmlBoundedByName, "/", gmlEnvelopeName))
  if (!is.na(.env)) {
    .boundedBy <- list(
      srsName = xml2::xml_attr(x = .env, attr = "srsName", ns = SosAllNamespaces()),
      lowerCorner = xml2::xml_text(x = xml2::xml_child(x = .env, search = gmlLowerCornerName, ns = SosAllNamespaces())),
      upperCorner = xml2::xml_text(x = xml2::xml_child(x = .env, search = gmlUpperCornerName, ns = SosAllNamespaces())))

    if (verbose) cat("[parseObservationCollection] Parsed envelope:", toString(.boundedBy), "\n")

    if (sosSwitchCoordinates(sos)) {
      warning("Switching coordinates in envelope of ObservationCollection!")
      .origLC <- strsplit(x = .boundedBy[["lowerCorner"]], split = " ")
      .lC <- paste(.origLC[[1]][[2]], .origLC[[1]][[1]])
      .origUC <- strsplit(x = .boundedBy[["upperCorner"]], split = " ")
      .uC <- paste(.origUC[[1]][[2]], .origUC[[1]][[1]])
      .boundedBy <- list(srsName = xml2::xml_attr(x = .env, attr = "srsName"),
                         lowerCorner = .lC, upperCorner = .uC)
    }
  }
  else {
    if (verbose) cat("[parseObservationCollection] Empty envelope!\n")
    .boundedBy <- list()
  }

  .resultList <- lapply(X = .members, FUN = parseOM, sos = sos, verbose = verbose)
  names(.resultList) <- lapply(X = .members, FUN = function(member) {
    children <- xml2::xml_children(member)
    idOrName <- xml2::xml_attr(children, attr = "id", default = xml2::xml_name(children))
    if (length(idOrName) < 1) {
      xml2::xml_name(member)
    } else {
      idOrName
    }
  })

  if (is.list(.resultList)) {
    .obsColl <- OmObservationCollection(members = .resultList,
                                        boundedBy = .boundedBy)
  }
  else {
    .obsColl <- OmObservationCollection(members = list(.resultList),
                                        boundedBy = .boundedBy)
  }

  if (verbose)
    cat("[parseObservationCollection] Done. Processed", length(.obsColl),
        "elements:", names(.obsColl), "\n")

  return(.obsColl)
}

#
# om:result
#
parseResult <- function(obj, sos, verbose = FALSE) {
  if (verbose) cat("[parseResult] Starting ...\n")
  .result <- NULL

  .children <- xml2::xml_children(x = obj)
  if (verbose) cat("[parseResult]", length(.children), " non-text nodes, names:", xml2::xml_name(.children), "\n")

  # Check if remaining element is there
  if (length(.children) == 0) {
    .children <- xml2::xml_children(x = obj)
    stop("Continue implementation here: OM-methods-parsing.R")
    cat("[parseResult] No non-text nodes in result, returning NULL.\n")

    #in O&M 2.0 there can be (literal) results of type MeasurementType
    .typeAttributValue <- xml2::xml_attr(x = obj, attr = om20ResultTypeAttributeName, default = NA_character_)

    .typeWithQualifiedname <- strsplit(.typeAttributValue, ":")

    .type <- NA_character_

    if (length(.typeWithQualifiedname) > 0) {

      if (length(.typeWithQualifiedname[[1]]) > 1) {
        .type <- .typeWithQualifiedname[[1]][2]
      }
    }

    if (!is.na(.type)) {
      if (.type == om20ResultMeasureTypeName) {
        return(xml2::xml_text(x = obj))
      }
    }

    return(NULL)
  }

  .name <- xml2::xml_name(x = .children[[1]], ns = SosAllNamespaces())
  if (.name == sweDataArrayName) {
    if (verbose) cat("[parseResult] Parsing result with swe:DataArray.\n")

    # data array parser is exchangeable
    .dataArrayParsingFunction <- sosParsers(sos)[[sweDataArrayName]]
    .result <- .dataArrayParsingFunction(.children[[1]], sos, verbose)
  }
  else if (.name == xmlTextNodeName) {
    .result <- as.numeric(xml2::xml_text(x = .children))
    if (is.na(.result)) {
      .result <- xml2::xml_text(x = .children, trim = TRUE)
    }
  }
  else {
    warning(paste("[parseResult] Parsing of given result is NOT supported:",
                  xml2::xml_name(x = .children[[1]], ns = SosAllNamespaces()),
                  "-- only", sweDataArrayName,
                  " or text nodes containing strings or numbers can be parsed."))
  }

  if (is.null(.result)) {
    stop("[parseResult] result is null! Given result:\n")
    print(obj)
  }

  if (verbose) cat("[parseResult] Done\n")

  return(.result)
}

parseGeometryObservation <- function(obj, sos, verbose = FALSE) {
  warning("Parsing of om:GeometryObservation is not implemented!")
  return(NA)
}

parseCategoryObservation <- function(obj, sos, verbose = FALSE) {
  warning("Parsing of om:CategoryObservation is not implemented!")
  return(NA)
}

parseCountObservation <- function(obj, sos, verbose = FALSE) {
  warning("Parsing of om:CountObservation is not implemented!")
  return(NA)
}

parseTruthObservation <- function(obj, sos, verbose = FALSE) {
  warning("Parsing of om:TruthObservation is not implemented!")
  return(NA)
}

parseTemporalObservation <- function(obj, sos, verbose = FALSE) {
  warning("Parsing of om:TemporalObservatio is not implemented!")
  return(NA)
}

parseComplexObservation <- function(obj, sos, verbose = FALSE) {
  warning("Parsing of om:ComplexObservation is not implemented!")
  return(NA)
}

#
# parseFOI ----
# (not exchangeable)
# parse sos:featureOfInterest to according Element of GML or SA
#
parseFOI <- function(obj, sos, verbose = FALSE) {
  if (verbose) cat("[parseFOI] starting...\n")
  .foi <- NULL

  # has href attribute? if yes, use it!
  .href <- xml2::xml_attr(x = obj, attr = "href")
  if (!is.na(.href)) {
    if (verbose) cat("[parseFOI] referenced FOI:", .href, "\n")
    # feature is referenced
    .foi <- GmlFeatureProperty(href = .href)
  }
  else {
    # feature is available in the element
    .feature <- xml2::xml_child(x = obj)
    .name <- xml2::xml_name(x = .feature, ns = SosAllNamespaces())

    if (verbose) cat("[parseFOI] inline FOI:", .name, "\n")

    # cannot use switch here, because it does not work with a ':' in the expresssion
    if (.name == saSamplingPointName) {
      .sp <- parseSamplingPoint(.feature, sos = sos)
      .foi <- GmlFeatureProperty(feature = .sp)
    }
    else if (.name == gmlFeatureCollectionName) {
      .foi <- parseFeatureCollection(.feature, sos = sos)
    }
    else if (.name == wmlMonitoringPointName) {
      .foi <- parseMonitoringPoint(.feature, sos = sos)
    }
    else if (.name == samsSamplingFeatureName) {
      .foi <- parseSams200SamplingFeature(.feature, sos = sos)
    }
    else if (.name == saSamplingSurface) {
      warning("[parseFOI] No parsing for sa:SamplingSurface implemented!")
      GmlFeatureProperty(href = .name)
    }
    else {
      warning("[parseFOI] No parsing for given feature implemented!")
      GmlFeatureProperty(href = .name)
    }
  }

  return(.foi)
}


#
# parseTime ----
# (not exchangeable)
# handles time instant, time period, and time reference
#
parseTime <- function(obj, format, verbose = FALSE) {
  if (verbose) cat("[parseTime] Entering\n")

  .tiXML <- xml2::xml_find_first(x = obj, xpath = gmlTimeInstantName, ns = SosAllNamespaces())
  .tpXML <- xml2::xml_find_first(x = obj, xpath = gmlTimePeriodName, ns = SosAllNamespaces())
  .timeReference <- xml2::xml_attr(x = obj, attr = "xlink:href", ns = SosAllNamespaces())
  .timeObject <- NULL

  if (!is.na(.tiXML)) {
    if (verbose) cat("[parseTime] Found time instant\n")
    .timeObject <- parseTimeInstant(obj = .tiXML, format = format)
  }
  else if (!is.na(.tpXML)) {
    if (verbose) cat("[parseTime] Found time period\n")
    .timeObject <- parseTimePeriod(obj = .tpXML, format = format)
  }
  else if (!is.na(.timeReference)) {
    if (verbose) cat("[parseTime] Found referenced time\n")
    .timeObject <- GmlTimeInstantProperty(href = .timeReference)
    #if (is.null(.timeObject)) {
    #  stop(paste0("XML document invalid. Time reference '", .timeReference ,"' not in document."))
    #}
  }
  else {
    warning("Could not create GmlTimeObject from given O&M time object.
            Require gml:TimeInstant or gml:TimePeriod as children.")
    .timeObject <- GmlTimeInstant(timePosition = GmlTimePosition(
      time = as.POSIXct(x = NA)))
  }

  if (verbose) cat("[parseTime] Done:", toString(.timeObject), "\n")
  return(.timeObject)
}
