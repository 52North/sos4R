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
# Dispatch function for all exchangeable parsers for OM elements.
#
parseOM <- function(obj, sos, verbose = FALSE) {
  om <- NULL

  # switch submethods based on name (needs prefix)
  name <- xml2::xml_name(x = obj, ns = sos@namespaces)

  parsingFunction <- sos4R::sosParsers(sos)[[name]]
  if (!is.null(parsingFunction)) {
    if (verbose) cat("[parseOM] Matched name for parser is", name, "\n")
    om <- parsingFunction(obj = obj, sos = sos, verbose = verbose)
    if (verbose) cat("[parseOM] Done parsing\n")
  }
  else {
    warning(paste("[parseOM] No parsing function for given element", name))
  }

  return(om)
}

#
# Function extracts om:Obervation or om:Measurement from om:member.
#
parseObservationProperty <- function(obj, sos, verbose = FALSE) {
  # a member can only have one child element
  if (xml2::xml_length(x = obj) >= 1) {
    .child <- xml2::xml_child(obj)
    if (verbose) cat("[parseObservationProperty] Parsing child of member:",
                     xml2::xml_name(x = .child, ns = sos@namespaces), "\n")
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
                                      ns = sos@namespaces)
  .samplingTime <- parseTime(obj = .samplingTimeXml,
                             sos = sos,
                             verbose = verbose)

  # 52N SOS only returns om:Measurements (!) with procedure ids and observed
  # properties in xlink:href
  .procedure <- xml2::xml_attr(x = xml2::xml_child(x = obj, search = omProcedureName, ns = sos@namespaces), attr = "href")
  .observedProperty <- SwePhenomenonProperty(
    href = xml2::xml_attr(x = xml2::xml_child(x = obj, search = omObservedPropertyName, ns = sos@namespaces), attr = "href"))

  .featureOfInterest <- parseFOI(xml2::xml_child(x = obj, search = omFeatureOfInterestName, ns = sos@namespaces), sos = sos,
                                 verbose = verbose)

  # must be GmlMeasure
  .result <- parseMeasure(xml2::xml_child(x = obj, search = omResultName, ns = sos@namespaces))

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
  procedure <- xml2::xml_attr(x = xml2::xml_child(x = obj,
                                                   search = omProcedureName,
                                                   ns = sos@namespaces),
                               attr = "href",
                               default = NA_character_)

  observedProperty <- parsePhenomenonProperty(xml2::xml_child(x = obj, search = omObservedPropertyName, ns = sos@namespaces),
                                               verbose = verbose)

  if (!is.na(xml2::xml_child(x = obj, search = omSamplingTimeName, ns = sos@namespaces))) {
    samplingTime <- parseTime(obj = xml2::xml_child(x = obj,
                                                    search = omSamplingTimeName,
                                                    ns = sos@namespaces),
                              sos = sos,
                              verbose = verbose)
  } else {
    warning("om:samplingTime is mandatory in om:Observation, but is missing!")
    samplingTime <- NULL
  }

  if (!is.na(xml2::xml_child(x = obj, search = omFeatureOfInterestName, ns = sos@namespaces))) {
    featureOfInterest <- parseFOI(xml2::xml_child(x = obj, search = omFeatureOfInterestName, ns = sos@namespaces),
                                   sos = sos, verbose = verbose)
  } else {
    warning("om:featureOfInterest is mandatory in om:Observation, but is missing!")
    featureOfInterest <- NULL
  }

  # result parser is exchangeable
  resultParsingFunction <- sosParsers(sos)[[omResultName]]
  result <- resultParsingFunction(xml2::xml_child(x = obj, search = omResultName, ns = sos@namespaces), sos, verbose)

  # optional elements
  if (!is.na(xml2::xml_child(x = obj, search = omResultTimeName, ns = sos@namespaces))) {
    resultTime <- parseTime(obj = xml2::xml_child(x = obj, search = omResultTimeName, ns = sos@namespaces),
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

  obs <- OmObservation(samplingTime = samplingTime,
                        procedure = procedure,
                        observedProperty = observedProperty,
                        featureOfInterest = featureOfInterest,
                        result = result)

  return(obs)
}

#
#
#
parseObservationCollection <- function(obj, sos, verbose = FALSE) {
  members <- xml2::xml_find_all(x = obj, xpath = omMemberName, ns = sos@namespaces)

  if (verbose) cat("[parseObservationCollection] with ", length(members), "element(s).\n")

  env <- xml2::xml_child(x = obj, search = paste0(gmlBoundedByName, "/", gmlEnvelopeName))
  if (!is.na(env)) {
    boundedBy <- parseEnvelope(obj = env, sos = sos, verbose = verbose, namespaces = sos@namespaces)
  }
  else {
    if (verbose) cat("[parseObservationCollection] Empty envelope!\n")
    boundedBy <- list()
  }

  resultList <- lapply(X = members, FUN = parseOM, sos = sos, verbose = verbose)
  names(resultList) <- lapply(X = members, FUN = function(member) {
    children <- xml2::xml_children(member)
    idOrName <- xml2::xml_attr(children, attr = "id", default = xml2::xml_name(children))
    if (length(idOrName) < 1) {
      xml2::xml_name(member)
    } else {
      idOrName
    }
  })

  if (is.list(resultList)) {
    obsColl <- OmObservationCollection(members = resultList,
                                       boundedBy = boundedBy)
  }
  else {
    obsColl <- OmObservationCollection(members = list(resultList),
                                       boundedBy = boundedBy)
  }

  if (verbose) cat("[parseObservationCollection] Done. Processed", length(obsColl), "elements:", names(obsColl), "\n")

  return(obsColl)
}

#
# om:result
#
parseResult <- function(obj, sos, verbose = FALSE) {
  if (verbose) cat("[parseResult] Starting ...\n")
  result <- NA

  children <- xml2::xml_children(x = obj)
  if (verbose) cat("[parseResult]", length(children), " non-text nodes, names:", xml2::xml_name(children), "\n")

  # Check if remaining element is there
  if (length(children) == 0) {
    if (verbose) cat("[parseResult] No non-text nodes in result, returning content.\n")

    # in O&M 2.0 there can be (literal) results of type MeasurementType
    omTypeAttribute <- xml2::xml_attr(x = obj, attr = gmlResultTypeAttributeName, ns = sos@namespaces)
    xsiType <- xml2::xml_attr(x = obj, attr = xsiTypeName, ns = sos@namespaces)
    uom <- xml2::xml_attr(x = obj, attr = "uom")

    if (!is.na(xsiType)) {
      value <- xml2::xml_text(x = obj)

      if (grepl(pattern = "MeasureType", x = xsiType)) {
        value <- sosConvertDouble(value, sos = sos)
      }

      result <- data.frame(value)
      oldAttrs <- attributes(result[,1])
      attributes(result[,1]) <- c(as.list(oldAttrs), list("unit of measurement" = uom))
      names(result) <- uom
    } else if (!is.na(omTypeAttribute)) {
      typeWithQualifiedName <- strsplit(omTypeAttribute, ":")
      type <- NA_character_
      if (length(typeWithQualifiedName) > 0) {
        if (length(typeWithQualifiedName[[1]]) > 1) {
          type <- typeWithQualifiedName[[1]][2]
        }
      }

      if (!is.na(type)) {
        if (type == om20ResultMeasureTypeName) {
          result <- xml2::xml_text(x = obj)
        }
      }
    } else {
      warning("Could not extract value from result with text content.")
    }
  } else {
    name <- xml2::xml_name(x = children[[1]], ns = sos@namespaces)
    if (name %in% c(sweDataArrayName, swe20DataArrayName)) {
      if (verbose) cat("[parseResult] Parsing result with swe:DataArray.\n")

      # data array parser is exchangeable
      dataArrayParsingFunction <- sosParsers(sos)[[sweDataArrayName]]
      result <- dataArrayParsingFunction(children[[1]], sos, verbose)
    }
    else if (name == xmlTextNodeName) {
      result <- as.numeric(xml2::xml_text(x = children))
      if (is.na(result)) {
        result <- xml2::xml_text(x = children, trim = TRUE)
      }
    }
    else if (name == wmlMeasurementTimeseriesName) {
      parsingFunction <- sosParsers(sos)[[wmlMeasurementTimeseriesName]]
      result <- parsingFunction(children[[1]], sos, verbose)
    }
    else {
      warning(paste("[parseResult] Parsing of given result is NOT supported:",
                    xml2::xml_name(x = children[[1]], ns = sos@namespaces),
                    "-- only", sweDataArrayName,
                    " or text nodes containing strings or numbers can be parsed."))
    }
  }

  if (is.null(result)) {
    stop("[parseResult] Result is null! Given input:\n")
    print(obj)
  }

  if (verbose) cat("[parseResult] Done.\n")

  return(result)
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
# parseFOI
# (not exchangeable)
# parse sos:featureOfInterest to according Element of GML or SA
#
parseFOI <- function(obj, sos, verbose = FALSE) {
  if (verbose) cat("[parseFOI] starting...\n")
  foi <- NULL

  # has href attribute? if yes, use it!
  href <- xml2::xml_attr(x = obj, attr = "href")
  if (!is.na(href)) {
    if (verbose) cat("[parseFOI] referenced FOI:", href, "\n")
    # feature is referenced
    foi <- GmlFeatureProperty(href = href)
  }
  else {
    # feature is available in the element
    feature <- xml2::xml_child(x = obj)
    name <- xml2::xml_name(x = feature, ns = sos@namespaces)

    if (verbose) cat("[parseFOI] inline FOI:", name, "\n")

    # cannot use switch here, because it does not work with a ':' in the expresssion
    if (name == saSamplingPointName) {
      sp <- parseSamplingPoint(feature, sos = sos)
      foi <- GmlFeatureProperty(feature = sp)
    }
    else if (name == gmlFeatureCollectionName) {
      foi <- parseFeatureCollection(feature, sos = sos)
    }
    else if (name == wmlMonitoringPointName) {
      foi <- parseWmlMonitoringPoint(feature, sos = sos)
    }
    else if (name == samsSamplingFeatureName) {
      foi <- parseSams200SamplingFeature(feature, sos = sos)
    }
    else if (name == saSamplingSurface) {
      warning("[parseFOI] No parsing for sa:SamplingSurface implemented!")
      GmlFeatureProperty(href = name)
    }
    else {
      warning("[parseFOI] No parsing for given feature implemented!")
      GmlFeatureProperty(href = name)
    }
  }

  return(foi)
}


#
# parseTime
# (not exchangeable)
# handles time instant, time period, and time reference
#
parseTime <- function(obj, sos, verbose = FALSE) {
  if (verbose) cat("[parseTime] Entering\n")

  .tiXML <- xml2::xml_find_first(x = obj, xpath = gmlTimeInstantName, ns = sos@namespaces)
  .tpXML <- xml2::xml_find_first(x = obj, xpath = gmlTimePeriodName, ns = sos@namespaces)
  .timeReference <- xml2::xml_attr(x = obj, attr = "xlink:href", ns = sos@namespaces)
  .timeObject <- NULL

  if (!is.na(.tiXML)) {
    if (verbose) cat("[parseTime] Found time instant\n")
    .timeObject <- parseTimeInstant(obj = .tiXML, sos = sos)
  }
  else if (!is.na(.tpXML)) {
    if (verbose) cat("[parseTime] Found time period\n")
    .timeObject <- parseTimePeriod(obj = .tpXML, sos = sos)
  }
  else if (!is.na(.timeReference)) {
    if (verbose) cat("[parseTime] Found referenced time\n")
    .timeObject <- GmlTimeInstantProperty(href = .timeReference)
  }
  else {
    warning("Could not create GmlTimeObject from given O&M time object.
            Require gml:TimeInstant or gml:TimePeriod as children.")
    .timeObject <- GmlTimeInstant(timePosition = GmlTimePosition(
      time = parsedate::parse_iso_8601(NA)))
  }

  if (verbose) cat("[parseTime] Done:", toString(.timeObject), "\n")
  return(.timeObject)
}
