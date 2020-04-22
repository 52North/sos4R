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
# Created: 2013-03-06                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

parseSosCapabilities200 <- function(obj, sos) {
  if (sos@verboseOutput) cat("[parseSosCapabilities200] entered... \n")

  caps.root <- xml2::xml_root(x = obj)

  # attributes:
  caps.version <- xml2::xml_attr(x = caps.root, attr = "version", default = NA_character_, ns = sos@namespaces)
  caps.updateSequence <- xml2::xml_attr(x = caps.root, attr = "updateSequence", default = NA_character_, ns = sos@namespaces)
  if (sos@verboseOutput) cat("[parseSosCapabilities200] version, update sequence:", caps.version, caps.updateSequence, "\n")

  owsSI <- xml2::xml_child(x = caps.root, search = owsServiceIdentificationName, ns = sos@namespaces)
  if (!is.na(owsSI)) caps.si <- parseOwsServiceIdentification(owsSI, sos = sos)
  else caps.si <- NULL

  owsSP <- xml2::xml_child(x = caps.root, search = owsServiceProviderName, ns = sos@namespaces)
  if (!is.na(owsSP)) caps.sp <- parseOwsServiceProvider(owsSP, sos = sos)
  else caps.sp <- NULL

  owsOM <- xml2::xml_child(x = caps.root, search = owsOperationsMetadataName, ns = sos@namespaces)
  if (!is.na(owsOM)) {
    if (sos@verboseOutput) cat("[parseSosCapabilities200] entering", owsOperationsMetadataName, "... \n")

    operationsXML <- xml2::xml_find_all(x = caps.root,
                                        xpath = paste0(owsOperationsMetadataName, "/", owsOperationName),
                                        ns = sos@namespaces)

    operations <- lapply(operationsXML, parseOwsOperation, sos = sos)
    # add names for indexing of list
    names(operations) <- lapply(operations,
                                function(obj) {
                                  return(obj@name)
                                })
    caps.om <- OwsOperationsMetadata(operations = operations)
  }
  else caps.om <- NULL

  sosContents <- xml2::xml_child(x = caps.root,
                                 search = paste0(sos200contentsName,
                                                 "/", sos200ContentsName),
                                 ns = sos@namespaces)
  if (!is.na(sosContents)) {
    if (sos@verboseOutput)
      cat("[parseSosCapabilities200] entering", sos200ContentsName, "... \n")

    observations = lapply(X = xml2::xml_children(sosContents),
                          FUN = parseSosObservationOffering_200,
                          sos = sos)
    # add names to list
    names(observations) <- lapply(observations, function(obj) {
      return(obj@id)
      })

    caps.contents <- SosContents(observationOfferings = observations)
  }
  else caps.contents <- NULL

  # TODO implement if needed, see parseSosCapabilities100(..)
  caps.fc <- NULL

  capabilities <- SosCapabilities(version = caps.version,
                                   updateSequence = caps.updateSequence,
                                   identification = caps.si,
                                   provider = caps.sp,
                                   operations = caps.om,
                                   filterCapabilities = caps.fc,
                                   contents = caps.contents)

  return(capabilities)
}

parseSosObservationOffering_200 <- function(obj, sos) {
  if (sos@verboseOutput) {
    cat("[parseSosObservationOffering_200] entering... \n")
    print(obj)
  }

  namespaces <- SosAllNamespaces(version = sos200_version)

  sosOffering <- xml2::xml_child(x = obj, search = sos200ObservationOfferingName, ns = sos@namespaces)
  if (is.na(sosOffering)) {
    warning("No offering found with name ", sos200ObservationOfferingName)
    return(NULL)
  }

  # not optional, but have a default just in case...
  id <- xml2::xml_text(x = xml2::xml_child(x = sosOffering,
                                           search = swesIdentifierName,
                                           ns = sos@namespaces))
  name <- xml2::xml_text(x = xml2::xml_child(x = sosOffering,
                                             search = swesNameName,
                                             ns = sos@namespaces))
  if (sos@verboseOutput) cat("[parseSosObservationOffering_200] id:", id, "name:", name, "\n")

  # can be references or contained inline, so use lists
  observableProperty <- parseSwesObservableProperty(xml2::xml_find_all(x = sosOffering,
                                                                       xpath = swesObservablePropertyName,
                                                                       ns = sos@namespaces),
                                                    verbose = sos@verboseOutput)
  observableProperty <- as.list(observableProperty)
  if (sos@verboseOutput) cat("[parseSosObservationOffering_200] observableProperty:", toString(observableProperty), "\n")

  featureOfInterestType <- xml2::xml_text(xml2::xml_find_all(x = sosOffering,
                                                             xpath = sos200FeatureOfInterestTypeName,
                                                             ns = sos@namespaces))
  featureOfInterestType <- as.list(featureOfInterestType)
  if (sos@verboseOutput) cat("[parseSosObservationOffering_200] featureOfInterestType:", toString(featureOfInterestType), "\n")

  observationType <- xml2::xml_text(xml2::xml_find_all(x = sosOffering,
                                                       xpath = sos200ObservationTypeName,
                                                       ns = sos@namespaces))
  observationType <- as.list(observationType)
  if (sos@verboseOutput) cat("[parseSosObservationOffering_200] observationType:", toString(observationType), "\n")

  procedure <- xml2::xml_text(xml2::xml_find_all(x = sosOffering,
                                                 xpath = swesProcedureName,
                                                 ns = sos@namespaces))
  if (sos@verboseOutput) cat("[parseSosObservationOffering_200] procedure:", toString(observationType), "\n")

  procedureDescriptionFormat <- xml2::xml_text(xml2::xml_find_all(x = sosOffering,
                                                                  xpath = swesProcedureDescriptionFormatName,
                                                                  ns = sos@namespaces))
  procedureDescriptionFormat <- as.list(procedureDescriptionFormat)
  if (sos@verboseOutput) cat("[parseSosObservationOffering_200] procedure description format:", toString(observationType), "\n")

  responseFormat <- xml2::xml_text(xml2::xml_find_all(x = sosOffering,
                                                      xpath = sos200ResponseFormatName,
                                                      ns = sos@namespaces))
  responseFormat <- as.list(responseFormat)
  if (sos@verboseOutput) cat("[parseSosObservationOffering_200] responseFormat:", toString(responseFormat), "\n")

  resultTimeXml <- xml2::xml_child(x = sosOffering, search = sos200ResultTimeName, ns = sos@namespaces)
  if (!is.na(resultTimeXml)) {
    resultTime <- parseTimeGeometricPrimitiveFromParent(obj = resultTimeXml, sos = sos)
    if (sos@verboseOutput) cat("[parseSosObservationOffering_200] resultTime: ", toString(resultTime), "\n")
  }
  else {
    resultTime <- GmlTimeInstant(timePosition = GmlTimePosition(time = parsedate::parse_iso_8601(NA)))
  }

  phenomenonTimeXml <- xml2::xml_child(x = sosOffering, search = sos200PhenomenonTimeName, ns = sos@namespaces)
  if (!is.na(resultTimeXml)) {
    phenomenonTime <- parseTimeGeometricPrimitiveFromParent(obj = phenomenonTimeXml, sos = sos)
    if (sos@verboseOutput) cat("[parseSosObservationOffering_200] resultTime: ", toString(resultTime), "\n")
  }
  else {
    phenomenonTime <- GmlTimeInstant(timePosition = GmlTimePosition(time = parsedate::parse_iso_8601(NA)))
  }

  env <- xml2::xml_find_first(x = sosOffering,
                              xpath = paste0(sos200ObservedAreaName, "/", gmlEnvelopeName),
                              ns = sos@namespaces)
  if (!is.na(env)) {
    observedArea <- parseEnvelope(obj = env, sos = sos, verbose = sos@verboseOutput, namespaces = namespaces)

    if (sos@verboseOutput) cat("[parseSosObservationOffering_200] boundedBy:", toString(observedArea), "\n")
  }
  else {
    observedArea <- list()
  }

  ob <- new("SosObservationOffering_2.0.0",
            id = id,
            name = name,
            resultTime = resultTime,
            phenomenonTime = phenomenonTime,
            procedure = procedure,
            observableProperty = observableProperty,
            featureOfInterestType = featureOfInterestType,
            observationType = observationType,
            responseFormat = responseFormat,
            procedureDescriptionFormat = procedureDescriptionFormat,
            observedArea = observedArea)

  if (sos@verboseOutput) cat("[parseSosObservationOffering] done: ", toString(ob), "\n")

  return(ob)
}

parseGetObservationResponse <- function(obj, sos, verbose = FALSE, retrieveFOI = TRUE) {
  if (verbose) {
    cat("[parseGetObservationResponse] entering... \n")
    print(obj)
  }

  observationsXML <- xml2::xml_find_all(x = obj,
                                       xpath = sos200ObservationDataName,
                                       ns = sos@namespaces)
  if (!length(observationsXML)) {
    if (verbose) cat("[parseGetObservationResponse] No content in response!\n")
    return(list())
  }

  observations <- sapply(observationsXML,
                         parseObservation_2.0,
                         sos = sos,
                         verbose = verbose,
                         retrieveFOI = retrieveFOI)

  return(observations)
}

parseGetObservationByIdResponse <- function(obj, sos, verbose = FALSE) {
  if (verbose) {
    cat("[parseGetObservationByIdResponse] entering... \n")
    print(obj)
  }

  observationsXML <- xml2::xml_find_all(x = obj,
                                        xpath = sos200ObservationName,
                                        ns = sos@namespaces)
  if (verbose) cat("[parseGetObservationByIdResponse] Parsing ", length(observationsXML), " observations\n")

  observations <- sapply(X = observationsXML,
                         parseObservation_2.0,
                         sos = sos,
                         verbose = verbose)

  return(observations)
}

parseGetFeatureOfInterestResponse <- function(obj, sos, verbose = FALSE) {
  if (verbose) {
    cat("[parseGetFeatureOfInterestResponse] entering... \n")
    print(obj)
  }

  featureXML <- xml2::xml_find_all(x = obj,
                                   xpath = "sos20:featureMember",
                                   ns = sos@namespaces)
  foi = sapply(X = featureXML,
               FUN = .parseFeatureMember,
               sos = sos,
               verbose = verbose)
  return(foi)
}

.parseFeatureMember <- function(obj, sos, verbose = FALSE) {
  href <- xml2::xml_attr(x = obj, attr = "xlink:href", ns = sos@namespaces)
  if (!is.na(href)) {
    if (verbose) cat("[.parseFeatureMember] Got references feature: ", href, "\n")

        parsed <- GmlFeatureProperty(href = href)
  }
  else {
    member <- xml2::xml_child(x = obj)
    name <- xml2::xml_name(x = member, ns = sos@namespaces)

    if (verbose) cat("[.parseFeatureMember] Parsing ", name, "\n")

    if (name == wmlMonitoringPointName) {
      sp <- parseWmlMonitoringPoint(member, sos = sos)
      parsed <- GmlFeatureProperty(feature = sp)
    }
    else if (name == samsSamplingFeatureName) {
      sf <- parseSams200SamplingFeature(obj = member, sos = sos)
      parsed <- GmlFeatureProperty(feature = sf)
    }
    else {
      warning(paste("No handling for given sos:featureMember available: ", name))
    }
  }

  return(parsed)
}

parseDescribeSensorResponse <- function(obj, sos, verbose = FALSE) {
  if (verbose) {
    cat("[parseDescribeSensorResponse] entering... \n")
    print(obj)
  }

  smlXml <- xml2::xml_find_first(x = obj,
                                 xpath = paste0("//", smlSensorMLName),
                                 ns = sos@namespaces)
  sml <- parseSensorML(obj = smlXml, sos = sos, verbose = verbose)

  timePeriodXml <- xml2::xml_find_first(x = obj,
                                        xpath = paste0(swesDescriptionName,
                                                       "/", swesSensorDescriptionName,
                                                       "/", swesValidTimeName,
                                                       "/", gmlTimePeriodName),
                                        ns = sos@namespaces)
  sml@validTime <- parseTimePeriod(obj = timePeriodXml, sos = sos)
  if (verbose) cat("[parseDescribeSensorResponse] Parsed validTime: ", toString(sml@validTime), "\n")
  return(sml)
}
