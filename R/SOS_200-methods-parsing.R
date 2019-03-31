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
# Author: Benjamin Pross (b.pross@52north.org)                                 #
# Created: 2013-03-06                                                          #
# Project: sos4R - visit the project web page,                                 #
#      http://52north.org/communities/sensorweb/clients/sos4R/                 #
################################################################################

#
# function for parsing Capabilities SOS v 2.0.0
#
parseSosCapabilities200 <- function(obj, sos) {
  if (sos@verboseOutput) cat("[parseSosCapabilities200] entered... \n")

  .caps.root <- xml2::xml_root(x = obj)
  namespaces <- SosAllNamespaces(version = sos200_version)

  # attributes:
  .caps.version <- xml2::xml_attr(x = .caps.root, attr = "version", default = NA_character_, ns = namespaces)
  .caps.updateSequence <- xml2::xml_attr(x = .caps.root, attr = "updateSequence", default = NA_character_, ns = namespaces)
  if (sos@verboseOutput) cat("[parseSosCapabilities200] version, update sequence:", .caps.version, .caps.updateSequence, "\n")

  .owsSI <- xml2::xml_child(x = .caps.root, search = owsServiceIdentificationName, ns = namespaces)
  if (!is.na(.owsSI))
    .caps.si <- parseOwsServiceIdentification(.owsSI)
  else .caps.si <- NULL

  .owsSP <- xml2::xml_child(x = .caps.root, search = owsServiceProviderName, ns = namespaces)
  if (!is.na(.owsSP))
    .caps.sp <- parseOwsServiceProvider(.owsSP)
  else .caps.sp <- NULL

  .owsOM <- xml2::xml_child(x = .caps.root, search = owsOperationsMetadataName, ns = namespaces)
  if (!is.na(.owsOM)) {
    if (sos@verboseOutput) cat("[parseSosCapabilities200] entering", owsOperationsMetadataName, "... \n")

    .operationsXML <- xml2::xml_find_all(x = .caps.root,
                                         xpath = paste0(owsOperationsMetadataName, "/", owsOperationName),
                                         ns = namespaces)

    .operations <- lapply(.operationsXML, parseOwsOperation)
    # add names for indexing of list
    names(.operations) <- lapply(.operations,
                                 function(obj) {
                                   return(obj@name)
                                 })
    .caps.om <- OwsOperationsMetadata(operations = .operations)
  }
  else .caps.om <- NULL

  .sosContents <- xml2::xml_child(x = .caps.root,
                                  search = paste0(sos200contentsName,
                                                  "/", sos200ContentsName),
                                  ns = namespaces)
  if (!is.na(.sosContents)) {
    if (sos@verboseOutput)
      cat("[parseSosCapabilities200] entering", sos200ContentsName, "... \n")

    .observations = lapply(X = xml2::xml_children(.sosContents),
                           FUN = parseSosObservationOffering_200,
                           sos = sos)
    # add names to list
    names(.observations) <- lapply(.observations,
                                   function(obj) {
                                     return(obj@id)
                                   })

    .caps.contents <- SosContents(observationOfferings = .observations)
  }
  else .caps.contents <- NULL

  # TODO implement if needed, see parseSosCapabilities100(..)
  .caps.fc <- NULL

  .capabilities <- SosCapabilities(version = .caps.version,
                                   updateSequence = .caps.updateSequence,
                                   identification = .caps.si,
                                   provider = .caps.sp,
                                   operations = .caps.om,
                                   filterCapabilities = .caps.fc,
                                   contents = .caps.contents)

  return(.capabilities)
}

parseSosObservationOffering_200 <- function(obj, sos) {
  if (sos@verboseOutput) {
    cat("[parseSosObservationOffering_200] entering... \n")
    print(obj)
  }

  namespaces <- SosAllNamespaces(version = sos200_version)

  .sosOffering <- xml2::xml_child(x = obj, search = sos200ObservationOfferingName, ns = namespaces)
  if (is.na(.sosOffering)) {
    warning("No offering found with name ", sos200ObservationOfferingName)
    return(NULL)
  }

  # not optional, but have a default just in case...
  .id <- xml2::xml_text(x = xml2::xml_child(x = .sosOffering,
                                            search = swesIdentifierName,
                                            ns = namespaces))
  .name <- xml2::xml_text(x = xml2::xml_child(x = .sosOffering,
                                              search = swesNameName,
                                              ns = namespaces))
  if (sos@verboseOutput) cat("[parseSosObservationOffering_200] id:", .id, "name:", .name, "\n")

  # can be references or contained inline, so use lists
  .observableProperty <- parseSwesObservableProperty(xml2::xml_find_all(x = .sosOffering,
                                                                        xpath = swesObservablePropertyName,
                                                                        ns = namespaces),
                                                     verbose = sos@verboseOutput)
  .observableProperty <- as.list(.observableProperty)
  if (sos@verboseOutput) cat("[parseSosObservationOffering_200] observableProperty:", toString(.observableProperty), "\n")

  .featureOfInterestType <- xml2::xml_text(xml2::xml_find_all(x = .sosOffering,
                                                              xpath = sos200FeatureOfInterestTypeName,
                                                              ns = namespaces))
  .featureOfInterestType <- as.list(.featureOfInterestType)
  if (sos@verboseOutput) cat("[parseSosObservationOffering_200] featureOfInterestType:", toString(.featureOfInterestType), "\n")

  .observationType <- xml2::xml_text(xml2::xml_find_all(x = .sosOffering,
                                                        xpath = sos200ObservationTypeName,
                                                        ns = namespaces))
  .observationType <- as.list(.observationType)
  if (sos@verboseOutput) cat("[parseSosObservationOffering_200] observationType:", toString(.observationType), "\n")

  .procedure <- xml2::xml_text(xml2::xml_find_all(x = .sosOffering,
                                                  xpath = swesProcedureName,
                                                  ns = namespaces))
  if (sos@verboseOutput) cat("[parseSosObservationOffering_200] procedure:", toString(.observationType), "\n")

  .procedureDescriptionFormat <- xml2::xml_text(xml2::xml_find_all(x = .sosOffering,
                                                                   xpath = swesProcedureDescriptionFormatName,
                                                                   ns = namespaces))
  .procedureDescriptionFormat <- as.list(.procedureDescriptionFormat)
  if (sos@verboseOutput) cat("[parseSosObservationOffering_200] procedure description format:", toString(.observationType), "\n")

  .responseFormat <- xml2::xml_text(xml2::xml_find_all(x = .sosOffering,
                                                       xpath = sos200ResponseFormatName,
                                                       ns = namespaces))
  .responseFormat <- as.list(.responseFormat)
  if (sos@verboseOutput) cat("[parseSosObservationOffering_200] responseFormat:", toString(.responseFormat), "\n")

  .resultTimeXml <- xml2::xml_child(x = .sosOffering, search = sos200ResultTimeName, ns = namespaces)
  if (!is.na(.resultTimeXml)) {
    .resultTime <- parseTimeGeometricPrimitiveFromParent(obj = .resultTimeXml,
                                                         format = sosTimeFormat(sos))
    if (sos@verboseOutput) cat("[parseSosObservationOffering_200] resultTime: ", toString(.resultTime), "\n")
  }
  else {
    warning("Mandatory element 'result time' missing in offering", .id)
    .resultTime <- GmlTimeInstant(timePosition = GmlTimePosition(time = as.POSIXct(x = NA)))
  }

  .phenomenonTimeXml <- xml2::xml_child(x = .sosOffering, search = sos200PhenomenonTimeName, ns = namespaces)
  if (!is.na(.resultTimeXml)) {
    .phenomenonTime <- parseTimeGeometricPrimitiveFromParent(obj = .phenomenonTimeXml,
                                                             format = sosTimeFormat(sos))
    if (sos@verboseOutput) cat("[parseSosObservationOffering_200] resultTime: ", toString(.resultTime), "\n")
  }
  else {
    warning("Mandatory element 'phenomenon time' missing in offering", .id)
    .phenomenonTime <- GmlTimeInstant(timePosition = GmlTimePosition(time = as.POSIXct(x = NA)))
  }

  .env <- xml2::xml_find_first(x = .sosOffering,
                               xpath = paste0(sos200ObservedAreaName, "/", gmlEnvelopeName),
                               ns = namespaces)
  if (!is.na(.env)) {
    .observedArea <- list(
      srsName = xml2::xml_attr(x = .env, attr = "srsName"),
      lowerCorner = xml2::xml_text(x = xml2::xml_child(x = .env, search = gmlLowerCornerName)),
      upperCorner = xml2::xml_text(x = xml2::xml_child(x = .env, search = gmlUpperCornerName))
    )

    if (sosSwitchCoordinates(sos)) {
      warning("Switching coordinates in envelope of ObservationOffering!")
      .origLC <- strsplit(x = .observedArea[["lowerCorner"]], split = " ")
      .lC <- paste(.origLC[[1]][[2]], .origLC[[1]][[1]])
      .origUC <- strsplit(x = .observedArea[["upperCorner"]], split = " ")
      .uC <- paste(.origUC[[1]][[2]], .origUC[[1]][[1]])
      .observedArea <- list(srsName = xml2::xml_attr(x = .env, attr = "srsName"),
                            lowerCorner = .lC, upperCorner = .uC)
    }

    if (sos@verboseOutput) cat("[parseSosObservationOffering_200] boundedBy:", toString(.observedArea), "\n")
  }
  else {
    .observedArea <- list()
  }

  .ob <- new("SosObservationOffering_2.0.0",
             id = .id,
             name = .name,
             resultTime = .resultTime,
             phenomenonTime = .phenomenonTime,
             procedure = .procedure,
             observableProperty = .observableProperty,
             featureOfInterestType = .featureOfInterestType,
             observationType = .observationType,
             responseFormat = .responseFormat,
             procedureDescriptionFormat = .procedureDescriptionFormat,
             observedArea = .observedArea)

  if (sos@verboseOutput)
    cat("[parseSosObservationOffering] done: ", toString(.ob), "\n")

  return(.ob)

}

parseGetObservationResponse <- function(obj, sos, verbose = FALSE) {
  if (sos@verboseOutput) {
    cat("[parseGetObservationResponse] entering... \n")
    print(obj)
  }

  .observationsXML <- xml2::xml_find_all(x = obj,
                                         xpath = "sos20:observationData",
                                         ns = SosAllNamespaces(version = sos200_version))
  featureCache <- list()
  .observations <- sapply(.observationsXML,
                         parseObservation_2.0,
                         sos = sos,
                         featureCache = featureCache)

  return(.observations)
}

parseGetFeatureOfInterestResponse <- function(obj, sos, verbose = FALSE) {

  if (sos@verboseOutput) {
    cat("[parseGetFeatureOfInterestResponse] entering... \n")
    print(obj)
  }

  .featureXML <- xml2::xml_find_all(x = obj,
                                    xpath = "sos20:featureMember",
                                    SosAllNamespaces(version = sos200_version))
  .foi = sapply(.featureXML, .parseFeatureMember, sos = sos)
  return(.foi)
}

.parseFeatureMember <- function(obj, sos) {
  .member <- xml2::xml_child(x = obj)

  .name <- xml2::xml_name(x = .member, ns = SosAllNamespaces(version = sos200_version))

  if (.name == wmlMonitoringPointName) {
    .sp <- parseMonitoringPoint(.member, sos = sos)
    .member.parsed <- GmlFeatureProperty(feature = .sp)
  }
  else if (.name == samsSamplingFeatureName) {
    .sf <- parseSams200SamplingFeature(obj = .member, sos = sos)
    .member.parsed <- GmlFeatureProperty(feature = .sf)
  }
  else {
    warning(paste("No handling for given sos:featureMember available: ", .name))
  }
  return(.member.parsed)
}
