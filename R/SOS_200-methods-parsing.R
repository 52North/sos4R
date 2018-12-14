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
# Author: Benjamin Pross (b.pross@52north.org)                                 #
# Created: 2013-03-06                                                          #
# Project: sos4R - visit the project web page,                                 #
#      http://52north.org/communities/sensorweb/clients/sos4R/                 #
################################################################################

#
# function for parsing Capabilities SOS v 2.0.0
#
parseSosCapabilities200 <- function(obj, sos) {
  if(sos@verboseOutput)
    cat("[parseSosCapabilities] entered... \n")

  .caps.root <- xmlRoot(obj)

  # attributes:
  .caps.version <- xmlGetAttr(node = .caps.root, name = "version",
                              default = NA_character_)
  .caps.updateSequence <- xmlGetAttr(node = .caps.root,
                                     name = "updateSequence", default = NA_character_)
  if(sos@verboseOutput)
    cat("[parseSosCapabilities] version, update sequence:", .caps.version,
        .caps.updateSequence, "\n")

  if(!is.null(.caps.root[[owsServiceIdentificationName]])) {
    .caps.si <- parseOwsServiceIdentification(
      .caps.root[[owsServiceIdentificationName]])
  }
  else .caps.si <- NULL

  if(!is.null(.caps.root[[owsServiceProviderName]])) {
    .caps.sp <- parseOwsServiceProvider(.caps.root[[owsServiceProviderName]])
  }
  else .caps.sp <- NULL

  if(!is.null(.caps.root[[owsOperationsMetadataName]])) {
    if(sos@verboseOutput)
      cat("[parseSosCapabilities] entering", owsOperationsMetadataName,
          "... \n")

    .operationsXML <- .filterXmlChildren(
      node = .caps.root[[owsOperationsMetadataName]],
      xmlTagName = owsOperationName)

    .operations <- lapply(.operationsXML, parseOwsOperation)
    # add names for indexing of list
    names(.operations) <- lapply(.operations,
                                 function(obj) {
                                   return(obj@name)
                                 })
    .caps.om <- OwsOperationsMetadata(operations = .operations)
  }
  else .caps.om <- NULL

  #  <sos:contents>
  #     <sos:Contents>
  if(!is.null(.caps.root[[sos200ContentsName]])) {
    if(sos@verboseOutput)
      cat("[parseSosCapabilities] entering", sos200ContentsName, "... \n")

    .offeringsXML <- .filterXmlChildren(
      node = .caps.root[[sos200ContentsName]][[sosContentsName]],
      xmlTagName = swesOfferingName)
    .observations = sapply(.offeringsXML, parseSosObservationOffering_200,
                           sos = sos)
    # add names to list
    names(.observations) <- lapply(.observations,
                                   function(obj) {
                                     return(obj@id)
                                   })

    .caps.contents <- SosContents(observationOfferings = .observations)
  }
  else .caps.contents <- NULL

  #TODO implement if needed
  #if(!is.null(.caps.root[[sos200FilterCapabilitiesName]])) {
  #  if(sos@verboseOutput)
  #    cat("[parseSosCapabilities] entering", sos200FilterCapabilitiesName,
  #        "... \n")
  #
  #  .caps.fc <- parseFesFilter_Capabilities(
  #    obj = .caps.root[sos200FilterCapabilitiesName][[sosFilterCapabilitiesName]], sos = sos)
  #}
  #else .caps.fc <- NULL

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
  .sosOffering = xmlChildren(obj)[[sosObservationOfferingName]]
  #if(!is.na(.sosOffering)){
  #  parseSosObservationOffering(.sosOffering, sos)
  #}

  if(sos@verboseOutput) {
    cat("[parseSosObservationOffering_200] entering... \n")
    print(.sosOffering)
  }

  # not optional, but have a default just in case...
  .id <- xmlValue(.sosOffering[[swesIdentifierName]])
  .name <- xmlValue(.sosOffering[[swesNameName]])
  if(sos@verboseOutput)
    cat("[parseSosObservationOffering_200] id:", .id, "name:", .name, "\n")

  # can be references or contained inline, so use lists
  .observableProperty <- parseSwesObservableProperty(.sosOffering[swesObservablePropertyName],
                                                     verbose = sos@verboseOutput)

  if(sos@verboseOutput)
    cat("[parseSosObservationOffering_200] observableProperty:",
        toString(.observableProperty), "\n")
  #.featureOfInterestType <- .sosOffering[sosFeatureOfInterestTypeName]

  .featureOfInterestType <- lapply(.sosOffering[sosFeatureOfInterestTypeName], xmlValue)

  if(sos@verboseOutput)
    cat("[parseSosObservationOffering_200] featureOfInterestType:",
        toString(.featureOfInterestType), "\n")

  .observationType <- lapply(.sosOffering[sosObservationTypeName], xmlValue)

  if(sos@verboseOutput)
    cat("[parseSosObservationOffering_200] observationType:",
        toString(.observationType), "\n")

  # can be transformed to character vectors
  # this works, but its really swes:procedure element NOT 1...* sos:procedure elements for SOS v 2.0.0
  .procedure <- xmlValue(.sosOffering[[sosProcedureName]])

  # handle missing procedures
  if(.procedure == "") {
    .procedure <- as.character(NA)
    warning(paste("Mandatory element 'procedure' missing in offering",
                  .id))
  }
  if(sos@verboseOutput)
    cat("[parseSosObservationOffering_200] procedure:",
        toString(.procedure), "\n")

  if(!length(.sosOffering[swesProcedureDescriptionFormatName]) < 1) {
    .procedureDescriptionFormat <- lapply(.sosOffering[swesProcedureDescriptionFormatName], xmlValue)
    if(sos@verboseOutput)
      cat("[parseSosObservationOffering_200] procedureDescriptionFormat:",
          toString(.procedureDescriptionFormat), "\n")
  }
  else {
    .procedureDescriptionFormat <- NA_character_
    warning(paste("Mandatory element 'procedureDescriptionFormat' missing in offering",
                  .id))
  }

  #
  # not optional, but potentially missing in some instances...
  #
  if(!length(.sosOffering[sosResponseFormatName]) < 1) {
    .responseFormat <- lapply(.sosOffering[sosResponseFormatName], xmlValue)
    if(sos@verboseOutput)
      cat("[parseSosObservationOffering_200] responseFormat:",
          toString(.responseFormat), "\n")
  }
  else {
    .responseFormat <- NA_character_
    warning(paste("Mandatory element 'responseFormat' missing in offering",
                  .id))
  }

  if(!is.null(.sosOffering[[sosResultTimeName]])) {
    .resultTime <- parseTimeGeometricPrimitiveFromParent(obj = .sosOffering[[sosResultTimeName]],
                                                         format = sosTimeFormat(sos))
    if(sos@verboseOutput)
      cat("[parseSosObservationOffering_200] resultTime: ", toString(.resultTime), "\n")
  }
  else {
    warning("Mandatory element 'time' missing in offering", .id)
    .resultTime <- GmlTimeInstant(timePosition = GmlTimePosition(
      time = as.POSIXct(x = NA)))
  }

  if(!is.null(.sosOffering[[sosPhenomenonTimeName]])) {
    .phenomenonTime <- parseTimeGeometricPrimitiveFromParent(obj = .sosOffering[[sosPhenomenonTimeName]],
                                                             format = sosTimeFormat(sos))
    if(sos@verboseOutput)
      cat("[parseSosObservationOffering_200] phenomenonTime: ", toString(.phenomenonTime), "\n")
  }
  else {
    warning("Mandatory element 'time' missing in offering", .id)
    .phenomenonTime <- GmlTimeInstant(timePosition = GmlTimePosition(
      time = as.POSIXct(x = NA)))
  }

  #
  # optional, so check if list is empty!
  #
  .resultModel <- sapply(.sosOffering[sosResultModelName], xmlValue)
  if(length(.resultModel) == 0) .resultModel <- NA_character_
  .intendedApplication <- sapply(.sosOffering[sosIntendedApplicationName], xmlValue)
  if(length(.intendedApplication) == 0) .intendedApplication <- NA_character_

  .env <- .sosOffering[[sosObservedAreaName]][[gmlEnvelopeName]]
  if(!is.null(.env)) {
    .observedArea <- list(
      srsName = xmlGetAttr(.env, "srsName"),
      lowerCorner = xmlValue(.env[[gmlLowerCornerName]]),
      upperCorner = xmlValue(.env[[gmlUpperCornerName]]))

    if(sosSwitchCoordinates(sos)) {
      warning("Switching coordinates in envelope of ObservationOffering!")
      .origLC <- strsplit(x = .observedArea[["lowerCorner"]], split = " ")
      .lC <- paste(.origLC[[1]][[2]], .origLC[[1]][[1]])
      .origUC <- strsplit(x = .observedArea[["upperCorner"]], split = " ")
      .uC <- paste(.origUC[[1]][[2]], .origUC[[1]][[1]])
      .observedArea <- list(srsName = xmlGetAttr(.env, "srsName"),
                            lowerCorner = .lC, upperCorner = .uC)
    }

    if(sos@verboseOutput)
      cat("[parseSosObservationOffering_200] boundedBy:",
          toString(.observedArea), "\n")
  }
  else {
    .observedArea <- list()
  }

  # warn if time or envelope is missing -> probably sensor without data.
  .warningText <- ""
  if(length(.observedArea) < 1) {
    .warningText <- "\t'sos:observedArea' is NA/empty.\n"
  }
  if(extends(class(.resultTime), "GmlTimeInstant") &&
     is.na(.resultTime@timePosition@time)) {
    .warningText <- paste(.warningText, "\t'sos:resultTime' is NA/empty.\n")
  }
  if(extends(class(.phenomenonTime), "GmlTimeInstant") &&
     is.na(.phenomenonTime@timePosition@time)) {
    .warningText <- paste(.warningText, "\t'sos:phenomenonTime' is NA/empty.\n")
  }
  if(length(.warningText) > 1) {
    warning(paste("Error when parsing offering '", .id, "':\n",
                  .warningText, sep = ""))
  }

  .ob <- new("SosObservationOffering_2.0.0", id = .id, name = .name,
             resultTime = .resultTime,
             phenomenonTime = .phenomenonTime,
             procedure = .procedure,
             observableProperty = .observableProperty,
             featureOfInterestType = .featureOfInterestType,
             observationType = .observationType,
             responseFormat = .responseFormat,
             procedureDescriptionFormat = .procedureDescriptionFormat,
             observedArea = .observedArea)

  if(sos@verboseOutput)
    cat("[parseSosObservationOffering] done: ", toString(.ob), "\n")

  return(.ob)

}


parseGetObservationResponse <- function(obj, sos, verbose = FALSE) {

  if(sos@verboseOutput) {
    cat("[parseGetObservationResponse] entering... \n")
    print(obj)
  }

  .observationsXML <- .filterXmlChildren(node = obj, xmlTagName = "observationData")
  featureCache <<- list()
  .observations <- sapply(.observationsXML,
                         parseObservation_2.0,
                         sos = sos)
  featureCache <<- list()
  return(.observations)
}

parseGetFeatureOfInterestResponse <- function(obj, sos, verbose = FALSE) {

  if(sos@verboseOutput) {
    cat("[parseGetFeatureOfInterestResponse] entering... \n")
    print(obj)
  }

  .offeringsXML <- .filterXmlChildren(
    node = obj,
    xmlTagName = "featureMember")
  .foi = sapply(.offeringsXML, .parseFeatureMember,
                sos = sos)
  return(.foi)
}

.parseFeatureMember <- function(obj, sos) {
  .noneTexts <- .filterXmlOnlyNoneTexts(obj)
  .member <- .noneTexts[[1]]

  .name <- xmlName(.member)

  if(.name == wmlMonitoringPointName) {
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
