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
#
#
parseSosObservationOffering <- function(obj, sos) {
  if (sos@verboseOutput) {
    cat("[parseSosObservationOffering] entering... \n")
    print(obj)
  }

  # not optional, but have a default just in case...
  .id <- xml2::xml_attr(x = obj, attr = "id", default = NA_character_)
  .name <- xml2::xml_text(x = xml2::xml_child(x = obj, search = gmlNameName, ns = SosAllNamespaces()))
  if (sos@verboseOutput) cat("[parseSosObservationOffering] id:", .id, "name:", .name, "\n")

  .observedProperty <- parseSosObservedProperty(xml2::xml_find_all(x = obj,
                                                                   xpath = sosObservedPropertyName,
                                                                   ns = SosAllNamespaces()),
                                                verbose = sos@verboseOutput)
  if (sos@verboseOutput) cat("[parseSosObservationOffering] observedProperty:", toString(.observedProperty), "\n")

  .featureOfInterestXml <- xml2::xml_find_all(x = obj,
                                              xpath = sosFeatureOfInterestName,
                                              ns = SosAllNamespaces())
  .featureOfInterest <- lapply(X = .featureOfInterestXml,
                              FUN = xml2::xml_attr,
                              attr = "xlink:href",
                              ns = SosAllNamespaces())
  if (sos@verboseOutput) cat("[parseSosObservationOffering] featureOfInterest:", toString(.featureOfInterest), "\n")

  .procedureXml <- xml2::xml_find_all(x = obj,
                                      xpath = sosProcedureName,
                                      ns = SosAllNamespaces())
  .procedure <- sapply(X = .procedureXml,
                       FUN = xml2::xml_attr,
                       attr = "xlink:href",
                       ns = SosAllNamespaces())
  if (sos@verboseOutput) cat("[parseSosObservationOffering] procedure:", toString(.procedure), "\n")

  .responseFormat <- xml2::xml_text(xml2::xml_find_all(x = obj,
                                                       xpath = sosResponseFormatName,
                                                       ns = SosAllNamespaces()))
  if (sos@verboseOutput) cat("[parseSosObservationOffering] responseFormat:", toString(.responseFormat), "\n")

  .responseMode <- xml2::xml_text(xml2::xml_find_all(x = obj,
                                                       xpath = sosResponseModeName,
                                                       ns = SosAllNamespaces()))
  if (sos@verboseOutput) cat("[parseSosObservationOffering] responseMode:", toString(.responseMode), "\n")

  .timeXml <- xml2::xml_child(x = obj, search = sosTimeName, ns = SosAllNamespaces())
  if (!is.na(.timeXml)) {
    .time <- parseTimeGeometricPrimitiveFromParent(obj = .timeXml,
                                                   format = sosTimeFormat(sos))
    if (sos@verboseOutput) cat("[parseSosObservationOffering] time: ", toString(.time), "\n")
  }
  else {
    warning("Mandatory element 'time' missing in offering", .id)
    .time <- GmlTimeInstant(timePosition = GmlTimePosition(time = as.POSIXct(x = NA)))
  }

  .resultModel <- xml2::xml_text(xml2::xml_find_all(x = obj,
                                                    xpath = sosResultModelName,
                                                    ns = SosAllNamespaces()))
  if (sos@verboseOutput) cat("[parseSosObservationOffering] resultModel:", toString(.resultModel), "\n")

  .intendedApplication <- xml2::xml_text(xml2::xml_find_all(x = obj,
                                                    xpath = sosIntendedApplicationName,
                                                    ns = SosAllNamespaces()))
  if (sos@verboseOutput) cat("[parseSosObservationOffering] intendedApplication:", toString(.intendedApplication), "\n")

  .env <- xml2::xml_find_first(x = obj,
                               xpath = paste0(gmlBoundedByName, "/", gmlEnvelopeName),
                               ns = SosAllNamespaces())
  if (!is.na(.env)) {
    .boundedBy <- list(
      srsName = xml2::xml_attr(x = .env, attr = "srsName"),
      lowerCorner = xml2::xml_text(x = xml2::xml_child(x = .env, search = gmlLowerCornerName)),
      upperCorner = xml2::xml_text(x = xml2::xml_child(x = .env, search = gmlUpperCornerName))
      )

    if (sosSwitchCoordinates(sos)) {
      warning("Switching coordinates in envelope of ObservationOffering!")
      .origLC <- strsplit(x = .boundedBy[["lowerCorner"]], split = " ")
      .lC <- paste(.origLC[[1]][[2]], .origLC[[1]][[1]])
      .origUC <- strsplit(x = .boundedBy[["upperCorner"]], split = " ")
      .uC <- paste(.origUC[[1]][[2]], .origUC[[1]][[1]])
      .boundedBy <- list(srsName = xml2::xml_attr(x = .env, attr = "srsName"),
                         lowerCorner = .lC, upperCorner = .uC)
    }

    if (sos@verboseOutput) cat("[parseSosObservationOffering] boundedBy:", toString(.boundedBy), "\n")
  }
  else {
    .boundedBy <- list()
  }

  .ob <- SosObservationOffering(id = .id,
                                name = .name,
                                time = .time,
                                procedure = .procedure,
                                observedProperty = .observedProperty,
                                featureOfInterest = .featureOfInterest,
                                responseFormat = .responseFormat,
                                intendedApplication = .intendedApplication,
                                resultModel = .resultModel,
                                responseMode = .responseMode,
                                boundedBy = .boundedBy)

  if (sos@verboseOutput)
    cat("[parseSosObservationOffering] done: ", toString(.ob), "\n")

  return(.ob)
}

#
#
#
parseSosObservedProperty <- function(obj, verbose = FALSE) {
  if (verbose) cat("[parseSosObservedProperty] entered")

  .name <- xml2::xml_name(x = obj, ns = SosAllNamespaces())
  if (verbose) cat("[parseSosObservedProperty] found ", .name, "\n")

  .href <- xml2::xml_attr(x = obj, attr = "href")
  if (!is.na(.href)) {
    if (verbose) cat("[parseSosObservedProperty] found href:", .href, "\n")
    return(.href)
  }
  else  {
    .comp <- xml2::xml_child(x = obj, search = sweCompositePhenomenonName)

    if (!is.na(.comp)) {
      .parsed <- parseCompositePhenomenon(.comp)
      .id <- slot(.parsed, "id")
      if (verbose) cat("[parseSosObservedProperty] parsed phenomenon: ", toString(.parsed),
                       ", returning id '", .id, "' ONLY.\n", sep = "")
      return(.id)
    }

    warning(paste("could not parse observed property:", toString(obj)))
    return(NULL)
  }
}

parseSosObservedProperty <- function(obj, verbose = FALSE) {
  if (verbose) cat("[parseSosObservedProperty] entered\n")

  if (inherits(x = obj, "xml_nodeset")) {
    if (verbose) cat("[parseSosObservedProperty] have a node set with", length(obj), "elements.\n")
    .obsProps <- lapply(X = obj, FUN = parseSosObservedProperty, verbose = verbose)
    return(.obsProps)
  }

  .name <- xml2::xml_name(x = obj, ns = SosAllNamespaces())
  if (verbose) cat("[parseSosObservedProperty] found ", .name, "\n")

  .href <- xml2::xml_attr(x = obj, attr = "href")
  if (!is.na(.href)) {
    if (verbose) cat("[parseSosObservedProperty] found href:", .href, "\n")
    return(.href)
  }
  else  {
    .comp <- xml2::xml_child(x = obj, search = sweCompositePhenomenonName)

    if (!is.na(.comp)) {
      .parsed <- parseCompositePhenomenon(.comp)
      .id <- slot(.parsed, "id")
      if (verbose) cat("[parseSosObservedProperty] parsed phenomenon: ", toString(.parsed),
                       ", returning id '", .id, "' ONLY.\n", sep = "")
      return(.id)
    }

    warning(paste("could not parse observed property:", toString(obj)))
    return(NULL)
  }
}

parseSosCapabilities <- function(obj, sos) {
  if (sos@version == sos100_version){
    .caps <- parseSosCapabilities100(obj, sos)
  }
  else if (sos@version == sos200_version){
    .caps <- parseSosCapabilities200(obj, sos)
  }
}

parseSwesObservableProperty <- function(obj, verbose = FALSE) {
  if (verbose) cat("[parseSwesObservableProperty] entered\n")

  if (inherits(x = obj, "xml_nodeset")) {
    if (verbose) cat("[parseSwesObservableProperty] have a node set with", length(obj), "elements.\n")
    .obsProps <- lapply(X = obj, FUN = parseSwesObservableProperty, verbose = verbose)
    return(.obsProps)
  }

  .name <- xml2::xml_text(x = obj)
  if (!is.null(.name)) {
    if (verbose)
      cat("[parseSwesObservableProperty] found ", .name, "\n")
    return(.name)
  }
  else  {
    warning(paste("could not parse observable property:", toString(obj))) # warn ???
  }
}


#
# function for parsing Capabilities SOS v 1.0.0
#
parseSosCapabilities100 <- function(obj, sos) {
  if (sos@verboseOutput) cat("[parseSosCapabilities] entered... \n")

  .caps.root <- xml2::xml_root(x = obj)

  # attributes:
  .caps.version <- xml2::xml_attr(x = .caps.root, attr = "version", default = NA_character_, ns = SosAllNamespaces())
  .caps.updateSequence <- xml2::xml_attr(x = .caps.root, attr = "updateSequence", default = NA_character_, ns = SosAllNamespaces())
  if (sos@verboseOutput)
    cat("[parseSosCapabilities] version, update sequence:", .caps.version, .caps.updateSequence, "\n")

  .owsSI <- xml2::xml_child(x = .caps.root, search = owsServiceIdentificationName, ns = SosAllNamespaces())
  if (!is.na(.owsSI))
    .caps.si <- parseOwsServiceIdentification(.owsSI)
  else .caps.si <- NULL

  .owsSP <- xml2::xml_child(x = .caps.root, search = owsServiceProviderName, ns = SosAllNamespaces())
  if (!is.na(.owsSP))
    .caps.sp <- parseOwsServiceProvider(.owsSP)
  else .caps.sp <- NULL

  .owsOM <- xml2::xml_child(x = .caps.root, search = owsOperationsMetadataName, ns = SosAllNamespaces())
  if (!is.na(.owsOM)) {
    if (sos@verboseOutput) cat("[parseSosCapabilities] entering", owsOperationsMetadataName, "... \n")

    .operationsXML <- xml2::xml_find_all(x = .caps.root,
                                         xpath = paste0(owsOperationsMetadataName, "/", owsOperationName),
                                         ns = SosAllNamespaces())

    .operations <- lapply(.operationsXML, parseOwsOperation)
    # add names for indexing of list
    names(.operations) <- lapply(.operations,
                                 function(obj) {
                                   return(obj@name)
                                 })
    .caps.om <- OwsOperationsMetadata(operations = .operations)
  }
  else .caps.om <- NULL

  .sosContents <- xml2::xml_child(x = .caps.root, search = sosContentsName, ns = SosAllNamespaces())
  if (!is.na(.sosContents)) {
    if (sos@verboseOutput) cat("[parseSosCapabilities] entering", sosContentsName, "... \n")

    .observationsXML <- xml2::xml_find_all(x = .sosContents,
                                           xpath = paste0(sosObservationOfferingListName,
                                                          "/", sosObservationOfferingName),
                                           ns = SosAllNamespaces())
    .observations = sapply(X = .observationsXML,
                           FUN = parseSosObservationOffering,
                           sos = sos)
    # add names to list
    names(.observations) <- lapply(.observations,
                                   function(obj) {
                                     return(obj@id)
                                   })

    .caps.contents <- SosContents(observationOfferings = .observations)
  }
  else .caps.contents <- NULL

  .sosFC <- xml2::xml_child(x = .caps.root, search = sosFilterCapabilitiesName, ns = SosAllNamespaces())
  if (!is.na(.sosFC)) {
    if (sos@verboseOutput)
      cat("[parseSosCapabilities] entering", sosFilterCapabilitiesName, "... \n")

    .caps.fc <- parseSosFilter_Capabilities(obj = .sosFC, sos = sos)
  }
  else .caps.fc <- NULL

  .capabilities <- SosCapabilities(version = .caps.version,
                                   updateSequence = .caps.updateSequence,
                                   identification = .caps.si,
                                   provider = .caps.sp,
                                   operations = .caps.om,
                                   filterCapabilities = .caps.fc,
                                   contents = .caps.contents)

  return(.capabilities)
}

parseSosFilter_Capabilities <- function(obj, sos) {
  if (sos@verboseOutput) cat("[parseSosFilter_Capabilities] entering... \n")

  .s <- xml2::xml_child(x = obj, search = ogcSpatialCapabilitiesName, ns = SosAllNamespaces())
  if (!is.na(.s)) {
    if (sos@verboseOutput) cat("[parseSosFilter_Capabilities] parsing", ogcSpatialCapabilitiesName, "\n")

    .geometryOperands <- xml2::xml_find_all(x = .s, xpath = ogcGeometryOperandsName, ns = SosAllNamespaces())
    if (!is.na(.geometryOperands)) {
      .spatial.geom <- xml2::xml_find_all(x = .geometryOperands, xpath = ogcGeometryOperandName, ns = SosAllNamespaces())
      .spatial.geom.values <- xml2::xml_text(.spatial.geom)
    }
    else {
      .spatial.geom <- NA_character_
      .spatial.geom.values <- NA_character_
      warning(paste("Mandatory element", ogcGeometryOperandsName,
                    "missing in", ogcSpatialCapabilitiesName))
    }

    .spatialOperators <- xml2::xml_find_all(x = .s, xpath = ogcSpatialOperatorsName, ns = SosAllNamespaces())
    if (!is.na(.spatialOperators)) {
      .spatial.spat <- xml2::xml_find_all(x = .spatialOperators, xpath = ogcSpatialOperatorName, ns = SosAllNamespaces())
      .spatial.spat.values <- xml2::xml_attr(x = .spatial.spat, attr = "name")
    }
    else {
      .spatial.spat <- NA_character_
      .spatial.spat.values <- NA_character_
      warning(paste("Mandatory element", ogcSpatialOperatorsName,
                    "missing in", ogcSpatialCapabilitiesName))
    }

    .spatial <- list(.spatial.geom.values, .spatial.spat.values)
    names(.spatial) <- c(ogcGeometryOperandsName, ogcSpatialOperatorsName)
  }
  else {
    .spatial <- list(NA_character_)
    warning(paste("Mandatory element", ogcSpatialCapabilitiesName,
                  "missing in", sosFilterCapabilitiesName))
  }

  if (!is.na(xml2::xml_child(x = obj, search = ogcTemporalCapabilitiesName, ns = SosAllNamespaces()))) {
    if (sos@verboseOutput) cat("[parseSosFilter_Capabilities] parsing", ogcTemporalCapabilitiesName, "\n")

    .temporal.ands <- xml2::xml_find_all(x = obj,
                                         xpath = paste0(ogcTemporalCapabilitiesName,
                                                        "/", ogcTemporalOperandsName,
                                                        "/", ogcTemporalOperandName),
                                         ns = SosAllNamespaces())
    .temporal.ators <- xml2::xml_find_all(x = obj,
                                          xpath = paste0(ogcTemporalCapabilitiesName,
                                                         "/", ogcTemporalOperatorsName,
                                                         "/", ogcTemporalOperatorName),
                                          ns = SosAllNamespaces())
    .temporal <- list(xml2::xml_text(.temporal.ands), xml2::xml_attr(x = .temporal.ators, attr = "name"))
    names(.temporal) <- c(ogcTemporalOperandsName, ogcTemporalOperatorsName)
  }
  else {
    .temporal <- list(NA_character_)
    warning(paste("Mandatory element", ogcTemporalCapabilitiesName,
                  "missing in", sosFilterCapabilitiesName))
  }

  .scalarXML <- xml2::xml_child(x = obj, search = ogcScalarCapabilitiesName, ns = SosAllNamespaces())
  if (!is.null(.scalarXML)) {
    if (sos@verboseOutput)
      cat("[parseSosFilter_Capabilities] parsing",
          ogcScalarCapabilitiesName, "\n")

    .scalar <- list()

    .scalar.logicalXML <- xml2::xml_child(x = .scalarXML,
                                          search = ogcLogicalOperatorsName,
                                          ns = SosAllNamespaces())
    if (!is.na(.scalar.logicalXML)) {
      .scalar.logical <- xml2::xml_text(.scalar.logicalXML)
      .scalar <- c(.scalar, .scalar.logical)
    }

    .scalar.compXML <- xml2::xml_child(x = .scalarXML,
                                          search = ogcComparisonOperatorsName,
                                          ns = SosAllNamespaces())
    if (!is.na(.scalar.compXML)) {
      .scalar.comp <- xml2::xml_text(xml2::xml_children(x = .scalar.compXML))
      .scalar <- c(.scalar, .scalar.comp)
    }

    .scalar.arithmXML <- xml2::xml_child(x = .scalarXML,
                                       search = ogcArithmeticOperatorsName,
                                       ns = SosAllNamespaces())
    if (!is.na(.scalar.arithmXML)) {
      .scalar.arithm <- xml2::xml_text(.scalar.arithmXML)
      .scalar <- c(.scalar, .scalar.arithm)
    }
  }
  else {
    .scalar <- list(NA_character_)
    warning(paste("Mandatory element", ogcScalarCapabilitiesName,
                  "missing in", sosFilterCapabilitiesName))
  }

  .idXml <- xml2::xml_child(x = obj, search = ogcIdCapabilities, ns = SosAllNamespaces())
  if (!is.na(.idXml)) {
    if (sos@verboseOutput) cat("[parseSosFilter_Capabilities] parsing", ogcIdCapabilities, "\n")

    .id <- as.list(xml2::xml_name(xml2::xml_children(.idXml), ns = SosAllNamespaces()))
  }
  else {
    .id <- list(NA_character_)
    warning(paste("Mandatory element", ogcIdCapabilities,
                  "missing in", sosFilterCapabilitiesName))
  }

  .fc <- SosFilter_Capabilities(spatial = .spatial,
                                temporal = .temporal,
                                scalar = .scalar,
                                id = .id)

  if (sos@verboseOutput) cat("[parseSosFilter_Capabilities] done:", toString(.fc), "\n")

  return(.fc)
}


#
# parse saved documents from files ----
#
setMethod(f = "parseFile",
          signature = signature(sos = "SOS_versioned", file = "character"),
          definition = function(sos, file, verbose, ...) {
            .parsed <- xml2::read_xml(x = file, ...)
            .name <- xml2::xml_name(x = xml2::xml_root(x = .parsed), ns = SosAllNamespaces())

            if (verbose) cat("[parseFile] root", .name, "\n")

            if (.name == smlSensorMLName) {
              .opName <- sosDescribeSensorName
            }
            else if (.name == omObservationCollectionName ||
                    .name == omObservationName ||
                    .name == omMeasurementName)  {
              .opName <- sosGetObservationName
            }
            else if (.name == owsExceptionReportName) {
              if (verbose) cat("[parseFile] Parsing ExceptionReport...\n")

              .opName <- owsExceptionReportName
              .parsingFunction <- sosParsers(sos)[[.opName]]
              .obj <- .parsingFunction(obj = .parsed, verbose = verbose)

              if (verbose) cat("[parseFile] done\n")

              return(.obj)
            }
            else {
              stop(paste("Root element", .name, "not supported by file parser!"))
            }

            if (verbose) cat("[parseFile] parsing with parser for operation", .opName, "\n")
            # check if parsers are disabled, then just return the parsed object?

            .parsingFunction <- sosParsers(sos)[[.opName]]
            .obj <- .parsingFunction(obj = .parsed, sos = sos, verbose = verbose)

            if (verbose) cat("[parseFile] done\n")

            return(.obj)
          }
)

parseCSV <- function(obj, verbose = FALSE) {
  if (verbose) cat("[parseCSV] Parsing CSV...\n")

  if (inherits(x = obj, what = "data.frame")) {
    if (verbose) cat("[parseCSV] Already a data.frame, returning object\n")
    return(obj)
  }

  .lines <- strsplit(x = obj, split = "\n")[[1]]
  .data <- do.call(what = "strsplit", args = list(.lines, split = ","))

  # clean up names (double quotes)
  .names <- .data[[1]]
  .newNames <- c()
  for (.n in .names) {
    .newNames <- c(.newNames,
                   gsub(pattern = "\"", replacement = "", x = .n))
  }
  .names <- .newNames

  .rows <- length(.data)
  if (verbose) cat("[parseCSV] Got", .rows, "lines of data.\n")

  .df <- NULL
  for (.r in seq(2,.rows)) {
    if (verbose) cat("[parseCSV] Processing row in CSV:", .data[[.r]], "\n")

    # initialize first column of the data frame so it can be bound in loop
    .row.df <- as.data.frame(.data[[.r]][1])
    names(.row.df) <- .names[[1]]

    for (i in seq(2,length(.names))) {
      .df <- as.data.frame(.data[[.r]][i])
      names(.df) <- .names[[i]]
      .row.df <- cbind(.row.df, .df)
    }

    if (is.null(.df))
      .df <- .row.df
    else
      .df <- do.call(rbind, list(.df, .row.df))
  }

  if (verbose) cat("[parseCSV] Done.\n")

  return(.df)
}


#
# parse KML ----
#
parseKML <- function(obj, sos, verbose = FALSE) {
  if (verbose) cat("[parseKML] Processing KML... returning raw object!\n")

  return(obj)
}

