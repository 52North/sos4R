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
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)                          #
# Created: 2010-06-18                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

#
#
#
parseSosObservationOffering <- function(obj, sos) {
	if(sos@verboseOutput) {
		cat("[parseSosObservationOffering] entering... \n")
		print(obj)
	}
	
	# not optional, but have a default just in case...
	.id <- xmlGetAttr(node = obj, name = "id", default = NA_character_)
	.name <- xmlValue(obj[[gmlNameName]])
	if(sos@verboseOutput)
		cat("[parseSosObservationOffering] id:", .id, "name:", .name, "\n")
	
	# can be references or contained inline, so use lists
	.observedProperty <- parseSosObservedProperty(obj[sosObservedPropertyName],
	                                              verbose = sos@verboseOutput)
	
	if(sos@verboseOutput)
		cat("[parseSosObservationOffering] observedProperty:",
				toString(.observedProperty), "\n")
	.featureOfInterest <- lapply(obj[sosFeatureOfInterestName], xmlGetAttr,
			"href")
	if(sos@verboseOutput)
		cat("[parseSosObservationOffering] featureOfInterest:", 
				toString(.featureOfInterest), "\n")
	
	# can be transformed to character vectors
	.procedure <- sapply(obj[sosProcedureName], xmlGetAttr, "href")
	# handle missing procedures
	if(length(.procedure) < 1) {
		print("lala")
		.procedure <- as.character(c())
		warning(paste("Mandatory element 'procedure' missing in offering",
						.id))
	}
	if(sos@verboseOutput)
		cat("[parseSosObservationOffering] procedure:",
				toString(.procedure), "\n")
	
	############################################################
	# not optional, but potentially missing in some instances...
	if(!length(obj[sosResponseFormatName]) < 1) {
		.responseFormat <- sapply(obj[sosResponseFormatName], xmlValue)
		if(sos@verboseOutput)
			cat("[parseSosObservationOffering] responseFormat:",
					toString(.responseFormat), "\n")
	}
	else {
		.responseFormat <- NA_character_
		warning(paste("Mandatory element 'responseFormat' missing in offering",
						.id))
	}
	
	if(!length(obj[sosResponseModeName]) < 1) {
		.responseMode <- sapply(obj[sosResponseModeName], xmlValue)
		if(sos@verboseOutput)
			cat("[parseSosObservationOffering] responseMode:",
					toString(.responseMode), "\n")
	}
	else {
		.responseMode <- NA_character_
		warning(paste("Mandatory element 'responseMode' missing in offering",
						.id))
	}
	
	if(!is.null(obj[[sosTimeName]])) {
		.time <- parseTimeGeometricPrimitiveFromParent(obj = obj[[sosTimeName]],
				format = sosTimeFormat(sos))
		if(sos@verboseOutput)
			cat("[parseSosObservationOffering] time: ", toString(.time), "\n")
	}
	else {
		warning("Mandatory element 'time' missing in offering", .id)
		.time <- GmlTimeInstant(timePosition = GmlTimePosition(
						time = as.POSIXct(x = NA)))
	}
	
	##########
	# optional, so check if list is empty!
	.resultModel <- sapply(obj[sosResultModelName], xmlValue)
	if(length(.resultModel) == 0) .resultModel <- NA_character_
	.intendedApplication <- sapply(obj[sosIntendedApplicationName], xmlValue)
	if(length(.intendedApplication) == 0) .intendedApplication <- NA_character_
	
	.env <- obj[[gmlBoundedByName]][[gmlEnvelopeName]]
	if(!is.null(.env)) {
		.boundedBy <- list(
				srsName = xmlGetAttr(.env, "srsName"),
				lowerCorner = xmlValue(.env[[gmlLowerCornerName]]),
				upperCorner = xmlValue(.env[[gmlUpperCornerName]]))
		
		if(sosSwitchCoordinates(sos)) {
			warning("Switching coordinates in envelope of ObservationOffering!")
			.origLC <- strsplit(x = .boundedBy[["lowerCorner"]], split = " ")
			.lC <- paste(.origLC[[1]][[2]], .origLC[[1]][[1]])
			.origUC <- strsplit(x = .boundedBy[["upperCorner"]], split = " ")
			.uC <- paste(.origUC[[1]][[2]], .origUC[[1]][[1]])
			.boundedBy <- list(srsName = xmlGetAttr(.env, "srsName"),
					lowerCorner = .lC, upperCorner = .uC)
		}
		
		if(sos@verboseOutput)
			cat("[parseSosObservationOffering] boundedBy:",
					toString(.boundedBy), "\n")
	}
	else {
		.boundedBy <- list()
	}
	
	# warn if time or envelope is missing -> probably sensor without data.
	.warningText <- ""
	if(length(.boundedBy) < 1) {
		.warningText <- "\t'gml:boundedBy' is NA/empty.\n"
	}
	if(extends(class(.time), "GmlTimeInstant") &&
			is.na(.time@timePosition@time)) {
		.warningText <- paste(.warningText, "\t'sos:time' is NA/empty.\n")
	}
	if(length(.warningText) > 1) {
		warning(paste("Error when parsing offering '", .id, "':\n",
						.warningText, sep = ""))
	}
		
	.ob <- SosObservationOffering(id = .id, name = .name, 
			time = .time, procedure = .procedure,
			observedProperty = .observedProperty,
			featureOfInterest = .featureOfInterest,
			responseFormat = .responseFormat,
			intendedApplication = .intendedApplication,
			resultModel = .resultModel,
			responseMode = .responseMode, boundedBy = .boundedBy)
	
	if(sos@verboseOutput)
		cat("[parseSosObservationOffering] done: ", toString(.ob), "\n")
	
	return(.ob)
}

#
#
#
parseSosObservedProperty <- function(obj = list(), verbose = FALSE) {
    if(verbose)
        cat("[parseSosObservedProperty] entered,", length(obj), "input elements ... \n")
    
    .obsProps <- lapply(X = obj, FUN = function(obj) {
        .name <- xmlName(obj)
        if(verbose)
            cat("[parseSosObservedProperty] found ", .name, "\n")
        
        .href <- xmlGetAttr(node = obj, name = "href")
        if(!is.null(.href)) {
            if(verbose)
                cat("[parseSosObservedProperty] found href:", .href, "\n")
            return(.href)
        }
        else  {
            .comp <- obj[[sweCompositePhenomenonName]]
            str(.comp)
            cat(xmlName(.comp), "\n")
            
            if(!is.null(.comp)) {
                .parsed <- parseCompositePhenomenon(.comp)
                .id <- slot(.parsed, "id")
                if(verbose)
                    cat("[parseSosObservedProperty] parsed phenomenon: ", toString(.parsed), 
                        ", returning id '", .id, "'\n", sep = "")
                return(.id)
            }
            warn(paste("could not parse observed property:", toString(obj)))
        }
    })
        
    return(.obsProps)
}

#
# function for parsing Capabilities
#
parseSosCapabilities <- function(obj, sos) {
  if (sos@version == sos100_version){
    .caps <- parseSosCapabilities100(obj,sos)
  }
  else if (sos@version == sos200_version){
    .caps <- parseSosCapabilities200(obj,sos)
  }
}

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
      childrenName = owsOperationName)
    
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
      childrenName = swesOfferingName)
    .observations = sapply(.offeringsXML, parseSosObservationOffering_200,
                           sos = sos)
    # add names to list
    #names(.observations) <- lapply(.observations,
    #                               function(obj) {
    #                                 return(obj@id)
    #                               })
    
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
  
  ############################################################
  # not optional, but potentially missing in some instances...
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
  
  ##########
  # optional, so check if list is empty!
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

parseSwesObservableProperty <- function(obj, verbose = FALSE) {
  
  if(verbose)
    cat("[parseSwesObservableProperty] entered,", length(obj), "input elements ... \n")
  
  .obsProps <- lapply(X = obj, FUN = function(obj) {
    .name <- xmlValue(obj)
    if(!is.null(.name)) {
      if(verbose)
        cat("[parseSwesObservableProperty] found ", .name, "\n")
      return(.name)
    }
    else  {
      warn(paste("could not parse observable property:", toString(obj)))
    }
  })
  
  return(.obsProps)
  
}


#
# function for parsing Capabilities SOS v 1.0.0
#
parseSosCapabilities100 <- function(obj, sos) {
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
				childrenName = owsOperationName)
		
		.operations <- lapply(.operationsXML, parseOwsOperation)
		# add names for indexing of list
		names(.operations) <- lapply(.operations,
				function(obj) {
					return(obj@name)
				})
		.caps.om <- OwsOperationsMetadata(operations = .operations)
	}
	else .caps.om <- NULL
	
	if(!is.null(.caps.root[[sosContentsName]])) {
		if(sos@verboseOutput)
			cat("[parseSosCapabilities] entering", sosContentsName, "... \n")
		
		.observationsXML <- .filterXmlChildren(
				node = .caps.root[[sosContentsName]][[sosObservationOfferingListName]],
				childrenName = sosObservationOfferingName)
		.observations = sapply(.observationsXML, parseSosObservationOffering,
				sos = sos)
		# add names to list
		names(.observations) <- lapply(.observations,
				function(obj) {
					return(obj@id)
				})
		
		.caps.contents <- SosContents(observationOfferings = .observations)
	}
	else .caps.contents <- NULL
	
	if(!is.null(.caps.root[[sosFilterCapabilitiesName]])) {
		if(sos@verboseOutput)
			cat("[parseSosCapabilities] entering", sosFilterCapabilitiesName,
					"... \n")
		
		.caps.fc <- parseSosFilter_Capabilities(
				obj = .caps.root[[sosFilterCapabilitiesName]], sos = sos)
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
	if(sos@verboseOutput) {
		cat("[parseSosFilter_Capabilities] entering... \n")
		print(obj)
	}
	
	if(!is.null(obj[[ogcSpatialCapabilitiesName]])) {
		if(sos@verboseOutput)
			cat("[parseSosFilter_Capabilities] parsing", 
					ogcSpatialCapabilitiesName, "\n")
		
		.s <- obj[[ogcSpatialCapabilitiesName]]
		
		if(!is.null(.s[[ogcGeometryOperandsName]])) {
			.spatial.geom <- .filterXmlOnlyNoneTexts(
					node = .s[[ogcGeometryOperandsName]])
			.spatial.geom.values <- lapply(.spatial.geom, xmlValue)
		}
		else {
			.spatial.geom <- NA_character_
			.spatial.geom.values <- NA_character_
			warning(paste("Mandatory element", ogcGeometryOperandsName, 
							"missing in", ogcSpatialCapabilitiesName))
		}
		
		if(!is.null(.s[[ogcSpatialOperatorsName]])) {
			.spatial.spat <- .filterXmlOnlyNoneTexts(
					node = .s[[ogcSpatialOperatorsName]])
			.spatial.spat.values <- lapply(.spatial.spat, xmlGetAttr,
					name = "name")
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
	
	if(!is.null(obj[[ogcTemporalCapabilitiesName]])) {
		if(sos@verboseOutput)
			cat("[parseSosFilter_Capabilities] parsing", 
					ogcTemporalCapabilitiesName, "\n")
		
		.temporal.ands <- .filterXmlOnlyNoneTexts(
				node = obj[[ogcTemporalCapabilitiesName]][[ogcTemporalOperandsName]])
		.temporal.ators <- .filterXmlOnlyNoneTexts(
				node = obj[[ogcTemporalCapabilitiesName]][[ogcTemporalOperatorsName]])
		.temporal <- list(lapply(.temporal.ands, xmlValue),
				lapply(.temporal.ators, xmlGetAttr, name = "name"))
		names(.temporal) <- c(ogcTemporalOperandsName, ogcTemporalOperatorsName)
	}
	else {
		.temporal <- list(NA_character_)
		warning(paste("Mandatory element", ogcTemporalCapabilitiesName, 
						"missing in", sosFilterCapabilitiesName))
	}

	.scalarXML <- obj[[ogcScalarCapabilitiesName]]
	if(!is.null(.scalarXML)) {
		if(sos@verboseOutput)
			cat("[parseSosFilter_Capabilities] parsing", 
					ogcScalarCapabilitiesName, "\n")
		
		.scalar <- list()
		if(!is.null(.scalarXML[[ogcLogicalOperatorsName]])) {
			.scalar.logicalXML <- .filterXmlOnlyNoneTexts(
					.scalarXML[[ogcLogicalOperatorsName]])
			.scalar.logical <- lapply(.scalar.logicalXML, xmlValue)
			.scalar <- c(.scalar, .scalar.logical)
		}
		if(!is.null(.scalarXML[[ogcComparisonOperatorsName]])) {
			.scalar.compXML <- .filterXmlOnlyNoneTexts(
					.scalarXML[[ogcComparisonOperatorsName]])
			.scalar.comp <- lapply(.scalar.compXML, xmlValue)
			.scalar <- c(.scalar, .scalar.comp)
		}
		if(!is.null(.scalarXML[[ogcArithmeticOperatorsName]])) {
			.scalar.arithm <- xmlToList(
					.scalarXML[[ogcArithmeticOperatorsName]])
			.scalar <- c(.scalar, .scalar.arithm)
		}
	}
	else {
		.scalar <- list(NA_character_)
		warning(paste("Mandatory element", ogcScalarCapabilitiesName,
						"missing in", sosFilterCapabilitiesName))
	}
	
	if(!is.null(obj[[ogcIdCapabilities]])) {
		if(sos@verboseOutput)
			cat("[parseSosFilter_Capabilities] parsing", 
					ogcIdCapabilities, "\n")
		
		.idXML <- .filterXmlOnlyNoneTexts(obj[[ogcIdCapabilities]])
		.id <- lapply(.idXML, xmlName)
	}
	else {
		.id <- list(NA_character_)
		warning(paste("Mandatory element", ogcIdCapabilities,
						"missing in", sosFilterCapabilitiesName))
	}
	
	.fc <- SosFilter_Capabilities(spatial = .spatial, temporal = .temporal,
			scalar = .scalar, id = .id)
	
	if(sos@verboseOutput)
		cat("[parseSosFilter_Capabilities] done:", toString(.fc), "\n")
	
	return(.fc)
}




################################################################################
# parse saved documents
setMethod(f = "parseFile",
		signature = signature(sos = "SOS_versioned", file = "character"),
		def = function(sos, file, verbose, ...) {
			.parseFile(sos, file, verbose, ...)
		}
)

.parseFile <- function(sos, file, verbose, ...) {
	.parsed <- xmlParse(file, ...)
	.name <- xmlName(xmlRoot(.parsed))
	
	if(verbose) cat("[parseFile] root", .name, "\n")
	
	if(.name == smlSensorMLName) {
		.opName <- sosDescribeSensorName
	}
	else if(.name == omObservationCollectionName ||
			.name == omObservationName ||
			.name == omMeasurementName)  {
		.opName <- sosGetObservationName
	}
	else if(.name == owsExceptionReportName) {
		if(verbose) cat("[parseFile] Parsing ExceptionReport...\n")
		
		.opName <- owsExceptionReportName
		.parsingFunction <- sosParsers(sos)[[.opName]]
		.obj <- .parsingFunction(obj = .parsed, verbose = verbose)
		
		if(verbose) {
			cat("[parseFile] done:\n")
			print(.obj)
		}
		
		return(.obj)
	}
	else {
		stop(paste("Root element", .name, "not supported by file parser!"))
	}
	
	if(verbose) cat("[parseFile] parsing with parser for operation", 
				.opName, "\n")
	# check if parsers are disabled, then just return the parsed object?
	
	.parsingFunction <- sosParsers(sos)[[.opName]]
	.obj <- .parsingFunction(obj = .parsed, sos = sos,
			verbose = verbose)
	
	if(verbose) {
		cat("[parseFile] done:\n")
		print(.obj)
	}
	
	return(.obj)
}

################################################################################
# commma seperated values
#
parseCSV <- function(obj, verbose = FALSE) {
	if(verbose) cat("[parseCSV] Processing CSV...\n")
	
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
	if(verbose) cat("[parseCSV] Got", .rows, "lines of data.\n")
	
	if(.rows == 1) {
		warnings(paste("Received just one line of data: ", .data, "\n"))
		return(.data[[1]])
	}
	
	.df <- NULL
	for (.r in seq(2,.rows)) {
		if(verbose) cat("[parseCSV] Processing row in CSV:", .data[[.r]], "\n")
		
		# initialize first column of the data frame so it can be bound in loop
		.row.df <- as.data.frame(.data[[.r]][1])
		names(.row.df) <- .names[[1]]
		
		for (i in seq(2,length(.names))) {
			.df <- as.data.frame(.data[[.r]][i])
			names(.df) <- .names[[i]]
			.row.df <- cbind(.row.df, .df)
		}
#		print(paste("row", .r))
#		print(.row.df)
		
		if(is.null(.df))
			.df <- .row.df
		else
			.df <- do.call(rbind, list(.df, .row.df))
	}
	
	if(verbose) cat("[parseCSV] Done.\n")
	
	return(.df)
}


################################################################################
# KML
#
parseKML <- function(obj, sos, verbose = FALSE) {
	if(verbose) cat("[parseKML] Processing KML...\n")
	
	return(obj)
}

