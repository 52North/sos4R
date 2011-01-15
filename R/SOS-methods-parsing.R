################################################################################
# Copyright (C) 2010 by 52 North                                               #
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
	.id <- xmlGetAttr(obj, "id")
	.name <- xmlValue(obj[[gmlNameName]])
	
	.time <- parseTimeGeometricPrimitiveFromParent(obj = obj[[sosTimeName]],
			format = sosTimeFormat(sos))
	
	# can be references or containes, so use lists
	.observedProperty <- lapply(obj[sosObservedPropertyName], xmlGetAttr,
			"href")
	.featureOfInterest <- lapply(obj[sosFeatureOfInterestName], xmlGetAttr,
			"href")
	
	# can be transformed to character vectors
	.procedure <- sapply(obj[sosProcedureName], xmlGetAttr, "href")
	.responseFormat <- sapply(obj[sosResponseFormatName], xmlValue)
	
	# not optional, but potentially missing in some instances...
	if(!length(obj[sosResponseModeName]) < 1) {
		.responseMode <- sapply(obj[sosResponseModeName], xmlValue)
	}
	else {
		.responseMode <- NA_character_
		warning(paste("'responseMode' missing in offering", .id))
	}
	
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
	}
	else {
		.boundedBy <- list()
	}
	
	# warn if time or envelope is missing -> probably sensor without data.
	.warningText <- ""
	if(length(.boundedBy) < 1) {
		.warningText <- "\t'gml:boundedBy' is NA/empty.\n"
	}
	if(extends(class(.time), "GmlTimeInstant") && is.na(.time@timePosition@time)) {
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
	
	return(.ob)
}

#
#
#
parseSosCapabilities <- function(obj, sos) {
	.caps.root <- xmlRoot(obj)
	
	# attributes:
	.caps.version <- xmlGetAttr(node = .caps.root, name = "version",
			default = NA_character_)
	.caps.updateSequence <- xmlGetAttr(node = .caps.root,
			name = "updateSequence", default = NA_character_)
	
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
		.caps.fc <- parseSosFilter_Capabilities(
				.caps.root[[sosFilterCapabilitiesName]])
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

parseSosFilter_Capabilities <- function(obj) {
	.spatial.geom <- .filterXmlOnlyNoneTexts(
			node = obj[[ogcSpatialCapabilitiesName]][[ogcGeometryOperandsName]])
	.spatial.spat <- .filterXmlOnlyNoneTexts(
			node = obj[[ogcSpatialCapabilitiesName]][[ogcSpatialOperatorsName]])
	.spatial <- list(lapply(.spatial.geom, xmlValue),
			lapply(.spatial.spat, xmlGetAttr, name = "name"))
	names(.spatial) <- c(ogcGeometryOperandsName, ogcSpatialOperatorsName)
	
	.temporal.ands <- .filterXmlOnlyNoneTexts(
			node = obj[[ogcTemporalCapabilitiesName]][[ogcTemporalOperandsName]])
	.temporal.ators <- .filterXmlOnlyNoneTexts(
			node = obj[[ogcTemporalCapabilitiesName]][[ogcTemporalOperatorsName]])
	.temporal <- list(lapply(.temporal.ands, xmlValue),
			lapply(.temporal.ators, xmlGetAttr, name = "name"))
	names(.temporal) <- c(ogcTemporalOperandsName, ogcTemporalOperatorsName)
	
	.scalarXML <- obj[[ogcScalarCapabilitiesName]]
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
	
	.idXML <- .filterXmlOnlyNoneTexts(obj[[ogcIdCapabilities]])
	.id <- lapply(.idXML, xmlName)
	
	.fc <- SosFilter_Capabilities(spatial = .spatial, temporal = .temporal,
			scalar = .scalar, id = .id)
}

################################################################################
# parse saved documents
setMethod(f = "parseFile",
		signature = signature(sos = "SOS_1.0.0", file = "character"),
		def = function(sos, file, verbose, ...) {
			# TODO add parameter and handling for strings using xmlParseString(...)
			.parsed <- xmlParse(file, ...)
			.parsingFunction <- sosParsers(sos)[[sosGetObservationName]]
			
			.doc <- .parsingFunction(obj = .parsed, sos = sos,
					verbose = verbose)
			
			if(verbose) {
				cat("** PARSED DOCUMENT FROM FILE:\n")
				print(.doc)
			}
			return(.doc)
		}
)

################################################################################
#
#
#
#
parseCSV <- function(obj, verbose = FALSE) {
	if(verbose) cat("Processing CSV...\n")
	
	lines <- strsplit(x = obj, split = "\n")[[1]]
	data <- do.call(what = "strsplit", args = list(lines, split = ","))
	
	# clean up names (double quotes)
	.names <- data[[1]]
	.newNames <- c()
	for (.n in .names) {
		.newNames <- c(.newNames,
				gsub(pattern = "\"", replacement = "", x = .n))
	}
	.names <- .newNames
	
	.rows <- length(data)
	if(verbose) cat("Got", .rows, "lines of data.\n")
	
	if(.rows == 1) {
		warnings(paste("Received just one line of data: ", data, "\n"))
		return(data[[1]])
	}
	
	df <- NULL
	for (.r in seq(2,.rows)) {
		if(verbose) cat("Processing row in CSV:", data[[.r]], "\n")
		
		# initialize first column of the data frame so it can be bound in loop
		.row.df <- as.data.frame(data[[.r]][1])
		names(.row.df) <- .names[[1]]
		
		for (i in seq(2,length(.names))) {
			.df <- as.data.frame(data[[.r]][i])
			names(.df) <- .names[[i]]
			.row.df <- cbind(.row.df, .df)
		}
#		print(paste("row", .r))
#		print(.row.df)
		
		if(is.null(df))
			df <- .row.df
		else
			df <- do.call(rbind, list(df, .row.df))
	}
	
	return(df)
}

