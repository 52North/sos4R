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
# Author: Daniel Nüst (daniel.nuest@uni-muenster.de)                           #
# Created: 2010-06-18                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

parseOM <- function(obj, sos, verbose = FALSE) {
	.om <- NULL
	
	# check if this is the outermost call and a document is given, not a node
	if(inherits(obj, xmlInternalDocumentName))
		.root <- xmlRoot(obj)
	else .root <- obj
	
	# switch submethods based on name
	.rootName <- xmlName(.root)
	
	.parsingFunction <- sosParsers(sos)[[.rootName]]
	if(!is.null(.parsingFunction)) {
		if(verbose) cat("Parsing O&M", .rootName, "\n") #, "with: "); print(.parsingFunction)
		.om <- .parsingFunction(.root, sos, verbose)	
		if(verbose) cat("Done: ", substr(toString(.om), 0, 74), "...\n")
	}
	else {
		warning(paste("No parsing function for given element ", .rootName))
	}
	
	return(.om)
}

#
# extracts Obervation or Measurement from member
#
parseMember <- function(obj, sos, verbose = FALSE) {
	# a member can only have on child element, parse that, omit possible text nodes artefacts
	if(xmlSize(obj) > 1) {
		.noneTexts <- .filterXmlChildren(obj, xmlTextNodeName, includeNamed = FALSE)
		.child <- .noneTexts[[1]]
	}
	else {
		.child <- xmlChildren(obj)[[1]]
	}
	
	if(verbose) {
		cat("Parsing child of member:", xmlName(.child), "\n")
	}
	
	.mResult <- parseOM(.child, sos, verbose)
	
	return(.mResult)
}

#
#
#
parseMeasurement <- function(obj, sos, verbose = FALSE) {
	
	.samplingTime <- parseSamplingTime(obj = obj[[omSamplingTimeName]],
			timeFormat = sosTimeFormat(sos))
	
	# 52N SOS only returns om:Measurements (!) with procedure ids and observed 
	# properties in xlink:href
	.procedure <- xmlGetAttr(node = obj[[omProcedureName]], name = "href")
	.observedProperty <- SwePhenomenonProperty(
			href = xmlGetAttr(node = obj[[omObservedPropertyName]], name = "href"))
	
	.featureOfInterest <- parseFOI(obj[[omFeatureOfInterestName]])
	
	# must be OmMeasure
	.result <- parseMeasure(obj[[omResultName]])
	
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
#
#
parseObservation <- function(obj, sos, verbose = FALSE) {
	
	.samplingTime <- parseSamplingTime(obj = obj[[omSamplingTimeName]],
			timeFormat = sosTimeFormat(sos = sos))
	
	# 52N SOS only returns om:Observation with procedure ids xlink:href
	.procedure <- xmlGetAttr(node = obj[[omProcedureName]], name = "href")
	
	.observedProperty <- parsePhenomenonProperty(obj[[omObservedPropertyName]])
	
	.featureOfInterest <- parseFOI(obj[[omFeatureOfInterestName]])
	
	# result parser is exchangeable
	.resultParsingFunction <- sosParsers(sos)[[omResultName]]
	.result <- .resultParsingFunction(obj[[omResultName]], sos, verbose)
	
	# TODO optionals elements for OmObservation
	#.metadata
	#.resultTime
	#.resultQuality
	#.parameter
	#.metadata
	
	.obs <- OmObservation(samplingTime = .samplingTime,
			procedure = .procedure, observedProperty = .observedProperty,
			featureOfInterest = .featureOfInterest, result = .result)
	
	return(.obs)
}

#
#
#
parseObservationCollection <- function(obj, sos, verbose) {
	# remove nodes other than member
	.members <- .filterXmlChildren(obj, omMemberName, includeNamed = TRUE)
	
	if(verbose) cat("Parsing", length(.members),
				"element(s) in ObservationCollection:", names(.members), "\n")
	
	.resultList <- lapply(.members, parseOM, sos, verbose)
	names(.resultList) <- lapply(.resultList, class)
	
	if(verbose)
		cat("Parsed ObservationCollection with", length(.resultList),
				"elements:", names(.resultList), "\n")
	
	return(.resultList)
}

#
# not yet supported, TODO: implement parsing of further observation specializations (constraints)
#
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

parseResult <- function(obj, sos, verbose = FALSE) {
	.result <- NULL
	
	.noneText <- .filterXmlChildren(node = obj, xmlTextNodeName,
			includeNamed = FALSE)
	# 52N SOS currently only returns swe:DataArrayDocument, but still check
	if(xmlName(.noneText[[1]]) != sweDataArrayName) {
		warning(paste("Parsing of given result is NOT supported:",
						xmlName(.noneText[[1]]), "-- only", sweDataArrayName,
						"can be parsed."))
	}
	else {
		# data array parser is exchangeable
		.dataArrayParsingFunction <- sosParsers(sos)[[sweDataArrayName]]
		.dataArray <- .noneText[[1]]
		.result <- .dataArrayParsingFunction(.dataArray, sos, verbose)
	}

	return(.result)
}

#
# optimized for 52N SOS, that means only options used there in OMEncoder are
# handled here.
#
# For example, swe:elementCount can also have an attribut ref, but this is not
# checked here, and swe:Count is actually a swe:AbstractDataComponentType, but
# here it is just looked for a child element swe:value.
#
parseDataArray <- function(obj, sos, verbose = FALSE) {
	.elementCount <-  xmlValue(obj[["elementCount"]][["Count"]][["value"]])
	if(verbose) cat("Parsing DataArray with", .elementCount, "elements.\n")
	
	.eTParser <- sosParsers(sos)[[sweElementTypeName]]
	.fields <- .eTParser(obj[[sweElementTypeName]])
	
	.encParser <- sosParsers(sos)[[sweEncodingName]]
	.encoding <- .encParser(obj[[sweEncodingName]])
	
	.valParser <- sosParsers(sos)[[sweValuesName]]
	.values <- .valParser(values = obj[[sweValuesName]], fields = .fields,
			encoding = .encoding, sos = sos, verbose = verbose)
	
	return(.values)
}


#
# values is XML and encoding holds a SweTextBlock with the required separators.
#
parseValues <- function(values, fields, encoding, sos, verbose = FALSE) {
	if(verbose) cat("Parsing swe:values using", toString(encoding), "and",
				length(fields), "fields:", toString(names(fields)), "\n")
	if(!inherits(encoding, "SweTextBlock")) {
		stop("Handling for given encoding not implemented!")
	}
	
	.converters <- sosFieldConverters(sos)
	
	.blockLines <- strsplit(x = xmlValue(values),
			split = encoding@blockSeparator)
	.tokenLines <- sapply(.blockLines, strsplit,
			split = encoding@tokenSeparator)

	# data frame of correct length to be able to use cbind for first column
	.tempId = "tempID"
	.data <- data.frame(seq(1,length(.tokenLines)))
	names(.data) <- .tempId
	
	# do following for all fields
	.fieldCount <- length(fields)
	for (.currentFieldIdx in seq(1,.fieldCount)) {
		# create list for each variable
		.currentValues <- sapply(.tokenLines, "[[", .currentFieldIdx)
		.currentField <- fields[[.currentFieldIdx]]			
		
		# convert values to the correct types
		.method <- .converters[[.currentField[["definition"]]]]
		if(is.null(.method)) {
			# could still be a unit of measurement given, use as
			if(!is.na(.currentField["uom"])) {
				.method <- .converters[[.currentField[["uom"]]]]
				if(is.null(.method)) {
					# fallback option
					warning(paste("No converter for the unit of measurement ",
								.currentField[["uom"]],
								"! You can add one SOSFieldConverters()."))
					.method <- .converters[["uom"]]	
				}
			}
			else {
				warning(paste("No converter found for the given field",
								toString(.currentField)))		
			}
		}

		if(verbose) {
			cat("Using converter function: ")
			show(.method)
		}
		
		# do the conversion
		.currentValues <- .method(x = .currentValues, sos = sos)
		
		# bind new and existing data
		if(verbose) cat("Binding additional data.frame for",
					.currentField[["name"]], "with values",
					toString(.currentValues), "\n")
		.newData <- data.frame(.currentValues)
		names(.newData) <- .currentField[["name"]]
		.data <- cbind(.data, .newData)
	}
	
	# remove id column
	.data <- .data[,!colnames(.data)%in%.tempId]
	
	return(.data)
}

parseElementType <- function(obj) {
	# can only process swe:elementType containing a swe:SimpleDataRecord
	.simpleDR <- obj[[sweSimpleDataRecordName]]
	if(is.null(.simpleDR)) {
		stop(paste("Cannot parse swe:elementType, only children of type",
						sweSimpleDataRecordName, "is supported!"))
	}
	else {
		.fields <- .filterXmlChildren(node = .simpleDR, childrenName = sweFieldName,
			includeNamed = TRUE)
		
		# extract the fields, naming with attribute 'name'
		.parsedFields <- lapply(.fields, parseField)
		.names <- sapply(.parsedFields, "[", "name")
		names(.parsedFields) <- .names
		
		return(.parsedFields)
	}
}

parseEncoding <- function(obj) {
	.textBlock <- obj[[sweTextBlockName]]
	
	if(is.null(.textBlock)) {
		stop(paste("Cannot parse swe:encoding, only", sweTextBlockName,
						"is supported!"))
	}
	else {
		.tb <- parseTextBlock(.textBlock)
		return(.tb)
	}
}

################################################################################
# sub-parsing functions, not exchangeable via SosParsers

parseField <- function(obj) {
	.field <- NULL
	
	.name <- xmlGetAttr(node = obj, name = "name")
	
	.noneText <- .filterXmlChildren(node = obj, childrenName = xmlTextNodeName,
			includeNamed = FALSE)
	.innerField <- .noneText[[1]]
	.innerFieldName <- xmlName(.innerField)

	#cat("parsing ", .innerFieldName, "\n")
	
	# available options: Time, Text, Quantity
	if(.innerFieldName == sweTimeName) {
		.def <- xmlGetAttr(node = .innerField, name = "definition")
		.field <- c(name = .name, definition = .def)
	}
	else if (.innerFieldName == sweTextName) {
		.def <- xmlGetAttr(node = .innerField, name = "definition")
		.field <- c(name = .name, definition = .def)
	}
	else if (.innerFieldName == sweQuantityName) {
		.def <- xmlGetAttr(node = .innerField, name = "definition")
		.uom <- xmlGetAttr(node = .innerField[[sweUomName]], name = "code")
		.field <- c(name = .name, definition = .def, uom = .uom)
	}
	
	# TODO implement other options: DataRecord with Position, Category, Count, Boolean
	
	return(.field)
}

parseMeasure <- function(obj) {
	.value <- as.numeric(xmlValue(obj))
	.uom <- xmlGetAttr(node = obj, name = "uom", default = NA_character_)
	
	.result <- OmMeasure(.value, .uom)
	
	return(.result)
}

parseFOI <- function(obj) {
	.foi <- NULL
	
	# has href attribute? if yes, use it!
	.href <- xmlGetAttr(node = obj, name = "href")
	if(!is.null(.href)) {
		# feature is referenced
		.foi <- GmlFeatureProperty(href = .href)
	}
	else {
		# feature is available in the element
		.noneTexts <- .filterXmlChildren(obj, xmlTextNodeName,
				includeNamed = FALSE)
		.feature <- .noneTexts[[1]]
		.name <- xmlName(.feature)
		
		if(.name == saSamplingPointName) {
			.sp <- parseSamplingPoint(.feature)
			.foi <- GmlFeatureProperty(feature = .sp)
		}
		else if (.name == saSamplingSurface) {
			# TODO implement parsing of sampling surface
			.foi <- GmlFeatureProperty(feature = .name)
		}
		else if (.name == gmlFeatureCollectionName) {
			# TODO implement parsing of feature collection
			.foi <- GmlFeatureProperty(feature = .name)
		}
		else {
			warning("No parsing for given feature implemented")
			.foi <- GmlFeatureProperty(feature = .feature)
		}
	}
	
	return(.foi)
}

#
# create according GmlTimeObject from om:samplingTime
#
parseSamplingTime <- function(obj, timeFormat) {
	.timeObject <- parseAbstractTimeGeometricPrimitive(obj = obj,
			format = timeFormat)
	return(.timeObject)
}

