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

parseOM <- function(obj, parsers, verbose = FALSE) {
	.om <- NULL
	
	# check if this is the outermost call and a document is given, not a node
	if(inherits(obj, xmlInternalDocumentName))
		.root <- xmlRoot(obj)
	else .root <- obj
	
	# switch submethods based on name
	.rootName <- xmlName(.root)
	
	.parsingFunction <- parsers[[.rootName]]
	if(!is.null(.parsingFunction)) {
		if(verbose) cat("Parsing O&M", .rootName, "\n") #, "with: "); print(.parsingFunction)
		.om <- .parsingFunction(.root, parsers, verbose)	
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
parseMember <- function(obj, parsers, verbose = FALSE) {
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
	
	.mResult <- parseOM(.child, parsers, verbose)
	
	return(.mResult)
}

#
#
#
parseMeasurement <- function(obj, parsers, verbose = FALSE, 
		timeFormat = sosDefaultTimeParsingFormat) {
	
	.samplingTime <- parseSamplingTime(obj[[omSamplingTimeName]], timeFormat)
	
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
parseObservation <- function(obj, parsers, verbose = FALSE,
		timeFormat = sosDefaultTimeParsingFormat) {
	
	.samplingTime <- parseSamplingTime(obj[[omSamplingTimeName]], timeFormat)
	
	# 52N SOS only returns om:Observation with procedure ids xlink:href
	.procedure <- xmlGetAttr(node = obj[[omProcedureName]], name = "href")
	
	.observedProperty <- parsePhenomenonProperty(obj[[omObservedPropertyName]])
	
	.featureOfInterest <- parseFOI(obj[[omFeatureOfInterestName]])
	
	# result parser is exchangeable
	.resultParsingFunction <- parsers[[omResultName]]
	.result <- .resultParsingFunction(obj[[omResultName]], parsers, verbose)
	
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
parseObservationCollection <- function(obj, parsers, verbose) {
	# remove nodes other than member
	.members <- .filterXmlChildren(obj, omMemberName, includeNamed = TRUE)
	
	if(verbose) cat("Parsing", length(.members),
				"element(s) in ObservationCollection:", names(.members), "\n")
	
	.resultList <- lapply(.members, parseOM, parsers, verbose)
	names(.resultList) <- lapply(.resultList, class)
	
	if(verbose)
		cat("Parsed ObservationCollection with", length(.resultList),
				"elements:", names(.resultList), "\n")
	
	return(.resultList)
}

#
# not yet supported, TODO: implement parsing of further observation specializations (constraints)
#
parseGeometryObservation <- function(obj, parsers, verbose = FALSE,
		timeFormat = sosDefaultTimeParsingFormat) {
	warning("Parsing of om:GeometryObservation is not implemented!")
	return(NA)
}

parseCategoryObservation <- function(obj, parsers, verbose = FALSE,
		timeFormat = sosDefaultTimeParsingFormat) {
	warning("Parsing of om:CategoryObservation is not implemented!")
	return(NA)
}

parseCountObservation <- function(obj, parsers, verbose = FALSE,
		timeFormat = sosDefaultTimeParsingFormat) {
	warning("Parsing of om:CountObservation is not implemented!")
	return(NA)
}

parseTruthObservation <- function(obj, parsers, verbose = FALSE,
		timeFormat = sosDefaultTimeParsingFormat) {
	warning("Parsing of om:TruthObservation is not implemented!")
	return(NA)
}

parseTemporalObservation <- function(obj, parsers, verbose = FALSE,
		timeFormat = sosDefaultTimeParsingFormat) {
	warning("Parsing of om:TemporalObservatio is not implemented!")
	return(NA)
}

parseComplexObservation <- function(obj, parsers, verbose = FALSE,
		timeFormat = sosDefaultTimeParsingFormat) {
	warning("Parsing of om:ComplexObservation is not implemented!")
	return(NA)
}

parseResult <- function(obj, parsers, verbose = FALSE) {
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
		.dataArrayParsingFunction <- parsers[[sweDataArrayName]]
		.dataArray <- .noneText[[1]]
		.result <- .dataArrayParsingFunction(.dataArray, parsers, verbose)
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
parseDataArray <- function(obj, parsers, verbose = FALSE) {
	.elementCount <-  xmlValue(obj[["elementCount"]][["Count"]][["value"]])
	if(verbose) cat("Parsing DataArray with", .elementCount, "elements.\n")
	
	.eTParser <- parsers[[sweElementTypeName]]
	.fields <- .eTParser(obj[[sweElementTypeName]])
	
	.encParser <- parsers[[sweEncodingName]]
	.encoding <- .encParser(obj[[sweEncodingName]])
	
	.valParser <- parsers[[sweValuesName]]
	.values <- .valParser(obj[[sweValuesName]], .encoding, verbose)
	
	if(!is.data.frame(.values)) {
		stop("value parser needs to return a data frame for this data array parser to work!")
	}
	
	
	# struktur des data frames passt noch nicht ganz!
#	> str(tempValues)
#	'data.frame':	8 obs. of  1 variable:
#			$ V1:List of 8
#	..$ : chr  "2010-03-01T12:15:00.000+01:00" "urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93" "73.0"
#	..$ : chr  "2010-03-01T12:30:00.000+01:00" "urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93" "68.0"
#	..$ : chr  "2010-03-01T12:45:00.000+01:00" "urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93" "69.0"
#	..$ : chr  "2010-03-01T13:00:00.000+01:00" "urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93" "65.0"
#	..$ : chr  "2010-03-01T13:15:00.000+01:00" "urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93" "61.0"
#	..$ : chr  "2010-03-01T13:30:00.000+01:00" "urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93" "56.0"
#	..$ : chr  "2010-03-01T13:45:00.000+01:00" "urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93" "60.0"
#	..$ : chr  "2010-03-01T14:00:00.000+01:00" "urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93" "57.0"

	# TODO continue here!!
	
	# add values with names and encoding information as attributes to a data record
	
	# if one field contains defnition with "urn:ogc:data:time:iso8601", then convert the whole column to time objects
	
	# if one filed contains a uom, convert the whole column to numeric
	
	# set these conversion rules as default values
	
	return(.values)
}


#
# values is XML and encoding holds a SweTextBlock with the required separators.
#
parseValues <- function(values, encoding, verbose = FALSE) {
	if(verbose) cat("Parsing swe:values using", toString(encoding), "\n")
	
	if(!inherits(encoding, "SweTextBlock")) {
		stop("Handling for given encoding not implemented!")
	}
	else {
		.blockLines <- strsplit(x = xmlValue(values),
				split = encoding@blockSeparator)
		.tokenLines <- sapply(.blockLines, strsplit,
				split = encoding@tokenSeparator)
		.values <- as.data.frame(.tokenLines)
		return(.values)
	}
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
parseSamplingTime <- function(obj, format) {
	.timeObject <- parseAbstractTimeGeometricPrimitive(obj, format)
	return(.timeObject)
}

