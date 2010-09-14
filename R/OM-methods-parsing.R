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
	
	# TODO optionals elements for OmMeasurement
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
	
	# TODO add values with names and encoding information as attributes to a data record
	
	return(.values)
}


#
# values is XML and encoding holds a SweTextBlock with the required separators.
#
parseValues <- function(values, encoding, verbose = FALSE) {
	if(verbose) cat("Parsing swe:values using", toString(encoding))
	
	# TODO continue here
	# use some clever string handling... ?strsplit
	
	.values <- NULL
	
	return(.values)
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

parseTextBlock <- function(obj) {
	.id <- xmlGetAttr(node = obj, name = "id", default = NA_character_)
	.tS <- xmlGetAttr(node = obj, name = "tokenSeparator")
	.bS <- xmlGetAttr(node = obj, name = "blockSeparator")
	.dS <- xmlGetAttr(node = obj, name = "decimalSeparator")
	
	.tb <- SweTextBlock(tokenSeparator = .tS, blockSeparator = .bS,
			decimalSeparator = .dS, id = .id)
	return(.tb)
}

parsePhenomenonProperty <- function(obj) {
	.obsProp <- NULL
	
	# check if reference or inline phenomenon
	.href <- xmlGetAttr(node = obj, name = "href")
	if(!is.null(.href)) {
		.obsProp <- SwePhenomenonProperty(href = .href)
	}
	else {
		.noneText <- .filterXmlChildren(node = obj, xmlTextNodeName,
				includeNamed = FALSE)
		.compPhen <- .noneText[[1]]
		# 52N SOS only returns swe:CompositePhenomenon
		.name <- xmlName(.compPhen)
		
		if(.name == sweCompositePhenomenonName) {
			.phen <- parseCompositePhenomenon(.compPhen)
			.obsProp <- SwePhenomenonProperty(phenomenon = .phen)
		}
		else {
			warning(paste("Unsupprted observed property: ", .name))
		}
	}
	
	return(.obsProp)
}

parseCompositePhenomenon <- function(obj) {
	.id <- xmlGetAttr(node = obj, name = "id", default = NA_character_)
	.dimension <- as.integer(
			xmlGetAttr(node = obj, name = "dimension", default = NA_character_))
	.name <- xmlValue(obj[[gmlNameName]])
	
	.components <- lapply(obj[sweComponentName], parseComponent)
	
	# optional:
	.description <- NA_character_
	if(!is.null(obj[[gmlDescriptionName]])) {
		.description <- parsePhenomenonProperty(obj[[sweBaseName]])
	}
	.base <- NULL
	if(!is.null(obj[[sweBaseName]])) {
		.base <- parsePhenomenonProperty(obj[[sweBaseName]])
	}
	
	.compPhen <- SweCompositePhenomenon(id = .id, name = .name, 
			description = .description, dimension = .dimension,
			components = .components, base = .base)
	
	return(.compPhen)
}

parseComponent <- function(obj) {
	# 52N SOS only sets the href property on swe components, but still reuse function
	.component <- parsePhenomenonProperty(obj)
	return(.component)
}

parseMeasure <- function(obj) {
	.value <- as.numeric(xmlValue(obj))
	.uom <- xmlGetAttr(node = obj, name = "uom", default = NA_character_)
	
	.result <- OmMeasure(.value, .uom)
	
	return(.result)
}

parseFOI <- function(obj) {
	.foi <- NULL
	
	# has href attribute?
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
#
#
parseSamplingPoint <- function(obj) {
	.sampledFeatures <- list(obj[saSampledFeatureName])
	.position <- parsePosition(obj[[saPositionName]])
	
	.sp <- SaSamplingPoint(sampledFeatures = .sampledFeatures,
			position = .position)
	return(.sp)
}

#
#
#
parsePosition <- function(obj) {
	.position <- NULL
	
	# has href attribute?
	.href <- xmlGetAttr(node = obj, name = "href")
	if(!is.null(.href)) {
		# position is referenced
		.position <- GmlPointProperty(href = .href)
	}
	else {
		# must be point
		.position <- GmlPointProperty(point = parsePoint(obj[[gmlPointName]]))
	}
	
	return(.position)
}

#
#
#
parsePoint <- function(obj) {
	.point <- NA
	.pos <- obj[[gmlPosName]]
	
	.posString <- xmlValue(.pos)
	
	# optional attributes:
	.srsName <- xmlGetAttr(node = .pos, name = "srsName",
			default = NA_character_)
	.srsDimension <- xmlGetAttr(node = .pos, name = "srsDimension",
			default = NA_integer_)
	.axisLabels <- xmlGetAttr(node = .pos, name = "axisLabels",
			default = NA_character_)
	.uomLabels <- xmlGetAttr(node = .pos, name = "uomLabels",
			default = NA_character_)
	
	.point <- GmlDirectPosition(pos = .posString, srsName = .srsName,
			srsDimension = .srsDimension, axisLabels = .axisLabels,
			uomLabels = .uomLabels)
	
	return(.point)
}

#
# create according GmlTimeObject from om:samplingTime
#
parseSamplingTime <- function(obj, format) {
	.timeObject = NULL
	
	.ti <- xmlChildren(obj)[[gmlTimeInstantName]]
	.tp <- xmlChildren(obj)[[gmlTimePeriodName]]
	if(!is.null(.ti)) {
		.timeObject <- parseTimeInstant(.ti)
	}
	else if(!is.null(.tp)) {
		# optionals
		.id = xmlGetAttr(node = .tp, name = "id",
				default = NA_character_)
		.frame = xmlGetAttr(node = .tp, name = "frame",
				default = as.character(NA))
		.noneTexts <- .filterXmlChildren(node = .tp, gmlRelatedTimeName)
		if(!is.null(.noneTexts))
			.relatedTimes <- .noneTexts
		else
			.relatedTimes = list()
		
		# TODO parse gml:timeLength
		.duration <- NA_character_
		.timeInterval <- NA
	
		# begin and end
		if(!is.null(.tp[[gmlBeginName]]) || !is.null(.tp[[gmlEndName]])) {
			.begin <- parseTimeInstantProperty(.tp[[gmlBeginName]])
			.end <- parseTimeInstantProperty(.tp[[gmlEndName]])
		
			.timeObject <- GmlTimePeriod(begin = .begin, end = .end, duration = .duration,
					timeInterval = .timeInterval, id = .id,
					relatedTimes = .relatedTimes, frame = .frame)
		}
		# beginPosition and endPosition
		else if(!is.null(.tp[[gmlBeginPositionName]])
				|| !is.null(.tp[[gmlEndPositionName]])) {
			.beginPosition <- parseTimePosition(
					obj = .tp[[gmlBeginPositionName]],
					format = format)
			.endPosition <- parseTimePosition(
					obj = .tp[[gmlEndPositionName]],
					format = format)
			
			.timeObject <- GmlTimePeriod(beginPosition = .beginPosition,
					endPosition = .endPosition, duration = .duration,
					timeInterval = .timeInterval, id = .id,
					relatedTimes = .relatedTimes, frame = .frame)
		}
	}
	
	return(.timeObject)
}

parseTimeInstant <- function(obj) {
	.timePos <- parseTimePosition(.ti, format)
	
	#optionals
	.id = xmlGetAttr(node = obj, name = "id",
			default = NA_character_)
	.frame = xmlGetAttr(node = obj, name = "frame",
			default = as.character(NA))
	.noneTexts <- .filterXmlChildren(node = obj, gmlRelatedTimeName)
	if(!is.null(.noneTexts))
		.relatedTimes <- .noneTexts
	else
		.relatedTimes = list()
	
	.ti <- GmlTimeInstant(timePosition = .timePos, id = .id,
			relatedTimes = .relatedTimes, frame = .frame)
	return(.ti)
}

parseTimeInstantProperty <- function(obj) {
	.timeProp <- NULL
	
	# check if reference or inline phenomenon
	.href <- xmlGetAttr(node = obj, name = "href")
	if(!is.null(.href)) {
		.timeProp <- GmlTimeInstantProperty(href = .href)
	}
	else {
		.noneText <- .filterXmlChildren(node = obj, xmlTextNodeName,
				includeNamed = FALSE)
		.time <- parseTimeInstant(.noneText[[1]])
		.timeProp <- GmlTimeInstantProperty(time = .time)
	}
	
	return(.timeProp)
}

#
# 
#
parseTimePosition <- function(obj, format) {
	.attrs <- xmlAttrs(obj)
	
	.time <- strptime(xmlValue(obj), format)
	
	# optional:
	.frame = as.character(NA)
	.calendarEraName = as.character(NA)
	.indeterminatePosition = as.character(NA)
	
	if(!is.null(.attrs)) {
		if(!is.na(.attrs["frame"]))
			.frame <- .attrs[["frame"]]
		if(!is.na(.attrs["calendarEraName"]))
			.calendarEraName <- .attrs[["calendarEraName"]]
		if(!is.na(.attrs["indeterminatePosition"]))
			.indeterminatePosition <- attrs[["indeterminatePosition"]]
	}
	
	.timePosition <- GmlTimePosition(time = .time, frame = .frame,
			calendarEraName = .calendarEraName,
			indeterminatePosition = .indeterminatePosition)
}


