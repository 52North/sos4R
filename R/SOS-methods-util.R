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
# Created: 2010-09-21                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

################################################################################
# TODO read data function
# 
# convenience function using non-SWE names for stuff
#
# parameters:
#		sos:
#				the sos to query
#		
# 		time = c():	
#				either one time as POSIXt (or character which is tried to be
#				parsed to POSIXt) and forms a time instant, or two times
#				(character or POSIXt) that form an interval
#		mergeResult = FALSE:
#				boolean flag that turns on merging of the result into one
#				dataframe, the single observations cannot be accesses,
#				essentially only gives all [measurement,observation]@result
#		addLocation = FALSE
#				trigger if the location from the  should be added to the result
#				table fom the featureOfInterest
#
# * Function does not include "offering", which is/are instead automatically
#	detected... but what if one sensor is in several offerings?
#
# * Function returns data.frame, NOT OmObservation or OmMeasurement classes
#
# * if querying several procedures with different positions, put in them into 
#	one data.frame, or a list?
#
read.sos <- function(sos,
		sensors = NA_character_,
		phenomena = NA_character_,
		bbox = NA_character_, # one, or several?
		times = c(NA), # one, or several?
		mergeResult = FALSE,
		addLocation = FALSE,
		verbose = FALSE) {
	warning("Method is not implemented yet!")
}


#
# convenience info function using non-SWE names for stuff
#
info.sos <- function(sos,
		# select the info to return, if > 1 as named list
		phenomena = TRUE,
		bbox = TRUE,
		timePeriod = TRUE,
		# 
		locations = FALSE,
		all = FALSE,
		sensors = FALSE,
		offerings = FALSE,
		features = FALSE,
		metadata = FALSE,
		operations = FALSE,
		UOMs = FALSE,
		# select "filters", only one at a time and
		sensor = NA_character_,
		phenomenon = NA_character_,
		offering = NA_character_,
		# data is an index of the list of the info.data operation
		series = NA_integer_) {
	#standardGeneric("info.sos")
	warning("Method is not implemented yet!")
}


#
# sensors = procedure
# phenomena = observedProperty
# metadata: url, method, version, identification, provider, timeFormat
# offerings: shortly the name, the bounding box, the time period - DEFAULT!
# operations: just the names and how to get more info

#
# method creates a list (whose items can are named and can be used to read data,
# or just used in function read.sos)
# for all available and valid combinations of off/foi/phen/proc
#
info.series <- function(sos) {
	warning("Method is not implemented yet!")
}

#
# method returns information about sensors (procedures) matching the given
# criteria
#
info.sensors <- function(sos,
		phenomena = c(NA_character_),
		offerings = c(NA_character_),
		UOMs = c(NA_character_)) {
	warning("Method is not implemented yet!")
}

#
#
#
info.phenomena <- function(sos) {
	warning("Method is not implemented yet!")
}

#
#
#
info.features <- function(sos) {
	warning("Method is not implemented yet!")
}

#
#
#
info.offerings <- function(sos) {
	warning("Method is not implemented yet!")
}


################################################################################
# conversion methods and accessor function
sosConvertTime <- function(x, sos) {
	.t <- as.POSIXct(strptime(x = x, format = sosTimeFormat(sos = sos)))
	return(.t)
}
sosConvertDouble <- function(x, sos) {
	return(as.double(x = x))
}
sosConvertString <- function(x, sos) {
	return(as.character(x = x))
}
sosConvertLogical <- function(x, sos) {
	return(as.logical(x = x))
}


################################################################################
# convenience functions
if (!isGeneric("sosCreateTimeInstant"))
	setGeneric(name = "sosCreateTimeInstant", def = function(sos, time,
					frame = as.character(NA),
					calendarEraName = as.character(NA),
					indeterminatePosition = as.character(NA)) {
				standardGeneric("sosCreateTimeInstant")
			}
	)
setMethod(f = "sosCreateTimeInstant",
		signature = signature(sos = "SOS", time = "POSIXt"),
		def = function(sos, time, frame, calendarEraName,
				indeterminatePosition) {
			.time <- format(time, sosTimeFormat(sos))
			.timePos <- GmlTimePosition(
					time = strptime(.time, sosTimeFormat(sos)),
					frame = frame, calendarEraName = calendarEraName,
					indeterminatePosition = indeterminatePosition)
			.ti <- GmlTimeInstant(timePosition = .timePos)
			return(.ti)
		}
)

if (!isGeneric("sosCreateTimePeriod"))
	setGeneric(name = "sosCreateTimePeriod",
			def = function(sos, begin, end, frame = as.character(NA),
					calendarEraName = as.character(NA),
					indeterminatePosition = as.character(NA),
					duration = as.character(NA),
					timeInterval = NULL) {
				standardGeneric("sosCreateTimePeriod")
			}
	)
setMethod(f = "sosCreateTimePeriod",
		signature = signature(sos = "SOS", begin = "POSIXt", end = "POSIXt"),
		def = function(sos, begin, end, frame, calendarEraName,
				indeterminatePosition, duration, timeInterval) {
			.tf <- sosTimeFormat(sos)
			.beginPos <- GmlTimePosition(
					time = strptime(format(begin, .tf), .tf),
					frame = frame, calendarEraName = calendarEraName,
					indeterminatePosition = indeterminatePosition
			)
			.endPos <- GmlTimePosition(
					time = strptime(format(end, .tf), .tf),
					frame = frame, calendarEraName = calendarEraName,
					indeterminatePosition = indeterminatePosition
			)
			.tp <- GmlTimePeriod(beginPosition = .beginPos,
					endPosition = .endPos, duration = duration,
					timeInterval = timeInterval)
			return(.tp)
		}
)

#
#
#
if (!isGeneric("sosCreateEventTimeList"))
	setGeneric(name = "sosCreateEventTimeList",
			def = function(time, operator = sosDefaultTemporalOperator) {
				standardGeneric("sosCreateEventTimeList")
			})
setMethod(f = "sosCreateEventTimeList",
		signature = signature(time = "GmlTimeGeometricPrimitive"),
		def = function(time, operator) {
			.et <- list(sosCreateEventTime(time = time, operator = operator))
			return(.et)
		}
)
if (!isGeneric("sosCreateEventTime"))
	setGeneric(name = "sosCreateEventTime",
			def = function(time, operator = sosDefaultTemporalOperator) {
				standardGeneric("sosCreateEventTime")
			})
setMethod(f = "sosCreateEventTime",
		signature = signature(time = "GmlTimeGeometricPrimitive"),
		def = function(time, operator) {
			
			if(operator == ogcTempOpTMAfterName) {
				.tOps <- TM_After(time = time)
			}
			else if(operator == ogcTempOpTMBeforeName) {
				.tOps <- TM_Before(time = time)
			}
			else if(operator == ogcTempOpTMDuringName) {
				.tOps <- TM_During(time = time)
			}
			else if(operator == ogcTempOpTMEqualsName) {
				.tOps <- TM_Equals(time = time)
			}
			else {
				stop(paste("Given operator", operator, "is not supported,",
								"choose one of",
								toString(SosSupportedTemporalOperators())))
			}
			
			.et <- SosEventTime(.tOps)
			return(.et)
		}
)

#
#
#
if (!isGeneric("sosCreateFeatureOfInterest"))
	setGeneric(name = "sosCreateFeatureOfInterest",
			def = function(objectIDs = list(NA), spatialOps = NULL, bbox = NULL,
					srsName = NA_character_) {
				standardGeneric("sosCreateFeatureOfInterest")
			})
setMethod(f = "sosCreateFeatureOfInterest",
		signature = signature(),
		def = function(objectIDs, spatialOps, bbox, srsName) {
			# switch cases, either objectIDs or one of the spatialOps shortcuts
			if(!any(is.na(objectIDs))) {
				.foi <- SosFeatureOfInterest(objectIDs = objectIDs)
			}
			else if (!is.null(spatialOps)) {
				.foi <- SosFeatureOfInterest(spatialOps = spatialOps)
			}
			else if(!is.null(bbox)) {
				if(is.matrix(bbox)) {
					.env <- GmlEnvelope(
							lowerCorner = GmlDirectPositionLatLon(lat = bbox[2,1],
									lon = bbox[1,1]),
							upperCorner = GmlDirectPositionLatLon(lat = bbox[2,2],
									lon = bbox[1,2]),
							srsName = srsName)
					.bbox <- OgcBBOX(envelope = .env)
					.foi <- SosFeatureOfInterest(spatialOps = .bbox)
				}
				else {
					stop("bbox must be matrix!")
				}
			}
			else {
				stop("At least one of objectIDs or spatialOps has to be set!")
			}
			
			return(.foi)
		}
)
		
#
#
#
if (!isGeneric("sosCreateBBOX"))
	setGeneric(name = "sosCreateBBOX",
			def = function(lowLat, lowLon, uppLat, uppLon, srsName,
					srsDimension = NA_integer_, axisLabels = NA_character_,
					uomLabels = NA_character_,
					propertyName = sosDefaultSpatialOpPropertyName) {
				standardGeneric("sosCreateBBOX")
			})
setMethod(f = "sosCreateBBOX",
		signature = signature(lowLat = "numeric", lowLon = "numeric",
				uppLat = "numeric", uppLon = "numeric"),
		def = function(lowLat, lowLon, uppLat, uppLon, srsName,
			srsDimension = NA_integer_, axisLabels = NA_character_,
			uomLabels = NA_character_,
			propertyName = sosDefaultSpatialOpPropertyName) {
		.env <- GmlEnvelope(
				lowerCorner = GmlDirectPosition(
						pos = paste(lowLat, lowLon, sep = " ")),
				upperCorner = GmlDirectPosition(
						pos = paste(uppLat, uppLon, sep = " ")),
				srsName = srsName, srsDimension = srsDimension,
				axisLabels = axisLabels, uomLabels = uomLabels)
		
		.bbox <- OgcBBOX(propertyName = propertyName, envelope = .env)
		return(.bbox)
		}
)

#
#
#
if (!isGeneric("sosCreateBBoxMatrix"))
	setGeneric(name = "sosCreateBBoxMatrix",
			def = function(lowLat, lowLon, uppLat, uppLon) {
				standardGeneric("sosCreateBBoxMatrix")
			})
setMethod(f = "sosCreateBBoxMatrix",
		signature = signature(lowLat = "numeric", lowLon = "numeric",
				uppLat = "numeric", uppLon = "numeric"),
		def = function(lowLat, lowLon, uppLat, uppLon) {
			.m <- matrix(data = c(lowLon, lowLat, uppLon, uppLat),
					nrow = 2, ncol = 2,
					dimnames = list(
							c("longitude", "latitude"),
							c("lowerCorner", "upperCorner")))
			return(.m)
		}
)

################################################################################
# MISC

#
#
#
if (!isGeneric("sosCapabilitiesDocumentOriginal"))
	setGeneric(name = "sosCapabilitiesDocumentOriginal", def = function(sos) {
				standardGeneric("sosCapabilitiesDocumentOriginal")
			})
setMethod(f = "sosCapabilitiesDocumentOriginal",
		signature = signature(sos = "SOS"),
		def = function(sos) {
			.gc <- OwsGetCapabilities(service = sosService,
					acceptVersions = c(sos@version))
			.responseString = sosRequest(sos = sos, request = .gc,
					verbose = sos@verboseOutput, inspect = FALSE)
			.response <- xmlParseDoc(.responseString, asText = TRUE)
			return(.response)
		}
)

#
# helper methods for exception response handling
#
.isExceptionReport <- function(document) {
	if(owsExceptionReportName == xmlName(xmlRoot(document)))
		return(TRUE)
	else
		return(FALSE)
}
.handleExceptionReport <- function(sos, obj) {
	if(sos@verboseOutput) warning("Received ExceptionReport!")
	
	.parsingFunction <- sosParsers(sos)[[owsExceptionReportName]]
	.er <- .parsingFunction(obj)
	if(class(.er) == "OwsExceptionReport")
		warning(toString(.er))
	return(.er)
}

#
#
#
.createLatestEventTime <- function(verbose = FALSE) {
	if(verbose) cat("Creating non-standard event time 'latest'\n")
	.et <- SosEventTimeLatest()
	return(.et)
}

#
# encoding functions that just pass given content along...
#
setMethod(f = "encodeXML", signature = signature(obj = "XMLNode", sos = "SOS"),
		def = function(obj, sos, verbose = FALSE) {
			if(verbose) {
				cat("ENCODE XML from XMLNode\n")
			}
			return(obj)
		}
)
setMethod(f = "encodeXML", signature = signature(obj = "XMLInternalElementNode", sos = "SOS"),
		def = function(obj, sos, verbose = FALSE) {
			if(verbose) {
				cat("ENCODE XML from XMLInternalElementNode\n")
			}
			return(obj)
		}
)
setMethod(f = "encodeXML", signature = signature(obj = "character", sos = "SOS"),
		def = function(obj, sos, addNamespaces = FALSE, verbose = FALSE) {
			if(verbose) cat("ENCODE XML from character string\n")
			
			if(isXMLString(obj)) {
				#FIXME this just won't work, see testing.R, section "encode xml character string (again)"
				if(addNamespaces) {
					if(verbose) cat("Namespace hack for character string, trying to replace 'result>'!\n")
					.hack <- 'result xmlns:sos="http://www.opengis.net/sos/1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ows="http://www.opengis.net/ows/1.1" xmlns:om="http://www.opengis.net/om/1.0" xmlns:ogc="http://www.opengis.net/ogc" xmlns:gml="http://www.opengis.net/gml">'
					.hackedString <- sub(pattern = "result>",
							replacement = .hack,
							x = obj)
					.xml <- xmlTreeParse(.hackedString, asText = TRUE,
							useInternalNodes = FALSE)
				}
				else {
					.xml <- xmlParseString(obj)
				}
								
				if(verbose) {
					cat("Created XML from string:\n", toString(.xml))
				}
				return(.xml)
			}
			else {
				warning(paste("Could not encode given character string as XML!",
								" Character string: '", obj, "'", sep = ""))
			}
		}
)

################################################################################
# Helper functions for OWS exceptions, e.g. to get the meaning of an exception
# code.
#
setMethod(f = "sosExceptionCodeMeaning",
		signature = c(exceptionCode = "character"),
		def = function(exceptionCode) {
			.meaning <- as.character(
					.owsStandardExceptions[
							.owsStandardExceptions$exceptionCode==exceptionCode,
							2])
			return(.meaning)
		}
)

################################################################################
# get coordiante refernce system CRS
#
#
setMethod(f = "sosGetCRS",
		signature = c(obj = "character"),
		def = function(obj) {
			# get the position of EPSG
			.split <- strsplit(as.character(obj), split = ":")
			.idx <- which(toupper(.split[[1]]) == "EPSG")
			if(length(.idx) == 0) {
				# possibly versioned, try one index higher
				
				
				warning(paste("Could not create CRS from the given object:", obj))
				return(NULL)
			}
			.epsg <- .split[[1]][[length(.split[[1]])]]
			
			.initString <- paste("+init=epsg", .epsg, sep = ":")
			.crs <- CRS(.initString)
			return(.crs)
		}
)
setMethod(f = "sosGetCRS",
		signature = c(obj = "OmObservationCollection"),
		def = function(obj) {
			.l <- lapply(X = obj, FUN = sosGetCRS)
			.l <- unique(.l)
			
			if(length(.l) == 1)
				return(.l[[1]])
			else return(.l)
		}
)
setMethod(f = "sosGetCRS",
		signature = c(obj = "OmObservation"),
		def = function(obj) {
			.crs <- .getCRSfromOM(obj)
			return(.crs)
		}
)
setMethod(f = "sosGetCRS",
		signature = c(obj = "OmMeasurement"),
		def = function(obj) {
			.crs <- .getCRSfromOM(obj)
			return(.crs)
		}
)
setMethod(f = "sosGetCRS",
		signature = c(obj = "SosObservationOffering"),
		def = function(obj) {
			.srsName <- sosBoundedBy(obj)[["srsName"]]
			.crs <- sosGetCRS(.srsName)
			return(.crs)
		}
)
setMethod(f = "sosGetCRS",
		signature = c(obj = "SOS"),
		def = function(obj) {
			.offs <- sosOfferings(obj)
			.crss <- lapply(.offs, sosGetCRS)
			if(length(.crss) == 1)
				return(.crss[[1]])
			return(.crss)
		}
)
setMethod(f = "sosGetCRS",
		signature = c(obj = "list"),
		def = function(obj) {
			.crs <- lapply(X = obj, FUN = sosGetCRS)
			return(.crs)
		}
)

.getCRSfromOM <- function(obj) {
	.char <- as.vector(sosCoordinates(obj)[[sosDefaultColumnNameSRS]])
	.l <- sapply(X = .char, FUN = sosGetCRS)
	.l <- unique(.l)
	
	if(length(.l) == 1)
		return(.l[[1]])
	else return(.l)
}

#
#
#
setMethod(f = "sosGetDCP",
		signature = c(sos = "SOS", operation = "character"),
		def = function(sos, operation, type = NA) {
			.ops <- sosOperations(sos)
			
			if(is.null(.ops)) return(NULL)
			
			.dcps <- .ops[[operation]]@DCPs
			
			if(!is.na(type)) {
				return(.dcps[[type]])
			}
			else return(.dcps)
		}
)


################################################################################
#
sosChanges <- function() {
	.libloc <- Sys.getenv("R_LIBS_USER")
	if(length(.libloc) < 1)
		.libloc <- Sys.getenv("R_LIBS")
	
	.path <- paste(Sys.getenv("R_LIBS_USER"), "sos4R", "CHANGES", sep = "/")
	.con <- file(.path)
	.lines <- readLines(.con)
	close(.con)	
	
	cat(.lines, sep = "\n")
}
