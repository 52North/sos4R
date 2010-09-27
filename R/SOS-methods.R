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

################################################################################
# construction functions
SOS <- function(url, method = SosDefaultConnectionMethod(),
		version = "1.0.0",
		parsers = SosParsingFunctions(),
		encoders = SosEncodingFunctions(),
		dataFieldConverters = SosFieldConvertingFunctions(),
		curlOpts = list(),
		curlHandle = getCurlHandle(),
		timeFormat = sosDefaultTimeFormat, verboseOutput = FALSE) {
	if(method == .sosConnectionMethodPost)
		.curlOpts <- curlOptions(url = url)
	else .curlOpts <- curlOpts
	
	.sos <- new("SOS",
			url = url,
			method = method,
			version = version,
			# dummy capabilities to be replaced below
			capabilities = new("OwsCapabilities", version = "NA",
					updateSequence = as.character(NA),
					owsVersion = "1.1.0"),
			parsers = parsers,
			encoders = encoders,
			dataFieldConverters = dataFieldConverters,
			curlOptions = .curlOpts,
			curlHandle = curlHandle,
			timeFormat = timeFormat,
			verboseOutput = verboseOutput)
	
	.caps <- getCapabilities(.sos, verbose = verboseOutput)
	if(!is(.caps, "OwsCapabilities")) {
		stop("ERROR: Did not receive a Capabilities response!")
	}
	
	.sos@capabilities <- .caps
	
	cat("Created SOS for URL", url, "\n")
	return(.sos)
}


SosFilter_Capabilities <- function(spatial = list(NA_character_),
		temporal = list(NA_character_), scalar = list(NA_character_),
		id = list(NA_character_)) {
	new("SosFilter_Capabilities", spatial = spatial, temporal = temporal,
			scalar = scalar, id = id)
}

SosCapabilities <- function(version,  updateSequence = NA, owsVersion = "1.1.0",
		identification = NULL, provider = NULL, operations = NULL,
		filterCaps = NULL, contents = NULL) {
	if(owsVersion == "1.1.0") {
		new("SosCapabilities_1.1.0",
				version = version, updateSequence = updateSequence,
				owsVersion = owsVersion,
				identification = identification,
				provider = provider, operations = operations,
				filterCaps = filterCaps, contents = contents)
	}
	else if(owsVersion == "2.0.0") {
		stop("Version 2.0.0 not supported!")
	}
	else {
		new("OwsCapabilities",
				version = version, updateSequence = updateSequence,
				owsVersion = owsVersion)
	}	
}

SosObservationOffering <- function(id, name = as.character(NA),
		time, procedure, observedProperty,
		featureOfInterest, responseFormat,
		intendedApplication = as.character(NA), resultModel = as.character(NA),
		responseMode = as.character(NA), boundedBy = list()) {
	new("SosObservationOffering", id = id, name = name,
			time = time, procedure = procedure,
			observedProperty = observedProperty,
			featureOfInterest = featureOfInterest,
			responseFormat = responseFormat,
			intendedApplication = intendedApplication,
			resultModel = resultModel, responseMode = responseMode,
			boundedBy = boundedBy)
}

SosContents <- function(observationOfferings) {
	new("SosContents", observationOfferings = observationOfferings)
}

SosEventTime <- function(temporalOps) {
	new("SosEventTime", temporalOps = temporalOps)
}

SosEventTimeLatest <- function() {
	new("SosEventTimeLatest")
}

SosFeatureOfInterest <- function(objectIDs = list(NA), spatialOps = NULL) {
	new("SosFeatureOfInterest", objectIDs = objectIDs, spatialOps = spatialOps)
}

#
# main request method
#
setMethod(f = "sosRequest",
		signature = signature(sos = "SOS", request = "OwsServiceOperation",
				verbose = "logical", inspect = "logical"),
		def = function(sos, request, verbose = FALSE, inspect = FALSE) {
			# check the request for consistency with service description
			.checkResult <- checkRequest(service = sos, operation = request,
					verbose = verbose)
			if(!.checkResult) {
				warning("Check returned FALSE! Turn on verbose option for possible details.",
						immediate. = TRUE)
			}
				
			.response = ""
			
			# get encoding function for the respective method
			.encodingFunction <- sos@encoders[[sos@method]]
			if(verbose) {
				.f <- functionBody(.encodingFunction)
				cat("ENCODING FUNCTION (beginning of function body): ",
						substring(text = .f, first = 0, last = 60), " ... [",
						max((length(.f) - 60), 0), " more chrs].\n")
			}
			
			# encode!
			.encodedRequest = .encodingFunction(obj = request, sos = sos,
					verbose = verbose)
			
			if(sos@method == .sosConnectionMethodGet) {
				.url = paste(sos@url, .encodedRequest, sep = "?")
				if(verbose || inspect) {
					cat("*** GET! REQUEST: ", .url, "\n")
				}
				
				.response = getURL(.url)
				
				if(verbose)
					cat("*** RESPONSE:\n", .response, "\n")
			}
			else if(sos@method == .sosConnectionMethodPost) {
				if(verbose || inspect) {
					cat("*** POST! REQUEST:\n")
					print(.encodedRequest)
				}
				
				# using 'POST' for application/x-www-form-urlencoded content
				.response <- postForm(uri = sos@url,
						request = toString(.encodedRequest),
						style = "POST",
						.encoding = sosDefaultCharacterEncoding)
				
				if(verbose) {
					cat("*** RESPONSE:\n")
					print(.response)
				}
			}
			else if(sos@method == .sosConnectionMethodSOAP) {
				if(verbose || inspect) {
					print("SOAP! REQUEST:\n")
					print(.encodedRequest)
				}
				
				# TODO SOAP request method
				
			}
			else {
				stop(paste("Unsupported method, has to be one",
								SosSupportedConnectionMethods()))
			}
			
			if(regexpr("<!DOCTYPE HTML", .response) > 0) {
				cat(.response, "\n")
				stop("ERROR: Got HTML response!")
			}
	
			return(.response)
	}
)


################################################################################
# functions for SOS operations

#
#
#
setMethod(f = "getCapabilities",
		signature = signature(sos = "SOS"),
		def = function(sos, verbose, inspect) {
			if (verbose) {
				cat("** GET CAPABILITIES of", sos@url, "\n")
			}
			
			.gc <- OwsGetCapabilities(service = sosService,
					acceptVersions = c(sos@version))
			if(verbose) cat("** REQUEST:\n", toString(.gc), "\n")
			
			.responseString = sosRequest(sos = sos, request = .gc,
					verbose = verbose, inspect = inspect)
			.response <- xmlParseDoc(.responseString, asText = TRUE)
			if(verbose || inspect) {
				cat("** RESPONSE DOC:\n")
				print(.response)
			}
			
			if(.isExceptionReport(.response)) {
				return(.handleExceptionReport(sos, .response))
			}
			else {
				.parsingFunction <- sosParsers(sos)[[sosGetCapabilitiesName]]
				.caps <- .parsingFunction(obj = .response, sos = sos)
				if (verbose) {
					cat("** DONE WITH PARSING!\n")
				} 
				return(.caps)
			}
		}
)


#
#
#
setMethod(f = "describeSensor",
		signature = signature(sos = "SOS", procedure  ="character"), 
		def = function(sos, procedure, outputFormat, verbose, inspect) {
			if(verbose) {
				cat("** DESCRIBE SENSOR: ", procedure, "@", sos@url, "\n")
			}
			
			.ds <- DescribeSensor(service = sosService, version = sos@version,
					procedure = procedure, outputFormat = outputFormat)
			if(verbose)
				cat("** REQUEST:\n", toString(.ds), "\n")
			
			
			.responseString = sosRequest(sos = sos, request = .ds,
					verbose = verbose, inspect = inspect)
			.response <- xmlParseDoc(.responseString, asText = TRUE)
			if(verbose || inspect) {
				cat("** RESPONSE DOC:\n")
				print(.response)
			}
			
			if(.isExceptionReport(.response)) {
				return(.handleExceptionReport(sos, .response))
			}
			else {
				.parsingFunction <- sosParsers(sos)[[sosDescribeSensorName]]
				.sml <- .parsingFunction(obj = .response)
				return(.sml)
			}
		}
)


#
# 
#
setMethod(f = "getObservationById",
		signature = signature(sos = "SOS", observationId = "character"), 
		def = function(sos, observationId, responseFormat, srsName,
				resultModel, responseMode, verbose, inspect) {
			if(verbose) {
				cat("** GET OBSERVATION BY ID: ", observationId, "\n")
			}
			
			.go <- GetObservationById(service = sosService,
					version = sos@version, observationId = observationId,
					responseFormat =  responseFormat, srsName = srsName,
					resultModel = resultModel, responseMode = responseMode)
			
			if(verbose)
				cat("** REQUEST:\n", toString(.go), "\n")
			
			
			.responseString = sosRequest(sos = sos, request = .go,
					verbose = verbose, inspect = inspect)
			.response <- xmlParseDoc(.responseString, asText = TRUE)
			if(verbose || inspect) {
				cat("** RESPONSE DOC:\n")
				print(.response)
			}
			
			if(.isExceptionReport(.response)) {
				return(.handleExceptionReport(sos, .response))
			}
			else {
				.parsingFunction <- sosParsers(sos)[[sosGetObservationByIdName]]
				.obs <- .parsingFunction(obj = .response, sos = sos,
						verbose = verbose)
				
				# remove list if only one element
				if(is.list(.obs) && length(.obs) == 1)
					.obs <- .obs[[1]]
				
				if(verbose) {
					cat("** PARSED RESPONSE:\n")
					print(.obs)
				}
				
				return(.obs)
			}
			
			return(.response)
		}
)


#
#
#
setMethod(f = "getObservation",
		signature = signature(sos = "SOS",
				offering = "SosObservationOfferingOrCharacter"),
		def = function(sos, offering, observedProperty, responseFormat, srsName,
				eventTime,	procedure, featureOfInterest, result, resultModel,
				responseMode, BBOX, latest, verbose, inspect) {
			
			if(is.character(offering))
				.offeringId <- offering
			else .offeringId <- offering@id
			
			if(verbose)
				cat("** GET OBSERVATION to ", sos@url, " with offering ",
						.offeringId, "\n")
			
			if(latest) .eventTime <- list(.createLatestEventTime(verbose))
			else .eventTime <- eventTime
			
			if(latest && !is.na(eventTime))
				warning("'Latest' is set to TRUE > given eventTime is ignored!")
			
			.go <- GetObservation(service = sosService, version = sos@version, 
					offering = .offeringId, observedProperty =  observedProperty,
					responseFormat =  responseFormat, srsName = srsName,
					eventTime = .eventTime, procedure = procedure,
					featureOfInterest = featureOfInterest, result = result,
					resultModel = resultModel, responseMode = responseMode,
					BBOX = BBOX)
			
			if(verbose)
				cat("** REQUEST:\n", toString(.go), "\n")
			
			.responseString = sosRequest(sos = sos, request = .go,
					verbose = verbose, inspect = inspect)
			.response <- xmlParseDoc(.responseString, asText = TRUE)
			if(verbose || inspect) {
				cat("** RESPONSE DOC:\n")
				print(.response)
			}
						
			if(.isExceptionReport(.response)) {
				return(.handleExceptionReport(sos, .response))
			}
			else {
				.parsingFunction <- sosParsers(sos)[[sosGetObservationName]]
				.obs <- .parsingFunction(obj = .response, sos = sos,
						verbose = verbose)
				
				# remove list if only one element
				if(is.list(.obs) && length(.obs) == 1) {
					.obs <- .obs[[1]]
					.resultLength <- length(sosResult(.obs))
				}
				
				if (is.list(.obs)) {
					# get the first element of the dim of every sosResult 
					.resultLength <- sapply(lapply(lapply(.obs, sosResult),
									dim), "[[", 1)
				}
				else {
					# not a list
					.dim <- dim(sosResult(.obs))
					if(!is.null(.dim)) {
						.resultLength <- .dim[[1]]
					}
					else {
						.resultLength <- NA_character_
					}
				}
				
				if(verbose) {
					cat("** PARSED RESPONSE:\n")
					print(.obs)
				}
				
				cat("Finished getObservation to", sos@url, "- received",
						length(.obs), "observation(s)/measurement(s) having",
						paste(.resultLength), "elements.\n")
				
				return(.obs)
			}
		}
)


################################################################################
# encoding functions

setMethod(f = "encodeXML",
		signature = signature(obj = "SosEventTime", sos = "SOS"),
		function(obj, sos, verbose = FALSE) {
			if(verbose) {
				cat("ENCODE XML", class(obj), "\n")
			}
			
			.temporalOpsClass <- class(obj@temporalOps)
			if(!is.null(SosSupportedTemporalOperators()[[.temporalOpsClass]])) {
				.eventTime <- xmlNode(name = sosEventTimeName,
						namespace = sosNamespacePrefix)
				.temporalOpsXML <- encodeXML(obj = obj@temporalOps,
						sos = sos, verbose = verbose)
				.eventTime$children[[1]] <- .temporalOpsXML
				
				return(.eventTime)
			}
			else {
				stop(paste("temporalOps type not supported:",
								.temporalOpsClass))
			}
		}
)
setMethod(f = "encodeXML",
		signature = signature(obj = "SosEventTimeLatest", sos = "SOS"),
		function(obj, sos, verbose = FALSE) {
			if(verbose) {
				cat("ENCODE XML", class(obj), "\n")
			}
			
			.eventTime <- xmlNode(name = sosEventTimeName,
					namespace = sosNamespacePrefix)
			.tmEquals <- xmlNode(name = ogcTempOpTMEqualsName,
					namespace = ogcNamespacePrefix)
			.propertyName <- xmlNode(name = ogcPropertyNameName,
					namespace = ogcNamespacePrefix)
			xmlValue(.propertyName) <- sosDefaultTempOpPropertyName
			.latestTime <- xmlNode(name = gmlTimeInstantName,
					namespace = gmlNamespacePrefix)
			.tpos <- xmlNode(name = gmlTimePositionName,
					namespace = gmlNamespacePrefix)
			xmlValue(.tpos) <- sosEventTimeLatestValue
			
			.latestTime$children[[1]] <- .tpos
			.tmEquals$children[[1]] <- .propertyName
			.tmEquals$children[[2]] <- .latestTime
			.eventTime$children[[1]] <- .tmEquals
			
			return(.eventTime)
		}
)

setMethod(f = "encodeXML",
		signature = signature(obj = "SosFeatureOfInterest", sos = "SOS"),
		function(obj, sos, verbose = FALSE) {
			if(verbose) {
				cat("ENCODE XML", class(obj), "\n")
			}
			
			.foi <- xmlNode(name = sosFeatureOfInterestName,
					namespace = sosNamespacePrefix)
			
			# switch between objectIDs and spatialOps
			if(!any(is.na(obj@objectIDs))) {
				.ids <- lapply(X = obj@objectIDs, FUN = xmlNode,
						name = sosObjectIDName, namespace = sosNamespacePrefix)
				.foi <- addChildren(node = .foi, kids = .ids)
			}
			else if (!is.null(obj@spatialOps)) {
				.spOp <- encodeXML(obj = obj@spatialOps, sos = sos,
						verbose = verbose)
				.foi <- addChildren(node = .foi, kids = list(.spOp))
			}
			
			return(.foi)
		}
)


#
# 
#
setMethod(f = "encodeKVP",
		signature = signature(obj = "SosEventTime", sos = "SOS"),
		function(obj, sos, verbose = FALSE) {
			if(verbose) {
				cat("ENCODE KVP ", class(obj), "\n")
			}
			
			.temporalOpsKVP <- encodeKVP(obj = obj@temporalOps, sos = sos,
					verbose = verbose)
			return(.temporalOpsKVP)
		}
)
