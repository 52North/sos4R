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

#
#
#
SOS <- function(url, method = SOSDefaultConnectionMethod(),
		version = "1.0.0", parsers = SOSParsers(), encoders = SOSEncoders(),
		dataFieldConverters = SOSFieldConverters(), curlOpts = list(),
		curlHandle = getCurlHandle(),
		timeFormat = sosDefaultTimeParsingFormat, verboseOutput = FALSE) {
	if(method == .sosConnectionMethodPost)
		.curlOpts <- curlOptions(url = url)
	else .curlOpts <- curlOpts
	
	.sos <- new("SOS",
			url = url,
			method = method,
			version = version,
			capabilities = new("OwsCapabilities", version = "NA", # dummy capabilities to be replaced below
					updateSequence = as.character(NA),
					owsVersion = "1.1.0"),
			parsers = parsers,
			encoders = encoders,
			dataFieldConverters = dataFieldConverters,
			curlOptions = .curlOpts,
			curlHandle = curlHandle,
			timeFormat = timeFormat,
			verboseOutput = verboseOutput)
	
	if(verboseOutput) {
		warning("Verbose output is activated!", immediate. = TRUE)
	}
	
	.caps <- getCapabilities(.sos, verbose = verboseOutput)
	.sos@capabilities <- .caps
	
	cat("Created SOS class from URL", url, "\n")
	return(.sos)
}


################################################################################
# other construction functions
#
SosFilter_Capabilities <- function(xmlNode) {
	new("SosFilter_Capabilities", xml = xmlNode)
}

SosCapabilities <- function(version,  updateSequence = NA, owsVersion = "1.1.0",
		identification, provider, operations, filterCaps, contents) {
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

#
# main request method
#
setMethod(f = "sosRequest",
		signature = signature(sos = "SOS", request = "ANY", verbose = "logical",
				inspect = "logical"),
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
			.encode <- sos@encoders[[sos@method]]
			if(verbose) {
				.f <- functionBody(.encode)
				cat("ENCODING FUNCTION (beginning of function body): ",
						substring(text = .f, first = 0, last = 60), " ... [",
						max((length(.f) - 60), 0), " more characters].\n")
			}
			
			if(sos@method == .sosConnectionMethodGet) {
				.kvpEncoding = .encode(request, verbose)
				.url = paste(sos@url, .kvpEncoding, sep = "?")
				if(verbose || inspect) {
					cat("*** GET! REQUEST: ", .url, "\n")
				}
				
				.response = getURL(.url)
				
				if(verbose) {
					cat("*** RESPONSE:\n")
					cat(.response)
				}
			}
			else if(sos@method == .sosConnectionMethodPost) {
				.xmlEncoding <- .encode(request, verbose)
				if(verbose || inspect) {
					cat("*** POST! REQUEST:\n")
					print(.xmlEncoding)
				}
				
				# using 'POST' for application/x-www-form-urlencoded content
				.response <- postForm(uri = sos@url,
						request = toString(.xmlEncoding),
						style = "POST",
						.encoding = sosDefaultCharacterEncoding)
				
				if(verbose) {
					cat("*** RESPONSE:\n")
					print(.response)
				}
			}
			else if(sos@method == .sosConnectionMethodSOAP) {
				.soapEncoding <- .encode(request, verbose)
				
				if(verbose || inspect) {
					print("SOAP! REQUEST:\n")
				}
				
				# TODO implement SOAP stuff
				
			}
			else {
				stop(paste("Unsupported method, has to be one",
								SosSupportedConnectionMethods()))
			}
	
			return(.response)
	}
)


#
# helper methods for exception response handling
#
.isExceptionReport <- function(document) {
	if(sosOwsExceptionReportRootName == xmlName(xmlRoot(document)))
		return(TRUE)
	else
		return(FALSE)
}


################################################################################
# accessor functions
if (!isGeneric("sosCaps"))
	setGeneric(name = "sosCaps", def = function(sos) {
				standardGeneric("sosCaps")
			})
setMethod(f = "sosCaps", signature = signature(sos = "SOS"), def = function(sos) {
			return(sos@capabilities)
		}
)

if (!isGeneric("sosUrl"))
	setGeneric(name = "sosUrl", def = function(sos) {
				standardGeneric("sosUrl")
			})
setMethod(f = "sosUrl", signature = signature(sos = "SOS"), def = function(sos) {
			return(sos@url)
		})

if (!isGeneric("sosVersion"))
	setGeneric(name = "sosVersion", def = function(sos) {
				standardGeneric("sosVersion")
			})
setMethod(f = "sosVersion", signature = signature(sos = "SOS"), def = function(sos) {
			return(sos@version)
		})

if (!isGeneric("sosMethod"))
	setGeneric(name = "sosMethod", def = function(sos) {
				standardGeneric("sosMethod")
			})
setMethod(f = "sosMethod", signature = signature(sos = "SOS"), def = function(sos) {
			return(sos@method)
		})

if (!isGeneric("sosProcedures"))
	setGeneric(name = "sosProcedures", def = function(sos) {
				standardGeneric("sosProcedures")
			})
setMethod(f = "sosProcedures", signature = signature(sos = "SOS"), def = function(sos) {
			.caps <- sosCaps(sos)
			.ds <- .caps@operations@operations[["GetObservation"]]
			return(.ds@parameters$procedure)
		})

if (!isGeneric("sosObservedProperties"))
	setGeneric(name = "sosObservedProperties", def = function(sos) {
				standardGeneric("sosObservedProperties")
			})
setMethod(f = "sosObservedProperties", signature = signature(sos = "SOS"),
		def = function(sos) {
			.caps <- sosCaps(sos)
			.getOb <- .caps@operations@operations[[sosGetObservationName]]
			return(.getOb@parameters[[sosObservedPropertyName]])
		})

if (!isGeneric("sosOfferings"))
	setGeneric(name = "sosOfferings", def = function(sos) {
				standardGeneric("sosOfferings")
			})
setMethod(f = "sosOfferings", signature = signature(sos = "SOS"), def = function(sos) {
			.offerings <- sos@capabilities@contents@observationOfferings
			return(.offerings)
		})
if (!isGeneric("sosOffering"))
	setGeneric(name = "sosOffering", def = function(sos, offeringId) {
				standardGeneric("sosOffering")
			})
setMethod(f = "sosOffering", signature = signature(sos = "SOS", 
				offeringId = "character"),
		def = function(sos, offeringId) {
			.offerings <- sos@capabilities@contents@observationOfferings
			return(.offerings[[offeringId]])
		})
if (!isGeneric("sosOfferingIds"))
	setGeneric(name = "sosOfferingIds", def = function(sos) {
				standardGeneric("sosOfferingIds")
			})
setMethod(f = "sosOfferingIds", signature = signature(sos = "SOS"),
		def = function(sos) {
			return(names(sosOfferings(sos)))
		})

if (!isGeneric("sosFOIs"))
	setGeneric(name = "sosFOIs", def = function(sos) {
				standardGeneric("sosFOIs")
			})
setMethod(f = "sosFOIs", signature = signature(sos = "SOS"), def = function(sos) {
			.caps <- sosCaps(sos)
			
			# via GetFeatureOfInterest
			.gfoi <- .caps@operations@operations[["GetFeatureOfInterest"]]
			if(!is.null(.gfoi)) {
				return(.gfoi@parameters$featureOfInterestId)
			}
			else return("GetFeatureOfInterest-Operation not supported!")
				
			return(.fois)
		})

if (!isGeneric("sosOperationInfo"))
	setGeneric(name = "sosOperationInfo", def = function(sos, operationName) {
				standardGeneric("sosOperationInfo")
			})
setMethod(f = "sosOperationInfo",
		signature = signature(sos = "SOS", operationName = "character"),
		def = function(sos, operationName) {
			.caps <- sosCaps(sos)
			return(.caps@operations@operations[[operationName]])
		})

if (!isGeneric("sosResponseFormats"))
	setGeneric(name = "sosResponseFormats", def = function(sos) {
				standardGeneric("sosResponseFormats")
			})
setMethod(f = "sosResponseFormats", signature = signature(sos = "SOS"),
		def = function(sos) {
			.caps <- sosCaps(sos)
			.getOb <- .caps@operations@operations[[sosGetObservationName]]
			return(.getOb@parameters$responseFormat)
		})

if (!isGeneric("sosResponseMode"))
	setGeneric(name = "sosResponseMode", def = function(sos) {
				standardGeneric("sosResponseMode")
			})
setMethod(f = "sosResponseMode", signature = signature(sos = "SOS"),
		def = function(sos) {
			.caps <- sosCaps(sos)
			.getOb <- .caps@operations@operations[[sosGetObservationName]]
			return(.getOb@parameters$responseMode)
		})

if (!isGeneric("sosSrsName"))
	setGeneric(name = "sosSrsName", def = function(sos) {
				standardGeneric("sosSrsName")
			})
setMethod(f = "sosSrsName", signature = signature(sos = "SOS"),
		def = function(sos) {
			.caps <- sosCaps(sos)
			.getOb <- .caps@operations@operations[[sosGetObservationName]]
			return(.getOb@parameters$srsName)
		})

if (!isGeneric("sosResultModels"))
	setGeneric(name = "sosResultModels", def = function(sos) {
				standardGeneric("sosResultModels")
			})
setMethod(f = "sosResultModels", signature = signature(sos = "SOS"),
		def = function(sos) {
			.caps <- sosCaps(sos)
			.getOb <- .caps@operations@operations[[sosGetObservationName]]
			return(.getOb@parameters$resultModel)
		})

if (!isGeneric("sosTimePeriod"))
	setGeneric(name = "sosTimePeriod", def = function(obj) {
				standardGeneric("sosTimePeriod")
			})
setMethod(f = "sosTimePeriod", signature = signature(obj = "SOS"),
		def = function(obj) {
			.caps <- sosCaps(obj)
			.getOb <- .caps@operations@operations[[sosGetObservationName]]
			return(.getOb@parameters$eventTime)
		})
setMethod(f = "sosTimePeriod", signature = signature(obj = "SosObservationOffering"),
		def = function(obj) {
			return(obj@time)
		})

if (!isGeneric("sosTimeFormat"))
	setGeneric(name = "sosTimeFormat", def = function(sos) {
				standardGeneric("sosTimeFormat")
			})
setMethod(f = "sosTimeFormat", signature = signature(sos = "SOS"),
		def = function(sos) {
			return(sos@timeFormat)
		})

if (!isGeneric("sosParsers"))
	setGeneric(name = "sosParsers", def = function(sos) {
				standardGeneric("sosParsers")
			})
setMethod(f = "sosParsers", signature = signature(sos = "SOS"),
		def = function(sos) {
			return(sos@parsers)
		})

if (!isGeneric("sosFieldConverters"))
	setGeneric(name = "sosFieldConverters", def = function(sos) {
				standardGeneric("sosFieldConverters")
			})
setMethod(f = "sosFieldConverters", signature = signature(sos = "SOS"),
		def = function(sos) {
			return(sos@dataFieldConverters)
		})

#
# set parsers, encoders and convertes to default
#
if (!isGeneric("sosSetFunctionsToDefault"))
	setGeneric(name = "sosSetFunctionsToDefault", def = function(sos) {
				standardGeneric("sosSetFunctionsToDefault")
			})
setMethod(f = "sosSetFunctionsToDefault", signature = signature(sos = "SOS"),
		def = function(sos) {
			.defaultMethods <- sosInitDefaults()
			sos@parsers <- .defaultMethods[[1]]
			sos@encoders <- .defaultMethods[[2]]
			sos@dataFieldConverters <- .defaultMethods[[3]]
		})

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
			.time <- format(time, sos@timeFormat)
			.timePos <- GmlTimePosition(time = strptime(.time, sos@timeFormat),
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
			.beginPos <- GmlTimePosition(
					time = strptime(
							format(begin, sos@timeFormat),
							sos@timeFormat),
					frame = frame, calendarEraName = calendarEraName,
					indeterminatePosition = indeterminatePosition
					)
			.endPos <- GmlTimePosition(
					time = strptime(
							format(end, sos@timeFormat),
							sos@timeFormat),
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
# sosCreateEventTime(SosSupportedTemporalOperators()[["TM_During"]], p1)
#
if (!isGeneric("sosCreateEventTime"))
	setGeneric(name = "sosCreateEventTime",
			def = function(time, operator = sosDefaultTemporalOperator) {
				standardGeneric("sosCreateEventTime")
			})
setMethod(f = "sosCreateEventTime",
		signature = signature(time = "GmlTimeGeometricPrimitive",
				operator = "ANY"),
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
								" choose one of",
								SosSupportedTemporalOperators()))
			}
			
			.et <- list(SosEventTime(.tOps))
			return(.et)
		}
)


################################################################################
# encoding functions

#
#
#
setMethod("encodeXML", "SosEventTime", 
		function(obj, verbose = FALSE) {
			if(verbose) {
				cat("ENCODE XML", class(obj), "\n")
			}
			
			.temporalOpsClass <- class(obj@temporalOps)
			if(!is.null(SosSupportedTemporalOperators()[[.temporalOpsClass]])) {
				.eventTime <- xmlNode(name = sosEventTimeName,
						namespace = sosNamespacePrefix)
				.temporalOpsXML <- encodeXML(obj@temporalOps, verbose)
				.eventTime$children[[1]] <- .temporalOpsXML
				
				return(.eventTime)
			}
			else {
				stop(paste("temporalOps type not supported:",
								.temporalOpsClass))
			}
		}
)
setMethod("encodeXML", "SosEventTimeLatest", 
		function(obj, verbose = FALSE) {
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
			xmlValue(.tpos) <- "latest"

			.latestTime$children[[1]] <- .tpos
			.tmEquals$children[[1]] <- .propertyName
			.tmEquals$children[[2]] <- .latestTime
			.eventTime$children[[1]] <- .tmEquals
			
			return(.eventTime)
		}
)


#
# 
#
setMethod("encodeKVP", "SosEventTime", 
		function(obj, verbose = FALSE) {
			if(verbose) {
				cat("ENCODE KVP ", class(obj), "\n")
			}
			
			.temporalOpsKVP <- encodeKVP(obj@temporalOps)
			return(.temporalOpsKVP)
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
				cat("Requesting capabilities... ")
			}
			
			.gc <- OwsGetCapabilities(service = sosService,
					acceptVersions = c(sos@version))
			if(verbose) {
				cat("** REQUEST:\n")
				print(.gc)
			}
			
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
				cat("DESCRIBE SENSOR: ", procedure, "@", sos@url, "\n")
			}
			
			.ds <- DescribeSensor(service = sosService, version = sos@version,
					procedure = procedure, outputFormat = outputFormat)
			if(verbose) {
				cat("REQUEST:\n")
				print(.ds)
			}
			
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
			.go <- GetObservationById(service = sosService,
					version = sos@version, observationId = observationId,
					responseFormat =  responseFormat, srsName = srsName,
					resultModel = resultModel, responseMode = responseMode)
			if(verbose) {
				cat("REQUEST:\n");
				print(.go)
			}
			
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
				offering = "ANY", # SosObservationOffering or character
				observedProperty = "list"),
		def = function(sos, offering, observedProperty, responseFormat, srsName,
				eventTime,	procedure, featureOfInterest, result, resultModel,
				responseMode, BBOX, latest, verbose, inspect) {
			
			if(is.character(offering))
				.offeringId <- offering
			else .offeringId <- offering@id
			
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
			if(verbose  || inspect) {
				cat("REQUEST:\n");
				print(.go)
			}
			
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
				if(is.list(.obs) && length(.obs) == 1)
					.obs <- .obs[[1]]
				
				if(verbose) {
					cat("** PARSED RESPONSE:\n")
					print(.obs)
				}
				
				return(.obs)
			}
		}
)


################################################################################
#
.handleExceptionReport <- function(sos, obj) {
	if(sos@verboseOutput) warning("Received ExceptionReport!")
		
	.parsingFunction <- sosParsers(sos)[[sosOwsExceptionReportRootName]]
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

################################################################################
# conversion methods
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
