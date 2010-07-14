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
SOS <- function(url, method = "GET", version = "1.0.0", verboseOutput = FALSE) {
	.sos <- new("SOS", url = url, method = method, version = version,
			capabilities = new("OwsCapabilities", version = "NA", 
					updateSequence = as.character(NA),
					owsVersion = "1.1.0"),
			verboseOutput = verboseOutput)
	if(verboseOutput) warning("Verbose output is activated!", immediate. = TRUE)
	
	.caps <- getCapabilities(.sos, verbose = verboseOutput)
	.sos@capabilities <- .caps
	return(.sos)
}

#
#
#
if (!isGeneric("sosRequest"))
	setGeneric(name = "sosRequest",
			def = function(sos, request, verbose = FALSE) {
				standardGeneric("sosRequest")
			})
setMethod(f = "sosRequest",
		signature = c(sos = "SOS", request = "ANY", verbose = "logical"),
		def = function(sos, request, verbose = FALSE) {
			.checkResult <- checkRequest(service = sos, operation = request,
					verbose = verbose)
			if(!.checkResult) {
				warning("Check returned FALSE!", immediate. = TRUE)
				if(!verbose) {
					checkRequest(service = sos, operation = request,
							verbose = TRUE)
				}
			}
				
			.response = ""
			
			if(sos@method == "GET") {
				.kvpEncoding = kvp(request)
				.url = paste(sos@url, .kvpEncoding, sep="?")
				if(verbose) cat("*** GET! REQUEST: ", .url, "\n")
				
				.response = getURL(.url)
				if(verbose) {
					cat("*** RESPONSE:\n")
					cat(.response)
				}
			}
			else if(sos@method == "POST") {
				.xmlEncoding <- encode(request, verbose)
				if(verbose) {
					cat("*** POST! REQUEST:\n")
					print(.xmlEncoding)
				}
				
				# using 'post' for application/x-www-form-urlencoded content
				.response <- postForm(uri = sos@url,
						request = toString(.xmlEncoding),
						style = "POST",
						.encoding = "UTF-8")
				
				if(verbose) {
					cat("*** RESPONSE:\n")
					print(.response)
				}
			}
			else if(sos@method == "SOAP") {
				if(verbose) print("SOAP!")
				
				# TODO implement SOAP stuff
				
			}
			else {
				warning("Unsupported method, has to be one of GET, POST, or SOAP!")
			}
	
			return(.response)
	}
)


#
# helper methods for exception response handling
#
.isExceptionReport <- function(document) {
	if("ExceptionReport" == xmlName(xmlRoot(document)))
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
setMethod(f = "sosCaps", signature = c(sos = "SOS"), def = function(sos) {
			return(sos@capabilities)
		}
)

if (!isGeneric("sosUrl"))
	setGeneric(name = "sosUrl", def = function(sos) {
				standardGeneric("sosUrl")
			})
setMethod(f = "sosUrl", signature = c(sos = "SOS"), def = function(sos) {
			return(sos@url)
		})

if (!isGeneric("sosVersion"))
	setGeneric(name = "sosVersion", def = function(sos) {
				standardGeneric("sosVersion")
			})
setMethod(f = "sosVersion", signature = c(sos = "SOS"), def = function(sos) {
			return(sos@version)
		})

if (!isGeneric("sosMethod"))
	setGeneric(name = "sosMethod", def = function(sos) {
				standardGeneric("sosMethod")
			})
setMethod(f = "sosMethod", signature = c(sos = "SOS"), def = function(sos) {
			return(sos@method)
		})

if (!isGeneric("sosProcedures"))
	setGeneric(name = "sosProcedures", def = function(sos) {
				standardGeneric("sosProcedures")
			})
setMethod(f = "sosProcedures", signature = c(sos = "SOS"), def = function(sos) {
			.caps <- sosCaps(sos)
			.ds <- .caps@operations@operations[["GetObservation"]]
			return(.ds@parameters$procedure)
		})

if (!isGeneric("sosObservedProperties"))
	setGeneric(name = "sosObservedProperties", def = function(sos) {
				standardGeneric("sosObservedProperties")
			})
setMethod(f = "sosObservedProperties", signature = c(sos = "SOS"),
		def = function(sos) {
			.caps <- sosCaps(sos)
			.getOb <- .caps@operations@operations[["GetObservation"]]
			return(.getOb@parameters$observedProperty)
		})

if (!isGeneric("sosOfferings"))
	setGeneric(name = "sosOfferings", def = function(sos) {
				standardGeneric("sosOfferings")
			})
setMethod(f = "sosOfferings", signature = c(sos = "SOS"), def = function(sos) {
			.offerings <- sos@capabilities@contents@observationOfferings
			return(.offerings)
		})
if (!isGeneric("sosOffering"))
	setGeneric(name = "sosOffering", def = function(sos, offeringId) {
				standardGeneric("sosOffering")
			})
setMethod(f = "sosOffering", signature = c(sos = "SOS", 
				offeringId = "character"),
		def = function(sos, offeringId) {
			.offerings <- sos@capabilities@contents@observationOfferings
			return(.offerings[[offeringId]])
		})
if (!isGeneric("sosOfferingIds"))
	setGeneric(name = "sosOfferingIds", def = function(sos) {
				standardGeneric("sosOfferingIds")
			})
setMethod(f = "sosOfferingIds", signature = c(sos = "SOS"),
		def = function(sos) {
			return(names(sosOfferings(sos)))
		})

if (!isGeneric("sosFOIs"))
	setGeneric(name = "sosFOIs", def = function(sos) {
				standardGeneric("sosFOIs")
			})
setMethod(f = "sosFOIs", signature = c(sos = "SOS"), def = function(sos) {
			.caps <- sosCaps(sos)
			
			# via GetFeatureOfInterest
			.gfoi <- .caps@operations@operations[["GetFeatureOfInterest"]]
			if(!is.null(.gfoi)) {
				return(.gfoi@parameters$featureOfInterestId)
			}
			else return("GetFeatureOfInterest-Operation not supported!")
				
			return(.fois)
		})

if (!isGeneric("sosOperation"))
	setGeneric(name = "sosOperation", def = function(sos, operationName) {
				standardGeneric("sosOperation")
			})
setMethod(f = "sosOperation",
		signature = c(sos = "SOS", operationName = "character"),
		def = function(sos, operationName) {
			.caps <- sosCaps(sos)
			return(.caps@operations@operations[[operationName]])
		})

if (!isGeneric("sosResponseFormats"))
	setGeneric(name = "sosResponseFormats", def = function(sos) {
				standardGeneric("sosResponseFormats")
			})
setMethod(f = "sosResponseFormats", signature = c(sos = "SOS"),
		def = function(sos) {
			.caps <- sosCaps(sos)
			.getOb <- .caps@operations@operations[["GetObservation"]]
			return(.getOb@parameters$responseFormat)
		})

if (!isGeneric("sosResponseMode"))
	setGeneric(name = "sosResponseMode", def = function(sos) {
				standardGeneric("sosResponseMode")
			})
setMethod(f = "sosResponseMode", signature = c(sos = "SOS"),
		def = function(sos) {
			.caps <- sosCaps(sos)
			.getOb <- .caps@operations@operations[["GetObservation"]]
			return(.getOb@parameters$responseMode)
		})

if (!isGeneric("sosSrsName"))
	setGeneric(name = "sosSrsName", def = function(sos) {
				standardGeneric("sosSrsName")
			})
setMethod(f = "sosSrsName", signature = c(sos = "SOS"),
		def = function(sos) {
			.caps <- sosCaps(sos)
			.getOb <- .caps@operations@operations[["GetObservation"]]
			return(.getOb@parameters$srsName)
		})

if (!isGeneric("sosResultModels"))
	setGeneric(name = "sosResultModels", def = function(sos) {
				standardGeneric("sosResultModels")
			})
setMethod(f = "sosResultModels", signature = c(sos = "SOS"),
		def = function(sos) {
			.caps <- sosCaps(sos)
			.getOb <- .caps@operations@operations[["GetObservation"]]
			return(.getOb@parameters$resultModel)
		})

if (!isGeneric("sosTimePeriod"))
	setGeneric(name = "sosTimePeriod", def = function(obj) {
				standardGeneric("sosTimePeriod")
			})
setMethod(f = "sosTimePeriod", signature = c(obj = "SOS"),
		def = function(obj) {
			.caps <- sosCaps(obj)
			.getOb <- .caps@operations@operations[["GetObservation"]]
			return(.getOb@parameters$eventTime)
		})
setMethod(f = "sosTimePeriod", signature = c(obj = "SosObservationOffering"),
		def = function(obj) {
			return(obj@time)
		})


################################################################################
# functions for SOS operations

if (!isGeneric("getCapabilities"))
	setGeneric(name = "getCapabilities",
			valueClass = c("SosCapabilities", "SosCapabilities_1.1.0",
					"OwsExceptionReport"),
			signature = signature("sos", "verbose"),
			def = function(sos, verbose = FALSE) {
				standardGeneric("getCapabilities")	
			})
#
#
#
setMethod(f = "getCapabilities",
		signature = c(sos = "SOS", verbose = "ANY"),
		def = function(sos, verbose = FALSE) {
			if (verbose) print("Requesting capabilities... ")

			.gc <- OwsGetCapabilities(service = "SOS", acceptVersions = c(sos@version))
			if(verbose) {
				cat("REQUEST:\n")
				print(.gc)
			}
			
			.responseString = sosRequest(sos = sos, request = .gc, verbose)
			.response <- xmlParseDoc(.responseString, asText = TRUE)
			if(verbose) {
				cat("RESPONSE:\n")
				print(.response)
			}
			
			if(.isExceptionReport(.response)) {
				.er <- parseOwsExceptionReport(.response)
				if (verbose) cat(" done - Exception!\n")
				warning(toString(.er))
				return(.er)
			}
			else {
				.caps <- parseSosCapabilities(.response)
				if (verbose) cat("done!\n")
				return(.caps)
			}
		}
)


if (!isGeneric("describeSensor"))
	setGeneric(name = "describeSensor",
			valueClass = c("SensorML", "OwsExceptionReport"),
			signature = signature("sos", "procedure", "verbose"),
			def = function(sos, procedure, verbose = FALSE) {
				standardGeneric("describeSensor")	
			})
#
#
#
setMethod(f = "describeSensor",
		signature = c(sos = "SOS", procedure  ="character", 
				verbose = "ANY"), 
		def = function(sos, procedure, verbose = FALSE) {
			if(verbose) {
				cat("DESCRIBE SENSOR: ", procedure, "@", sos@url, "\n")
			}
			
			.ds <- DescribeSensor(service = "SOS", version = sos@version,
					procedure = procedure)
			if(verbose) {
				cat("REQUEST:\n")
				print(.ds)
			}
			
			.responseString = sosRequest(sos = sos, request = .ds, verbose)
			.response <- xmlParseDoc(.responseString, asText = TRUE)
			if(verbose) {
				cat("RESPONSE:\n")
				print(.response)
			}
			
			if(.isExceptionReport(.response)) {
				print("Received ExceptionReport in describeSensor!")
				.er <- parseOwsExceptionReport(.response)
				warning(toString(.er))
				return(.er)
			}
			else {
				.sml = SensorML(.response)
				return(.sml)
			}
		}
)


if (!isGeneric("getObservation"))
	setGeneric(name = "getObservation",
			#valueClass = c("OwsExceptionReport"),
			signature = signature("sos", "offering", "observedProperty",
					"responseFormat", "srsName", "eventTime", "procedure",
					"featureOfInterest", "result", "resultModel",
					"responseMode", "BBOX", "verbose"),
			def = function(sos, offering, observedProperty, responseFormat,
					srsName = as.character(NA), eventTime = as.character(NA), 
					procedure = c(NA), featureOfInterest = c(NA), 
					result = as.character(NA), resultModel = as.character(NA),
					responseMode = as.character(NA), BBOX = as.character(NA),
					verbose = FALSE) {
				standardGeneric("getObservation")
			})
#
#
#
setMethod(f = "getObservation",
		signature = c(sos = "SOS",
				offering = "character",
				observedProperty = "character",
				responseFormat = "character",
				#optional:
				srsName = "ANY", # "character",
				eventTime = "ANY", # "character", 
				procedure = "ANY", # "vector",
				featureOfInterest = "ANY", # "vector", 
				result = "ANY", # "character",
				resultModel = "ANY", # "character",
				responseMode = "ANY", # "character",
				BBOX = "ANY", # "character",
				verbose = "ANY"),  # "logical"
		function(sos, offering, observedProperty, responseFormat, 
				srsName = as.character(NA), eventTime = as.character(NA), 
				procedure = c(NA), featureOfInterest = c(NA), 
				result = as.character(NA), resultModel = as.character(NA),
				responseMode = as.character(NA), BBOX = as.character(NA),
				verbose = FALSE) {
			.go <- GetObservation(service = "SOS", version = sos@version, 
					offering = offering, observedProperty =  observedProperty,
					responseFormat =  responseFormat, srsName = srsName,
					eventTime = eventTime, procedure = procedure,
					featureOfInterest = featureOfInterest, result = result,
					resultModel = resultModel, responseMode = responseMode,
					BBOX = BBOX)
			if(verbose) cat("REQUEST:\n"); print(.go)
			
			.responseString = sosRequest(sos = sos, request = .go, verbose)
			.response <- xmlParseDoc(.responseString, asText = TRUE)
			
			if(.isExceptionReport(.response)) {
				cat("Received ExceptionReport in getObservation!\n")
				.er <- parseOwsExceptionReport(.response)
				warning(toString(.er))
				return(.er)
			}
			else {
				if(verbose) {
					cat("getObservation - parsed response:\n")
					print(.response)
				}
				
				# TODO add a check whether there is handling for the given response method implemented
				
				# TODO parse and return data.frame? sptX class?
			}
			
			return(.response)
		}
)


if (!isGeneric("getObservationById"))
	setGeneric(name = "getObservationById",
			def = function(sos, observationId, responseFormat, srsName,
					resultModel, responseMode, verbose = FALSE) {
				standardGeneric("getObservationById")
			})
#
#
#
setMethod(f = "getObservationById",
		signature = c(sos = "SOS", observationId = "character",
				responseFormat = "character", srsName = "character",
				resultModel = "character", responseMode = "character",
				verbose = "logical"), 
		function(sos, observationId, responseFormat, srsName = as.character(NA),
				resultModel = as.character(NA), responseMode = as.character(NA),
				verbose = FALSE) {
			.go <- GetObservation(service = "SOS", version = sos@version, 
					offering = offering, observedProperty =  observedProperty,
					responseFormat =  responseFormat, srsName = srsName,
					resultModel = resultModel, responseMode = responseMode)
			if(verbose) cat("REQUEST:\n"); print(.go)
			
			.responseString = sosRequest(sos = sos, request = .go, verbose)
			.response <- xmlParseDoc(.responseString, asText = TRUE)
			if(verbose) cat("RESPONSE:\n"); print(.response)
			
			if(.isExceptionReport(.response)) {
				print("Received ExceptionReport in describeSensor!")
				.er <- parseOwsExceptionReport(.response)
				warning(toString(.er))
				return(.er)
			}
			else {
				# TODO add a check whether there is handling for the given response method implemented
				
				# TODO parse and return data.frame? sptX class?
			}
			
			return(.response)
		}
)

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
		warning("Version 2.0.0 not supported!")
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
