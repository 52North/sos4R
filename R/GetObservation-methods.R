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
GetObservation <- function(
		service,
		version,
		offering, 
		observedProperty,
		responseFormat, 
		srsName = as.character(NA),
		eventTime = list(NA), 
		procedure = list(NA),
		featureOfInterest = NULL, 
		result = NULL,
		resultModel = as.character(NA),
		responseMode = as.character(NA),
		BBOX = as.character(NA)) {
	new("GetObservation",
			request = sosGetObservationName,
			service = service,
			version = version,
			offering = offering, 
			observedProperty = observedProperty,
			responseFormat = responseFormat,
			srsName = srsName,
			eventTime = eventTime,
			procedure = procedure, 
			featureOfInterest = featureOfInterest,
			result = result,
			resultModel = resultModel,
			responseMode = responseMode,
			BBOX = BBOX)
}

#
#
#
GetObservationById <- function(
		service,
		version,
		observationId,
		responseFormat, 
		srsName = as.character(NA),
		resultModel = as.character(NA),
		responseMode = as.character(NA)) {
	new("GetObservationById",
			request = sosGetObservationByIdName,
			service = service,
			version = version,
			observationId = observationId,
			responseFormat = responseFormat,
			srsName = srsName,
			resultModel = resultModel,
			responseMode = responseMode)
}

#
# encode as KVP
#
setMethod("encodeRequestKVP", "GetObservation", 
	function(obj, sos, verbose = FALSE) {
		if(obj@version == "1.0.0") {
			return(.sosEncodeRequestKVPGetObservation_1.0.0(obj, sos, verbose))		
		}
		else {
			stop("Version not supported!")
		}
	}
)
.sosEncodeRequestKVPGetObservation_1.0.0 <- function(obj, sos, verbose = FALSE) {
	# required:
	.request <- paste("request" , sosGetObservationName, sep = "=")
	.service <- paste("service",
			.kvpEscapeSpecialCharacters(x = obj@service), sep = "=")
	.version <- paste("version",
			.kvpEscapeSpecialCharacters(x = obj@version), sep = "=")
	.offering <- paste("offering",
			.kvpEscapeSpecialCharacters(x = obj@offering), sep = "=")
	.observedProperty <- .kvpKeyAndValues("observedProperty", 
			obj@observedProperty)
			
	.mandatory <- paste(.service, .request, .version, .offering,
			.observedProperty, sep = "&")
	
	# optional:
	.optionals = ""
	# is optional for GET
	if( !is.na(obj@responseFormat)) {
		.responseFormat <- paste(
			"responseFormat", 
			.kvpEscapeSpecialCharacters(x = gsub(obj@responseFormat,
							pattern = "&quot;",
							replacement = '"')),
			sep = "=")
		optionals <- paste(.optionals, .responseFormat, sep = "&")
	}
	
	if( !is.na(obj@srsName)) {
		.optionals <- paste(.optionals, paste("srsName", 
						.kvpEscapeSpecialCharacters(x = obj@srsName),
						sep = "="),
				sep = "&")
	}
	
	if( !is.na(obj@eventTime)) {
		if(length(obj@eventTime) > 1)
			warning("Only first event time in the list is used for KVP!")
		
		.timeString <- encodeKVP(obj = obj@eventTime[[1]],
				sos = sos, verbose = verbose)
		# if the eventTime is a latest request, it returns NA, the GET binding
		# says for the latest observation eventTime is omitted
		if(!is.na(.timeString)) {
			.optionals <- paste(.optionals, paste("eventTime", 
							.kvpEscapeSpecialCharacters(x = .timeString), 
							sep = "="), 
					sep = "&")
		}
		else {
			if(verbose) cat("encodeKVP returned NA for eventTime, omitting",
						"parameter for request for latest observation.")
		}
	}
	
	if( !any(sapply(obj@procedure, "is.na"), na.rm = TRUE)) {
		.optionals <- paste(.optionals, .kvpKeyAndValues("procedure",
						obj@procedure), sep = "&")
	}
	
	if( !is.null(obj@featureOfInterest)) {
		.optionals <- paste(.optionals, .kvpKeyAndValues("featureOfInterest",
						obj@featureOfInterest), sep = "&")
	}
	
	if( !is.null(obj@result)) {
		warning("GetObservation contains result, but that is not supported for 'GET' - parameter is discarded, use another method to include it!")
	}
	
	if( !is.na(obj@resultModel)) {
		.optionals <- paste(.optionals, paste("resultModel",
						.kvpEscapeSpecialCharacters(x = obj@resultModel),
						sep = "="),
				sep = "&")
	}
	
	if( !is.na(obj@responseMode)) {
		.optionals <- paste(.optionals, paste("responseMode",
						.kvpEscapeSpecialCharacters(x = obj@responseMode),
						sep = "="),
				sep = "&")
	}
	
	if( !is.na(obj@BBOX)) {
		.optionals <- paste(.optionals, paste("BBOX", 
						.kvpEscapeSpecialCharacters(x = obj@BBOX), sep = "="),
				sep = "&")
	}
	
	.kvpString <- paste(.mandatory, .optionals, sep = "")
		
	return(.kvpString)
}

setMethod("encodeRequestKVP", "GetObservationById", 
		function(obj, sos, verbose = TRUE) {
			stop("KVP encoding of operation 'GetObservationById' not supported!")
		}
)

#
# encode as XML
#
setMethod("encodeRequestXML", "GetObservation", 
	function(obj, sos, verbose = FALSE) {
		if(verbose) {
			cat("ENCODE XML", class(obj), "\n")
		}
		
		if(obj@version == "1.0.0") {
			return(.sosEncodeRequestXMLGetObservation_1.0.0(obj = obj,
							sos = sos, verbose = verbose))		
		}
		else {
			stop("Version not supported!")
		}
	}
)
.sosEncodeRequestXMLGetObservation_1.0.0 <- function(obj, sos, verbose = FALSE) {
	.xmlDoc <- xmlNode(name = sosGetObservationName,
			namespace = sosNamespacePrefix,
			namespaceDefinitions = c(.sosNamespaceDefinitionsAll,
					.sosNamespaceDefinitionsGetObs),
			attrs=c(.xsiSchemaLocationAttribute, service = obj@service,
					version = obj@version))
	
	# required and optional are mixed - schema requires a particular order:
	.offering <- xmlNode(name = "offering", namespace = sosNamespacePrefix,
			obj@offering)
	.xmlDoc <- addChildren(node = .xmlDoc, .offering)
	
	if(!any(is.na(obj@eventTime))) {
		.eventTimeList <- lapply(obj@eventTime, encodeXML, sos = sos,
				verbose = verbose)
		.xmlDoc <- addChildren(node = .xmlDoc, kids = .eventTimeList,
				append = TRUE)
	}
	
	if( !any(sapply(obj@procedure, "is.na"), na.rm = TRUE)) {
		.procedureList <- lapply(obj@procedure, "xmlNode",
				name="procedure", namespace = sosNamespacePrefix)
		.xmlDoc <- addChildren(node = .xmlDoc, kids = .procedureList,
				append = TRUE)
	}
	
	.observedProperties <- lapply(obj@observedProperty, "xmlNode",
			name="observedProperty", namespace = sosNamespacePrefix)
	.xmlDoc <- addChildren(node = .xmlDoc, kids = .observedProperties,
			append = TRUE)
	
	if( !is.null(obj@featureOfInterest)) {
		.foi <- encodeXML(obj = obj@featureOfInterest, sos = sos,
				verbose = verbose)
		.xmlDoc <- addChildren(node = .xmlDoc, kids = list(.foi),
				append = TRUE)
	}
	
	if( !is.null(obj@result)) {
		.result <- encodeXML(obj = obj@result, sos = sos, verbose = verbose)
		.xmlDoc <- addChildren(node = .xmlDoc, kids = list(.result),
				append = TRUE)
	}
	
	.responseFormat <- xmlNode(name = "responseFormat",
			namespace = sosNamespacePrefix,
			gsub(obj@responseFormat, pattern = "&quot;",
					replacement = '"'))
	.xmlDoc <- addChildren(node = .xmlDoc, kids = list(.responseFormat),
			append = TRUE)
	
	if( !is.na(obj@resultModel)) {
		.resultModel <- xmlNode(name = "resultModel",
				namespace = sosNamespacePrefix,
				obj@resultModel)
		.xmlDoc <- addChildren(node = .xmlDoc, kids = list(.resultModel),
				append = TRUE)
	}
	
	if( !is.na(obj@responseMode)) {
		.responseMode <- xmlNode(name = "responseMode",
				namespace = sosNamespacePrefix,
				obj@responseMode)
		.xmlDoc <- addChildren(node = .xmlDoc, kids = list(.responseMode),
				append = TRUE)
	}
	
	if( !is.na(obj@srsName)) {
		.xmlDoc <- addAttributes(.xmlDoc, srsName = obj@srsName, append = TRUE)
	}
	
	if( !is.na(obj@BBOX)) {
		warning("GetObservation contains BBOX, but that is not supported for 'POST' (and not at all in the SOS Specification...) - use featureOfInterest instead!")
	}
	
	return(.xmlDoc)
}

setMethod("encodeRequestXML", "GetObservationById", 
		function(obj, sos, verbose = FALSE) {
			if(verbose) {
				cat("ENCODE XML", class(obj), "\n")
			}
			
			if(obj@version == "1.0.0") {
				return(.sosEncodeRequestXMLGetObservationById_1.0.0(obj = obj,
								sos = sos))		
			}
			else {
				stop("Version not supported!")
			}
		}
)
.sosEncodeRequestXMLGetObservationById_1.0.0 <- function(obj, sos) {
	.xmlDoc <- xmlNode(name = "GetObservationById", namespace = sosNamespacePrefix,
			namespaceDefinitions = c(.sosNamespaceDefinitionsAll,
					.sosNamespaceDefinitionsGetObs),
			attrs=c(.xsiSchemaLocationAttribute,
					service = obj@service, version = obj@version))
	
	.obsId <- xmlNode(name = "ObservationId", namespace = sosNamespacePrefix,
			obj@observationId)
	.xmlDoc <- addChildren(node = .xmlDoc, .obsId)
	
	.responseFormat <- xmlNode(name = "responseFormat",
			namespace =  sosNamespacePrefix,
			gsub(obj@responseFormat, pattern = "&quot;",
					replacement = '"'))
	.xmlDoc <- addChildren(node = .xmlDoc, kids = list(.responseFormat),
			append = TRUE)
	
	if( !is.na(obj@resultModel)) {
		.resultModel <- xmlNode(name = "resultModel",
				namespace =  sosNamespacePrefix,
				obj@resultModel)
		.xmlDoc <- addChildren(node = .xmlDoc, kids = list(.resultModel),
				append = TRUE)
	}
	
	if( !is.na(obj@responseMode)) {
		.responseMode <- xmlNode(name = "responseMode",
				namespace =  sosNamespacePrefix,
				obj@responseMode)
		.xmlDoc <- addChildren(node = .xmlDoc, kids = list(.responseMode),
				append = TRUE)
	}
	
	if( !is.na(obj@srsName)) {
		.xmlDoc <- addAttributes(.xmlDoc, srsName = obj@srsName, append = TRUE)
	}
	
	return(.xmlDoc)
}

#
# encode for SOAP
#
setMethod("encodeRequestSOAP", "GetObservation", 
		function(obj, sos, verbose = FALSE) {
			if(verbose) {
				cat("ENCODE SOAP ", class(obj), "\n")
			}
			stop("Method not implemented yet!")
		}
)
setMethod("encodeRequestSOAP", "GetObservationById", 
		function(obj, sos, verbose = FALSE) {
			if(verbose) {
				cat("ENCODE SOAP ", class(obj), "\n")
			}
			stop("Method not implemented yet!")
		}
)

################################################################################
#
setMethod(f = "checkRequest",
		signature = signature(service = "SOS", operation = "GetObservation",
				verbose = "logical"),
		def = function(service, operation, verbose) {
			# check if operation is for SOS and operation is DescribeSensor
			if(!(operation@service == sosService && 
						operation@request == sosGetObservationName)) {
				stop("Wrong input! Require classes 'SOS' as service and 'GetObservation' as operation.")
				return(FALSE)
			}
			
			# TODO implement checkRequest for GetObservation
			
			# check if given responseFormat is supported by the service
			
			# check if temporal operator and operand are a valid combination according to filter capabilities
			
			return(TRUE)
		}
)

setMethod(f = "checkRequest",
		signature = signature(service = "SOS", operation = "GetObservationById",
				verbose = "logical"),
		def = function(service, operation, verbose) {
			# check if operation is for SOS and operation is DescribeSensor
			if(!(operation@service == sosService && 
						operation@request == sosGetObservationByIdName)) {
				stop("Wrong input! Require classes 'SOS' as service and 'GetObservationById' as operation.")
				return(FALSE)
			}
			
			# TODO implement checkRequest for GetObservationById
			# see above!
			
			return(TRUE)
		}
)
