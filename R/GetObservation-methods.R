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
GetObservation <- function(
		service,
		version,
		offering, 
		observedProperty,
		responseFormat, 
		srsName = as.character(NA),
		eventTime = as.character(NA), 
		procedure = c(NA),
		featureOfInterest = c(NA), 
		result = as.character(NA),
		resultModel = as.character(NA),
		responseMode = as.character(NA),
		BBOX = as.character(NA)) {
	new("GetObservation",
			request = "GetObservation",
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
			request = "GetObservationById",
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
	function(obj, verbose = FALSE) {
		if(obj@version == "1.0.0") {
			return(sosEncodeRequestKVPGetObservation_1.0.0(obj, verbose))		
		}
		else {
			warning("Version not supported!")
		}
	}
)
sosEncodeRequestKVPGetObservation_1.0.0 <- function(obj, verbose = FALSE) {
	# required:
	.request <- "request=GetObservation"
	.service <- paste("service",
			.kvpEscapeSpecialCharacters(obj@service), sep="=")
	.version <- paste("version",
			.kvpEscapeSpecialCharacters(obj@version), sep="=")
	.offering <- paste("offering",
			.kvpEscapeSpecialCharacters(obj@offering), sep="=")
	.observedProperty <- .kvpKeyAndValues("observedProperty", 
			obj@observedProperty)
	.responseFormat <- paste(
			"responseFormat", 
			.kvpEscapeSpecialCharacters(
					gsub(obj@responseFormat, pattern = "&quot;",
							replacement = '"')),
			sep="=")
	.mandatory <- paste(.service, .request, .version, .offering,
			.observedProperty, .responseFormat, sep="&")
	
	# optional:
	.optionals = ""
	if( !is.na(obj@srsName)) {
		.optionals <- paste(.optionals, paste("srsName", 
						.kvpEscapeSpecialCharacters(obj@srsName), sep="="),
				sep="&")
	}
	
	if( !is.na(obj@eventTime)) {
		.optionals <- paste(.optionals, paste("eventTime", 
						.kvpEscapeSpecialCharacters(obj@eventTime), 
						sep="="), 
				sep="&")
	}
	
	if( !any(sapply(obj@procedure, "is.na"), na.rm = TRUE)) {
		.optionals <- paste(.optionals, .kvpKeyAndValues("procedure", obj@procedure), sep="&")
	}
	
	if( !any(sapply(obj@featureOfInterest, "is.na"), na.rm = TRUE)) {
		.optionals <- paste(.optionals, .kvpKeyAndValues("featureOfInterest", obj@featureOfInterest), sep="&")
	}
	
	if( !is.na(obj@result)) {
		warning("GetObservation contains result, but that is not supported for 'GET' - parameter is discarded, use another method to include it!")
		
	}
	
	if( !is.na(obj@resultModel)) {
		.optionals <- paste(.optionals, paste("resultModel",
						.kvpEscapeSpecialCharacters(obj@resultModel),
						sep="="),
				sep="&")
	}
	
	if( !is.na(obj@responseMode)) {
		.optionals <- paste(.optionals, paste("responseMode",
						.kvpEscapeSpecialCharacters(obj@responseMode),
						sep="="),
				sep="&")
	}
	
	if( !is.na(obj@BBOX)) {
		.optionals <- paste(.optionals, paste("BBOX", 
						.kvpEscapeSpecialCharacters(obj@BBOX), sep="="),
				sep="&")
	}
	
	.kvpString <- paste(.mandatory, .optionals, sep="")
	
	if(verbose)
		cat(.kvpString)
	
	return(.kvpString)
}

setMethod("encodeRequestKVP", "GetObservationById", 
		function(obj, verbose = TRUE) {
			warning("KVP encoding of operation 'GetObservationById' not supported!")
		}
)

#
# encode as XML
#
setMethod("encodeRequestXML", "GetObservation", 
	function(obj, verbose = FALSE) {
		if(verbose) {
			cat("ENCODE XML ", class(obj), "\n")
		}
		
		if(obj@version == "1.0.0") {
			return(sosEncodeRequestXMLGetObservation_1.0.0(obj))		
		}
		else {
			warning("Version not supported!")
		}
	}
)
sosEncodeRequestXMLGetObservation_1.0.0 <- function(obj) {
	.xmlDoc <- xmlNode(name = "GetObservation", namespace = "sos",
			namespaceDefinitions = c(
					"sos" = "http://www.opengis.net/sos/1.0",
					"ows" = "http://www.opengis.net/ows/1.1",
					"om" = "http://www.opengis.net/om/1.0", # required for resultModel values
					"ogc" = "http://www.opengis.net/ogc",
					"xsi" = "http://www.w3.org/2001/XMLSchema-instance"),
			attrs=c("xsi:schemaLocation" = "http://www.opengis.net/sos/1.0 http://schemas.opengis.net/sos/1.0.0/sosAll.xsd",
					service = obj@service, version = obj@version))
	
	# required and optional are mixed as schema requires a particular order:
	.offering <- xmlNode(name = "offering", namespace = "sos", obj@offering)
	.xmlDoc <- addChildren(node = .xmlDoc, .offering)
	
	if( !is.na(obj@eventTime)) {
		# .eventTimes <- .eventTimeNodeList(obj@eventTime)
		# TODO model and implement
	}
	
	if( !any(sapply(obj@procedure, "is.na"), na.rm = TRUE)) {
		.procedureList <- lapply(obj@procedure, "xmlNode",
				name="procedure", namespace = "sos")
		.xmlDoc <- addChildren(node = .xmlDoc, kids = .procedureList,
				append = TRUE)
	}
	
	.observedProperties <- lapply(obj@observedProperty, "xmlNode",
			name="observedProperty", namespace = "sos")
	.xmlDoc <- addChildren(node = .xmlDoc, kids = .observedProperties,
			append = TRUE)
	
	if( !any(sapply(obj@featureOfInterest, "is.na"), na.rm = TRUE)) {
		# TODO add foi with ogc:spatialOps and sos:ObjectID
	}
	
	if( !is.na(obj@result)) {
		# TODO add result with ogc:comparisonOps
	}
	
	.responseFormat <- xmlNode(name = "responseFormat", namespace = "sos",
			gsub(obj@responseFormat, pattern = "&quot;",
					replacement = '"'))
	.xmlDoc <- addChildren(node = .xmlDoc, kids = list(.responseFormat),
			append = TRUE)
	
	if( !is.na(obj@resultModel)) {
		.resultModel <- xmlNode(name = "resultModel", namespace = "sos",
				obj@resultModel)
		.xmlDoc <- addChildren(node = .xmlDoc, kids = list(.resultModel),
				append = TRUE)
	}
	
	if( !is.na(obj@responseMode)) {
		.responseMode <- xmlNode(name = "responseMode", namespace = "sos",
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
		function(obj, verbose = FALSE) {
			if(verbose) {
				cat("ENCODE XML ", class(obj), "\n")
			}
			
			if(obj@version == "1.0.0") {
				return(sosEncodeRequestXMLGetObservationById_1.0.0(obj))		
			}
			else {
				warning("Version not supported!")
			}
		}
)
sosEncodeRequestXMLGetObservationById_1.0.0 <- function(obj) {
	.xmlDoc <- xmlNode(name = "GetObservationById", namespace = "sos",
			namespaceDefinitions = c(
					"sos" = "http://www.opengis.net/sos/1.0",
					"ows" = "http://www.opengis.net/ows/1.1",
					"om" = "http://www.opengis.net/om/1.0", # required for resultModel values
					"ogc" = "http://www.opengis.net/ogc",
					"xsi" = "http://www.w3.org/2001/XMLSchema-instance"),
			attrs=c("xsi:schemaLocation" = "http://www.opengis.net/sos/1.0 http://schemas.opengis.net/sos/1.0.0/sosAll.xsd",
					service = obj@service, version = obj@version))
	
	.obsId <- xmlNode(name = "ObservationId", namespace = "sos", obj@observationId)
	.xmlDoc <- addChildren(node = .xmlDoc, .obsId)
	
	.responseFormat <- xmlNode(name = "responseFormat", namespace = "sos",
			gsub(obj@responseFormat, pattern = "&quot;",
					replacement = '"'))
	.xmlDoc <- addChildren(node = .xmlDoc, kids = list(.responseFormat),
			append = TRUE)
	
	if( !is.na(obj@resultModel)) {
		.resultModel <- xmlNode(name = "resultModel", namespace = "sos",
				obj@resultModel)
		.xmlDoc <- addChildren(node = .xmlDoc, kids = list(.resultModel),
				append = TRUE)
	}
	
	if( !is.na(obj@responseMode)) {
		.responseMode <- xmlNode(name = "responseMode", namespace = "sos",
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
		function(obj, verbose = FALSE) {
			if(verbose) {
				cat("ENCODE SOAP ", class(obj), "\n")
			}
			
			if(obj@version == "1.0.0") {
				return(sosEncodeRequestXMLDescribeSensor_1.0.0(obj))
			}
			else {
				warning("Version not supported!")
			}
		}
)

################################################################################
#
setMethod(f = "checkRequest",
		signature = c(service = "ANY", operation = "GetObservation",
				verbose = "logical"),
		def = function(service, operation, verbose) {
			# check if operation is for SOS and operation is DescribeSensor
			if(!(operation@service == "SOS" && 
						operation@request == "GetObservation")) {
				warning("Wrong input! Require classes 'SOS' as service and 'GetObservation' as operation.")
				return(FALSE)
			}
			
			# TODO add useful checks for GetObservation
			
			# check if given responseFormat is supported by the service
			
			return(TRUE)
		})

setMethod(f = "checkRequest",
		signature = c(service = "ANY", operation = "GetObservationById",
				verbose = "logical"),
		def = function(service, operation, verbose) {
			# check if operation is for SOS and operation is DescribeSensor
			if(!(operation@service == "SOS" && 
						operation@request == "GetObservationById")) {
				warning("Wrong input! Require classes 'SOS' as service and 'GetObservationById' as operation.")
				return(FALSE)
			}
			
			# TODO add useful checks for GetObservationById
			# see above!
			
			return(TRUE)
		})
