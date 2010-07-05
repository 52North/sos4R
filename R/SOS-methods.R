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
SOS <- function(url, method = "GET", version = "1.0.0") {
	.sos <- new("SOS", url = url, method = method, version = version,
			capabilities = new("OwsCapabilities", version = "NA", 
					updateSequence = as.character(NA),
					owsVersion = "1.1.0")
			)
	.caps <- getCapabilities(.sos)
	.sos@capabilities <- .caps
	return(.sos)
}

#
#
#
if (!isGeneric("sosRequest"))
	setGeneric(name = "sosRequest", def = function(sos, request, verbose = FALSE)
				standardGeneric("sosRequest"))
setMethod("sosRequest", "SOS", 
	function(sos, request, verbose = TRUE) { # TODO change verbose default to FALSE
		.response = ""
		
		if(sos@method == "GET") {
			if(verbose) {
				print(paste("get! url: ", sos@url))
			}
			
			.kvpEncoding = kvp(request)
			.url = paste(sos@url, .kvpEncoding, sep="?")
			.response = getURL(.url)
			#print(response)
		}
		else if(sos@method == "POST") {
			if(verbose) print("post!")
				
			# TODO implement POST stuff
			.xmlEncoding <- encode(request)
			
			print(.xmlEncoding)
			
			.response = "NOT IMPLEMENTED!"
				
		}
		else if(sos@method == "SOAP") {
			if(verbose) print("soap!")
			
			# TODO implement SOAP stuff
			
		}
		else {
			warning("Unsupported method, has to be one of GET, POST, or SOAP!")
		}

		# TODO exception handling!
		
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
	setGeneric(name = "sosCaps", def = function(sos)
				standardGeneric("sosCaps"))
setMethod("sosCaps", "SOS", 
		function(sos) {
			return(sos@capabilities)
		}
)

if (!isGeneric("sosUrl"))
	setGeneric(name = "sosUrl", def = function(sos)
				standardGeneric("sosUrl"))
setMethod("sosUrl", "SOS", 
		function(sos) {
			return(sos@url)
		})

if (!isGeneric("sosVersion"))
	setGeneric(name = "sosVersion", def = function(sos)
				standardGeneric("sosVersion"))
setMethod("sosVersion", "SOS", 
		function(sos) {
			return(sos@version)
		})

if (!isGeneric("sosMethod"))
	setGeneric(name = "sosMethod", def = function(sos)
				standardGeneric("sosMethod"))
setMethod("sosMethod", "SOS", 
		function(sos) {
			return(sos@method)
		})

if (!isGeneric("sosProcedures"))
	setGeneric(name = "sosProcedures", def = function(sos)
				standardGeneric("sosProcedures"))
setMethod("sosProcedures", "SOS", 
		function(sos) {
			.caps <- sosCaps(sos)
			.ds <- .caps@operations@operations[["DescribeSensor"]]
			return(.ds@parameters$procedure)
		})

if (!isGeneric("sosObservedProperties"))
	setGeneric(name = "sosObservedProperties", def = function(sos)
				standardGeneric("sosObservedProperties"))
setMethod("sosObservedProperties", "SOS", 
		function(sos) {
			.caps <- sosCaps(sos)
			.getOb <- .caps@operations@operations[["GetObservation"]]
			return(.getOb@parameters$observedProperty)
		})

if (!isGeneric("sosOfferings"))
	setGeneric(name = "sosOfferings", def = function(sos)
				standardGeneric("sosOfferings"))
setMethod("sosOfferings", "SOS", 
		function(sos) {
			.offerings <- sos@capabilities@contents@observationOfferings
			return(.offerings)
		})
if (!isGeneric("sosOfferingIds"))
	setGeneric(name = "sosOfferingIds", def = function(sos)
				standardGeneric("sosOfferingIds"))
setMethod("sosOfferingIds", "SOS", 
		function(sos) {
			return(names(sosOfferings(sos)))
		})

if (!isGeneric("sosFOIs"))
	setGeneric(name = "sosFOIs", def = function(sos)
				standardGeneric("sosFOIs"))
setMethod("sosFOIs", "SOS", 
		function(sos) {
			.caps <- sosCaps(sos)
			
			# via GetFeatureOfInterest
			.gfoi <- .caps@operations@operations[["GetFeatureOfInterest"]]
			if(!is.null(.gfoi)) {
				return(.gfoi@parameters$featureOfInterestId)
			}
			else return("GetFeatureOfInterest-Operation not supported!")
				
			# via contents section, list flatting does NOT work yet
#			.fois <- c(sapply(.caps@contents@observationOfferings, 
#							slot, "featureOfInterest"))
#			.fois <- 
#			names(.fois) <- NULL # TODO remove duplicates
				
			return(.fois)
		})

# sosKeywords, sosTitle/sosAbstract

################################################################################
# functions for SOS operations

if (!isGeneric("getCapabilities"))
	setGeneric(name = "getCapabilities", def = function(sos)
				standardGeneric("getCapabilities"))
#
#
#
setMethod("getCapabilities", "SOS", 
		function(sos) {
			.gc <- OwsGetCapabilities(service = "SOS", acceptVersions = c(sos@version))
			
			.responseString = sosRequest(sos = sos, request = .gc)
			.response <- xmlParseDoc(.responseString, asText = TRUE)
			
			if(.isExceptionReport(.response)) {
				print("Received ExceptionReport in getCapabilities!")
				.er <- parseOwsExceptionReport(.response)
				return(.er)
			}
			else {
				.caps <- .parseSosCapabilities(.response)
				return(.caps)
			}
		}
)


if (!isGeneric("describeSensor"))
	setGeneric(name = "describeSensor", def = function(sos, procedure)
				standardGeneric("describeSensor"))
#
#
#
setMethod("describeSensor", "SOS", 
		function(sos, procedure) {
			.ds <- DescribeSensor(service = "SOS", version = sos@version,
					procedure = procedure)
			
			.responseString = sosRequest(sos = sos, request = .ds)
			.response <- xmlParseDoc(.responseString, asText = TRUE)
			
			if(.isExceptionReport(.response)) {
				print("Received ExceptionReport in describeSensor!")
				.er <- parseOwsExceptionReport(.response)
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
			def = function(sos, offering, observedProperty, responseFormat)
				standardGeneric("getObservation"))
#
#
#
setMethod("getObservation", "SOS", 
		function(sos, offering, observedProperty, responseFormat) {
			.go <- GetObservation(service = "SOS", version = sos@version, 
					offering = offering, observedProperty =  observedProperty,
					responseFormat =  responseFormat)
			
			.responseString = sosRequest(sos = sos, request = .go)
			.response <- xmlParseDoc(.responseString, asText = TRUE)
			
			if(.isExceptionReport(.response)) {
				print("Received ExceptionReport in describeSensor!")
				.er <- parseOwsExceptionReport(.response)
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
#

#
#
#
SosFilter_Capabilities <- function(xmlNode) {
	new("SosFilter_Capabilities", xml = xmlNode)
}

#
# construction function
#
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

#
# construction function
#
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

#
# construction function
#
SosContents <- function(observationOfferings) {
	new("SosContents", observationOfferings = observationOfferings)
}

#
#
#
.parseSosObservationOffering <- function(ob) {
	.id <- xmlGetAttr(ob, "id")
	.name <- xmlValue(ob[["name"]])
	
	.time <- list(beginPosition = xmlValue(
					ob[["time"]][["TimePeriod"]][["beginPosition"]]),
			endPosition = xmlValue(
					ob[["time"]][["TimePeriod"]][["endPosition"]]))
	
	.procedure <- sapply(ob["procedure"], xmlGetAttr, "href")
	.observedProperty <- sapply(ob["observedProperty"], xmlGetAttr, "href")
	.featureOfInterest <- sapply(ob["featureOfInterest"], xmlGetAttr, "href")
	
	.responseFormat <- sapply(ob["responseFormat"], xmlValue)
	.resultModel <- sapply(ob["resultModel"], xmlValue)
	.responseMode <- sapply(ob["responseMode"], xmlValue)
	
	.boundedBy <- list(
			srsName = xmlGetAttr(ob[["boundedBy"]][["Envelope"]], "srsName"),
			lowerCorner = xmlValue(ob[["boundedBy"]][["Envelope"]][["lowerCorner"]]),
			upperCorner = xmlValue(ob[["boundedBy"]][["Envelope"]][["upperCorner"]]))
	
	.ob <- SosObservationOffering(id = .id, name = .name, 
			time = .time, procedure = .procedure,
			observedProperty = .observedProperty,
			featureOfInterest = .featureOfInterest,
			responseFormat = .responseFormat, resultModel = .resultModel,
			responseMode = .responseMode, boundedBy = .boundedBy)
	
	return(.ob)
}

#
#
#
.parseSosCapabilities <- function(document) {
	.caps.root <- xmlRoot(document)
	
	# attributes:
	.caps.attrs <- xmlAttrs(.caps.root)
	.caps.version <- .caps.attrs[["version"]]
	if(any(sapply(names(.caps.attrs), "==", "updateSequence")))
		.caps.updateSequence <- .caps.attrs[["updateSequence"]]
	else .caps.updateSequence <- as.character(NA)
	
	# as xml only: service identification, service provider and filter capabilities 
	.caps.si <- OwsServiceIdentification(.caps.root[["ServiceIdentification"]])
	.caps.sp <- OwsServiceProvider(.caps.root[["ServiceProvider"]])
	.caps.fc <- SosFilter_Capabilities(.caps.root[["Filter_Capabilities"]])
	
	# parsed: operations and contents section
	.operationsXML <- .filterXmlChildren(.caps.root[["OperationsMetadata"]],
			"Operation")
	.operations <- sapply(.operationsXML, .parseOwsOperation)
	# add names for indexing of list
	names(.operations) <- lapply(.operations,
			function(obj) {
				return(obj@name)
			})
	
	.caps.om <- OwsOperationsMetadata(operations = .operations)
	
	.observationsXML <- .filterXmlChildren(.caps.root[["Contents"]][["ObservationOfferingList"]], "ObservationOffering")
	.observations = sapply(.observationsXML, .parseSosObservationOffering)
	# add names to list
	names(.observations) <- lapply(.observations,
			function(obj) {
				return(obj@id)
			})
	
	
	.caps.contents <- SosContents(observationOfferings = .observations)
	
	.capabilities <- SosCapabilities(version = .caps.version,
			updateSequence = .caps.updateSequence,
			identification = .caps.si,
			provider = .caps.sp,
			operations = .caps.om,
			filterCaps = .caps.fc,
			contents = .caps.contents)
	
	return(.capabilities)
}

