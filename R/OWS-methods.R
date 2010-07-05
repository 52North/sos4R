################################################################################
# Copyright (C) 2010 by 52 North											   #
# Initiative for Geospatial Open Source Software GmbH						   #
# 																			   #
# Contact: Andreas Wytzisk													   #
# 52 North Initiative for Geospatial Open Source Software GmbH				   #
# Martin-Luther-King-Weg 24													   #
# 48155 Muenster, Germany													   #
# info@52north.org															   #
#																			   #
# This program is free software; you can redistribute and/or modify it under   #
# the terms of the GNU General Public License version 2 as published by the    #
# Free Software Foundation.													   #
#																			   #
# This program is distributed WITHOUT ANY WARRANTY; even without the implied   #
# WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU #
# General Public License for more details.									   #
#																			   #
# You should have received a copy of the GNU General Public License along with #
# this program (see gpl-2.0.txt). If not, write to the Free Software		   #
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or #
# visit the Free Software Foundation web page, http://www.fsf.org.			   #
#																			   #
# Author: Daniel Nüst (daniel.nuest@uni-muenster.de)                           #
# Created: 2010-06-18														   #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #                                              #
#                                                                              #
################################################################################

#
#
#
OwsGetCapabilities <- function(
		service,
		acceptVersions,
		sections = c("All"),
		acceptFormats = c("text/xml"),
		updateSequence = c(as.character(NA)),
		owsVersion = "1.1.0",
		acceptLanguages = c(as.character(NA))) {
	if(owsVersion == "1.1.0") {
		if(!any(sapply(acceptLanguages, "is.na"), na.rm = TRUE))
			warning("Parameter 'acceptLanguages' is lost because it is not included in 1.1.0!")
		new("OwsGetCapabilities_1.1.0", service = service,
				acceptVersions = acceptVersions, sections = sections,
				acceptFormats = acceptFormats, updateSequence = updateSequence)
	}
	else if(owsVersion == "2.0.0") {
		new("OwsGetCapabilities_2.0.0", service = service,
				acceptVersions = acceptVersions, sections = sections,
				acceptFormats = acceptFormats, updateSequence = updateSequence,
				acceptLanguages = acceptLanguages)
	}
	else {
		new("OwsGetCapabilities", service = service,
				acceptVersions = acceptVersions, owsVersion = owsVersion)
	}
}

#
#
#
OwsCapabilities <- function(
		version, 
		updateSequence = NA,
		owsVersion = "1.1.0",
		identification,
		provider,
		operations,
		contents,
		languages = NA) {
	if(owsVersion == "1.1.0") {
		if(!is.na(languages))
			warning("Parameter 'languages' is lost because it is not included in 1.1.0!")
		new("OwsCapabilities_1.1.0",
			version = version, updateSequence = updateSequence,
			owsVersion = owsVersion,
			identification = identification,
			provider = provider, operations = operations,
			contents = contents)
	}
	else if(owsVersion == "2.0.0") {
		new("OwsCapabilities_2.0.0",
			version = version, updateSequence = updateSequence,
			owsVersion = owsVersion,
			identification = identification,
			provider = provider, operations = operations,
			contents = contents, languages = languages)
	}
	else {
		new("OwsCapabilities",
			version = version, updateSequence = updateSequence,
			owsVersion = owsVersion)
	}	
}

#
# construction methods for capabilities elements that are only XML
#
OwsServiceIdentification <- function(xmlNode) {
	new("OwsServiceIdentification", xml = xmlNode)
}
OwsServiceProvider <- function(xmlNode) {
	new("OwsServiceProvider", xml = xmlNode)
}

#
# construction functions
#
OwsOperationsMetadata <- function(operations, parameters = c(NA),
	constraints = c(NA), extendedCapabilities = xmlNode(NA)) {
	new("OwsOperationsMetadata", operations = operations,
		parameters = parameters, constraints = constraints,
		extendedCapabilities = extendedCapabilities)
}
OwsOperation <- function(name, DCPs, parameters = c(NA), constraints = c(NA),
			metadata = c(NA)) {
	new("OwsOperation", name = name, DCPs = DCPs, parameters = parameters,
		constraints = constraints, metadata = metadata)
}

#
# construction function
#
OwsContents <- function(xmlNode) {
	new("OwsContents", xml = xmlNode)
}

#
# helper method to add (possible) multiple values
#
.kvpKeyAndValues <- function(key, values) {
	if(is(values, "vector")) {
		.values <- sapply(values, .kvpEscapeSpecialCharacters)
		valueList <- paste(.values, collapse=",")
		return(paste(key, valueList, sep="="))
	}
	else {
		return(paste(key, .kvpEscapeSpecialCharacters(value), sep="="))
	}
}

#
# http://www.ietf.org/rfc/rfc2396.txt 
# via
# http://www.oostethys.org/best-practices/best-practices-get
#
# Special character  	Escaped encoding
# :					 	%3A
# / 					%2F
# # 					%23
# ? 					%3F
# = 					%3D
#
.kvpEscapeSpecialCharacters <- function(valueString) {
#	print(valueString)
	.escaped <- gsub(valueString, pattern = ":", replacement = "%3A")
	#.escaped <- gsub(.escaped, pattern = "/", replacement = "%2F")
	.escaped <- gsub(.escaped, pattern = "#", replacement = "%23")
	.escaped <- gsub(.escaped, pattern = "\\?", replacement = "%3F")
	.escaped <- gsub(.escaped, pattern = "=", replacement = "%3D")
#	print(.escaped)
	return(.escaped)
}

#
#
#
.filterXmlChildren <- function(node, childrenName, includeNamed = TRUE) {
	.temp <- xmlChildren(node)
	.filtered <- c()
	for (.x in .temp) {
		if(xmlName(.x) == childrenName && includeNamed)
			.filtered = c(.filtered, .x)
		else if(!includeNamed && xmlName(.x) != childrenName)
			.filtered = c(.filtered, .x) 
	}
	rm(.temp)
	return(.filtered)
}

#
#
#
.parseOwsOperation <- function(op) {
	.name <- xmlGetAttr(op, "name")
	
	.dcpsXML <- .filterXmlChildren(op, "DCP")
	.dcps <- c()
	for(.dcp in .dcpsXML) {
		.http <- .dcp[["HTTP"]]
		.endpoints <- c(
			.filterXmlChildren(.http, "Get"),
			.filterXmlChildren(.http, "Post"))
		
		for(.ep in .endpoints) {
			.newEndpoint <- c(xmlGetAttr(.ep, "href"))
			names(.newEndpoint) <- xmlName(.ep)
			.dcps <- c(.dcps, .newEndpoint)
		}
	}
	
	.parametersXML <- .filterXmlChildren(op, "Parameter")
	if(length(.parametersXML) > 0) {
		.parameters = list()
		.names = list()
		
		
		for(.p in .parametersXML) {
			# put all alowed values in list
			# check for ows:AnyValue	
			if(length(.p["AnyValue"]) > 0)
				.allowedValues = c("AnyValue")
			else 
				.allowedValues <- sapply(
					getNodeSet(
						.p,
						"ows:AllowedValues/ows:Value",
						c(ows = "http://www.opengis.net/ows/1.1")
						),
					xmlValue)
			
			.names <- c(.names, xmlGetAttr(.p, "name"))
			.parameters[[length(.parameters) + 1]] <- .allowedValues
		}
		
		#print("parameters")
		#print(.parameters)
		#print("names:")
		#print(.names)
		names(.parameters) <- .names
	}
	
	if(any(sapply(names(op), "==", "constraints")))
		warning("constraint elements are NOT processed!")
	.constraints = c(NA)
	
	if(any(sapply(names(op), "==", "metadata")))
		warning("metadata elements are NOT processed!")
	.metadata = c(NA)
	
	.op <- OwsOperation(name = .name, DCPs = .dcps,
			parameters = .parameters, constraints = .constraints,
			metadata = .metadata)
	return(.op)
}

#
#
#
setMethod("kvp", "OwsGetCapabilities", 
		function(obj) kvp.getCapabilities(obj))
setMethod("kvp", "OwsGetCapabilities_1.1.0", 
		function(obj) kvp.getCapabilities_1.1.0(obj))
setMethod("kvp", "OwsGetCapabilities_2.0.0", 
		function(obj) kvp.getCapabilities_2.0.0(obj))

#
#
#
kvp.getCapabilities <- function(obj) {
#	print("kvp.GetCapabilities")
	
	.service <- paste(
			"service",
			.kvpEscapeSpecialCharacters(obj@service),
			sep="=")
	.request <- paste(
			"request",
			.kvpEscapeSpecialCharacters(obj@request),
			sep="=")
	
	.kvpString <- paste(.service, .request, sep="&")
	
	return(.kvpString)
}

kvp.getCapabilities_1.1.0 <- function(obj) {
#	print("kvp.GetCapabilities_1.0.0")
	
	.mandatory <- kvp.getCapabilities(obj)
	
	.optionals = ""
	if( !is.na(obj@acceptVersions)) {
		.optionals <- paste(.optionals, .kvpKeyAndValues("acceptVersions", obj@acceptVersions), sep="&")
	}
	
	if(!any(sapply(obj@sections, "is.na"), na.rm = TRUE)) {
		.optionals <- paste(.optionals, .kvpKeyAndValues("sections", obj@sections), sep="&")
	}
	
	if( !is.na(obj@updateSequence)) {
		.optionals <- paste(.optionals, .kvpKeyAndValues("updateSequence", obj@updateSequence), sep="&")
	}
	
	if(!any(sapply(obj@acceptFormats, "is.na"), na.rm = TRUE)) {
		.optionals <- paste(.optionals, .kvpKeyAndValues("acceptFormats", obj@acceptFormats), sep="&")
	}
	
	.kvpString <- paste(.mandatory, .optionals, sep="")
	
	return(.kvpString)
}

kvp.getCapabilities_2.0.0 <- function(obj) {
#	print("kvp.GetCapabilities_2.0.0")
	
	.kvpString <- kvp.getCapabilities_1.1.0(obj)
	
	if(!any(sapply(obj@acceptLanguages, "is.na"), na.rm = TRUE)) {
		.kvpString <- paste(.kvpString, .kvpKeyAndValues("acceptLanguages", obj@acceptLanguages), sep="&")
	}
	
	return(.kvpString)
}


#
# encode as XML
#
setMethod("encode", "OwsGetCapabilities_1.1.0", 
		function(obj) {
			print("encode gc 1.1.0")
			
			xmlDoc <- xmlNode(name = "GetCapabilities", namespace = "sos",
					namespaceDefinitions = c(
							"sos" = "http://www.opengis.net/sos/1.0",
							"ows" = "http://www.opengis.net/ows/1.1",
							"ogc" = "http://www.opengis.net/ogc",
							"xsi" = "http://www.w3.org/2001/XMLSchema-instance"),
					attrs=c("xsi:schemaLocation" = "http://www.opengis.net/sos/1.0 http://schemas.opengis.net/sos/1.0.0/sosGetCapabilities.xsd",
							service=obj@service))
			
			# optional:
			if( !is.na(obj@acceptVersions)) {
				acceptVersions <- xmlNode(name = "AcceptVersions", namespace = "ows")
				acceptVersions$children <- lapply(
						obj@acceptVersions, "xmlNode", name="ows:Version")
				xmlDoc$children[[1]] <- acceptVersions
			}
			
			if(!any(sapply(obj@sections, "is.na"), na.rm = TRUE)) {
				sections <- xmlNode("ows:Sections")
				sections$children <- lapply(obj@sections, "xmlNode", name="Section", namespace="ows")
				xmlDoc$children[[2]] <- sections
			}
			
			if( !is.na(obj@updateSequence)) {
				xmlDoc <- addAttributes(xmlDoc, updateSequence = obj@updateSequence)
			}
			
			if(!any(sapply(obj@acceptFormats, "is.na"), na.rm = TRUE)) {
				acceptFormats <- xmlNode(name = "AcceptFormats", namespace = "ows")
				acceptFormats$children <- lapply(
						obj@acceptFormats, "xmlNode", name="ows:OutputFormat")
				xmlDoc$children[[3]] <- acceptFormats
			}
			
			return(xmlDoc)
		}
)


# decode from XML
setMethod("decode", "OwsGetCapabilities", 
		function(obj) {
			return("Function decode is not implemented for OwsGetCapabilities!")
		}
)


# saveXML(gc, file="/tmp/_testsave.xml")
setMethod("saveXML", "OwsGetCapabilities",
		function(doc, file=NULL, compression=0, indent=TRUE, prefix = '<?xml version="1.0"?>\n', doctype = NULL, encoding = "", ...) {
			saveXML(doc = encode(doc), file=file, compression=compression, indent=indent, prefix=prefix, doctype=doctype, encoding=encoding, ...)
		}
)


#
# Get the meaning of an OWS exception code
#
.codes = c(
		"OperationNotSupported",
		"MissingParameterValue",
		"InvalidParameterValue",
		"VersionNegotiationFailed",
		"InvalidUpdateSequence",
		"OptionNotSupported",
		"NoApplicableCode")
.meanings = c(
		"Request is for an operation that is not supported by this server",
		"Operation request does not include a parameter value, and this server did not declare a default parameter value for that parameter",
		"Operation request contains an invalid parameter value",
		"List of versions in 'AcceptVersions' parameter value in GetCapabilities operation request did not include any version supported by this server",
		"Value of (optional) updateSequence parameter in GetCapabilities operation request is greater than current value of service metadata updateSequence number",
		"Request is for an option that is not supported by this server",
		"No other exceptionCode specified by this service and server applies to this exception")
.locators = c(
		"Name of operation not supported",
		"Name of missing parameter",
		"Name of parameter with invalid value",
		"None, omit 'locator' parameter",
		"None, omit 'locator' parameter",
		"Identifier of option not supported",
		"None, omit 'locator' parameter")
.httpCode = c("501", "400", "400", "400", "400", "501", "3xx, 4xx, 5xx")
.httpMessage = c("Not Implemented", "Bad request", "Bad request", "Bad request",
		"Bad request", "Not implemented", "Internal Server Error")

OWS_STANDARD_EXCEPTIONS <- data.frame(
		exceptionCode = .codes,
		meaningOfCode = .meanings, 
		locator = .locators,
		httpStatusCode = .httpCode,
		httpMessage = .httpMessage,
		check.rows = TRUE, check.names = TRUE)

if (!isGeneric("owsMeaningOfCode"))
	setGeneric(name = "owsMeaningOfCode", def = function(exceptionCode)
				standardGeneric("owsMeaningOfCode"))
setMethod("owsMeaningOfCode", "character",
	def = function(exceptionCode) {
		.meaning <- as.character(
				OWS_STANDARD_EXCEPTIONS[
						OWS_STANDARD_EXCEPTIONS$exceptionCode=="OperationNotSupported",
						2])
		return(.meaning)
	}
)

#
#
#
OwsExceptionReport <- function(version, lang = as.character(NA), exceptions = list()) {
	new("OwsExceptionReport", version = version, lang = lang,
			exceptions = exceptions)
}

#
#
#
OwsException <- function(exceptionCode, exceptionText = c(),
		locator = as.character(NA)) {
	new("OwsException", exceptionCode = exceptionCode,
			exceptionText = exceptionText, 
			locator = locator)
}

#
# method for parsing an ows:ExceptionReport.
#
parseOwsExceptionReport <- function(document) {
#	print("parsing er!")
	
	.docRoot <- xmlRoot(document)
	## print(.docRoot)
	
	.attrs <- xmlAttrs(.docRoot)
	.version <- .attrs["version"]
	if(!is.null(.attrs["lang"]) && !is.na(.attrs["lang"]))
		.lang <- .attrs["lang"]
	else .lang <- as.character(NA)
	
	# remove all elements from docRoot that are not 'Exception'
	# could probably be done nicer with subsetting, but indexing with wildcards or similar (... xmlChildren()[[]] ...) did not work.
	.children <- xmlChildren(.docRoot) 
	.exceptionsXML <- list()
	for (x in .children) {
		if(xmlName(x) == "Exception")
			.exceptionsXML = c(.exceptionsXML, x)
		# else print(xmlName(x))
	}
	
	.exceptions = sapply(.exceptionsXML, parseOwsException)
	.report <- OwsExceptionReport(version = .version, lang = .lang, exceptions = .exceptions)
	
	return(.report)
}

#
# parsing a single xml node that is an ows:Exception
#
parseOwsException <- function(node) {
#	print("parsing e!")
	.attrs <- xmlAttrs(node)
	.code <- .attrs[["exceptionCode"]]
	if(!is.null(.attrs["locator"]) && !is.na(.attrs["locator"]))
		.locator <- .attrs[["locator"]]
	else .locator <- as.character(NA)
	
	if(!is.na(xmlChildren(node)["ExceptionText"]))
		.text <- xmlValue(xmlChildren(node)[["ExceptionText"]])
	else .text <- as.character(NA)
	
	.exception <- OwsException(exceptionCode = .code, 
			exceptionText = .text,
			locator = .locator)
	
	return(.exception)
}

