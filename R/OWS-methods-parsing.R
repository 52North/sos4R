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
parseOwsOperation <- function(op) {
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
			else {
				# list of allowed values				
				.allowedValues <- sapply(
						getNodeSet(
								.p,
								"ows:AllowedValues/ows:Value",
								c(ows = "http://www.opengis.net/ows/1.1")
						),
						xmlValue)
				# list of ranges
				.ranges <-  sapply(
						getNodeSet(
								.p,
								"ows:AllowedValues/ows:Range",
								c(ows = "http://www.opengis.net/ows/1.1")
						),
						parseOwsRange)
				.allowedValuesAndRanges <- c(.allowedValues, .ranges)
			}
			
			.names <- c(.names, xmlGetAttr(.p, "name"))
			.parameters[[length(.parameters) + 1]] <- .allowedValuesAndRanges
		}
		
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

#
#
#
parseOwsServiceIdentification <- function(node) {
#	print("parsing ows service identification!")
	
	.children <- xmlChildren(node)
	.serviceType <- sapply(.filterXmlChildren(node, "ServiceType"),
			xmlValue)
	.serviceTypeVersion <- sapply(.filterXmlChildren(node,
					"ServiceTypeVersion"),
			xmlValue)
	.title <- sapply(.filterXmlChildren(node, "Title"),
			xmlValue)
	
	# optional:
	if(!is.na(xmlChildren(node)["Profile"]))
		.profile <- lapply(.filterXmlChildren(node, "Profile"), xmlValue)
	else .profile <- c(NA)
	
	if(!is.na(xmlChildren(node)["Abstract"]))
		.abstract <- lapply(.filterXmlChildren(node, "Abstract"), xmlValue)
	else .abstract <- c(NA)
	
	if(!is.na(xmlChildren(node)["Keywords"])) {
		.keywordLists <- .filterXmlChildren(node, "Keywords")
		.keywords <- c(lapply(.keywordLists, FUN = xmlToList), recursive = TRUE)
		.keywords <- lapply(.keywords, gsub, pattern = "^[[:space:]]+|[[:space:]]+$",
				replacement = "") # http://finzi.psych.upenn.edu/R/Rhelp02a/archive/40714.html
	}
	else .keywords <- c(NA)
	
	if(!is.na(xmlChildren(node)["Fees"]))
		.fees <- sapply(.filterXmlChildren(node, "Fees"), xmlValue)
	else .fees <- as.character(NA)
	
	if(!is.na(xmlChildren(node)["AccessConstraints"]))
		.accessConstraints <- lapply(.filterXmlChildren(node,
						"AccessConstraints"),
				xmlValue)
	else .accessConstraints <- c(NA)
	
	.si <- OwsServiceIdentification(serviceType =  .serviceType,
			serviceTypeVersion = .serviceTypeVersion, profile = .profile,
			title = .title, abstract = .abstract, keywords = .keywords,
			fees = .fees, accessConstraints = .accessConstraints)
}

#
#
#
parseOwsServiceProvider <- function(node) {
	#print("parsing ows service provider!")
	.name <- xmlValue(node[["ProviderName"]])
	
	# optional:
	if(!is.null(xmlChildren(node)[["ProviderSite"]]))
		.site <- xmlGetAttr(node = node[["ProviderSite"]],
				name = "href", default = as.character(NA))
	else .site <- as.character(NA)
	
	if(!is.null(xmlChildren(node)[["ServiceContact"]])) {
		.contact <- node[["ServiceContact"]]
		.sp <- OwsServiceProvider(providerName = .name, providerSite = .site,
				serviceContact = .contact)
	}
	else .sp <- OwsServiceProvider(providerName = .name, providerSite = .site)
	
	return(.sp)
}

#
# all elements are optional
#
parseOwsRange <- function(node) {
	.children <- xmlChildren(node)
	
	.minimumXml <- .children[["MinimumValue"]]
	if(is.null(.minimumXml)) {
		.minimum <- as.character(NA)
	} else {
		.minimum <- xmlValue(.minimumXml)
	}
	
	
	.maximumXml <- .children[["MaximumValue"]]
	if(is.null(.maximumXml)) {
		.maximum <- as.character(NA)
	} else {
		.maximum <- xmlValue(.maximumXml)
	}
	
	.closure <- xmlGetAttr(node = node, name = "rangeClosure")
	if(is.null(.closure)) {
		.closure <- as.character(NA)
	}
	
	.spacingXml <- .children[["Spacing"]]
	if(is.null(.spacingXml)) {
		.spacing <- as.character(NA)
	} else {
		.spacing <- xmlValue(.spacingXml)
	}
	
	.range <- OwsRange(minimumValue = .minimum, maximumValue = .maximum,
			rangeClosure = .closure, spacing = .spacing)
	
	return(.range)
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
