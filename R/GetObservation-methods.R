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
# encode as KVP
#
setMethod("kvp", "GetObservation", 
	function(obj) {

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
		
		print(.mandatory)
		
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
			.optionals <- paste(.optionals, paste("result", 
							.kvpEscapeSpecialCharacters(obj@result), sep="="),
					sep="&")
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
		
		cat(.kvpString); cat("\n")
		
		return(.kvpString)
	}
)

#
# encode as XML
#
setMethod("encode", "GetObservation", 
	function(obj) {
		xmlDoc <- xmlNode(name = "GetObservation", namespace = "sos",
				namespaceDefinitions = c(
						"sos" = "http://www.opengis.net/sos/1.0",
						"ows" = "http://www.opengis.net/ows/1.1",
						"ogc" = "http://www.opengis.net/ogc",
						"xsi" = "http://www.w3.org/2001/XMLSchema-instance"),
						attrs=c("xsi:schemaLocation" = "http://www.opengis.net/sos/1.0 http://schemas.opengis.net/sos/1.0.0/sosGetCapabilities.xsd",
						service=obj@service))

		# mandatory:
		
		# optional:
		
		return(xmlDoc)
	}
)


# decode from XML
setMethod("decode", "GetObservation", 
	function(obj) {
		warning("Function decode is not implemented for GetObservation!")
	}
)


# TODO implement summary function
summary.GetObservation = function(object, ...) {
	obj = list()
#	obj[["class"]] = class(object)
#	obj[["proj4string"]] = object@proj4string@projargs
#	if (is(object, "SpatialPoints"))
#		obj[["npoints"]] = nrow(object@coords)
#	if (is(object, "SpatialGrid") || is(object, "SpatialPixels"))
#		obj[["grid"]] = gridparameters(object)
#	class(obj) = "summary.Spatial"
	obj
}
setMethod("summary", "GetObservation", summary.GetObservation)

# saveXML(gc, file="/tmp/_testsave.xml")
setMethod("saveXML", "GetObservation",
	function(doc, file=NULL, compression=0, indent=TRUE, prefix = '<?xml version="1.0"?>\n', doctype = NULL, encoding = "", ...) {
		saveXML(doc = encode(doc), file=file, compression=compression, indent=indent, prefix=prefix, doctype=doctype, encoding=encoding, ...)
	}
)

