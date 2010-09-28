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
DescribeSensor <- function(
		service,
		version,
		procedure,
		outputFormat) {
	new("DescribeSensor",
			request = sosDescribeSensorName,
			service = service,
			version = version,
			procedure = procedure,
			outputFormat = outputFormat)
}


#
# see: http://www.oostethys.org/best-practices/best-practices-get
#
setMethod("encodeRequestKVP", "DescribeSensor", 
		function(obj, sos, verbose = FALSE) {
			
			if(obj@version == "1.0.0") {
				return(sosEncodeRequestKVPDescribeSensor_1.0.0(obj = obj,
								sos = sos, verbose = verbose))
			}
			else {
				stop("Version not supported!")
			}
		}
)
sosEncodeRequestKVPDescribeSensor_1.0.0 <- function(obj, sos, verbose = FALSE) {
	# mandatory:
	.service <- paste("service",
			.kvpEscapeSpecialCharacters(x = obj@service), sep = "=")
	.request <- paste("request" , sosDescribeSensorName, sep = "=")
	.version <- paste("version", 
			.kvpEscapeSpecialCharacters(x = obj@version), sep = "=")
	.procedure <- paste("procedure",
			.kvpEscapeSpecialCharacters(x = obj@procedure), sep = "=")
	.format <- paste(
			"outputFormat",
			.kvpEscapeSpecialCharacters(x = gsub(obj@outputFormat,
							pattern = "&quot;",
							replacement = '"')),
			sep = "=")
	
	.kvpString <- paste(.service, .request, .version, .procedure,
			.format, sep = "&")
	
	if(verbose)
		cat(.kvpString)
	
	return(.kvpString)
}

#
# encode as XML
#
setMethod("encodeRequestXML", "DescribeSensor", 
		function(obj, sos, verbose = FALSE) {
			if(verbose) {
				cat("ENCODE XML", class(obj), "\n")
			}
			
			if(obj@version == "1.0.0") {
				return(sosEncodeRequestXMLDescribeSensor_1.0.0(obj = obj))
			}
			else {
				stop("Version not supported!")
			}
		}
)
sosEncodeRequestXMLDescribeSensor_1.0.0 <- function(obj) {
	xmlDoc <- xmlNode(name = sosDescribeSensorName,
			namespace = sosNamespacePrefix,
			namespaceDefinitions = .sosNamespaceDefinitionsAll,
			attrs=c(.xsiSchemaLocationAttribute,
					service = obj@service,
					outputFormat = obj@outputFormat,
					version = obj@version))
	
	procedure <- xmlNode(name = "procedure", namespace = sosNamespacePrefix,
			obj@procedure)
	xmlDoc$children[[1]] <- procedure
	
	return(xmlDoc)
}

#
# encode for SOAP
#
setMethod("encodeRequestSOAP", "DescribeSensor", 
		function(obj, sos, verbose = FALSE) {
			if(verbose) {
				cat("ENCODE SOAP ", class(obj), "\n")
			}
			
			if(obj@version == "1.0.0") {
				return(sosEncodeRequestXMLDescribeSensor_1.0.0(obj))
			}
			else {
				stop("Version not supported!")
			}
		}
)


################################################################################
#
setMethod(f = "checkRequest",
		signature = signature(service = "SOS", operation = "DescribeSensor",
				verbose = "logical"),
		def = function(service, operation, verbose) {
			if(verbose) {
				cat("Checking DescribeSensor... ")
			}
			
			# check if operation is for SOS and operation is DescribeSensor
			if(!(operation@service == sosService && 
						operation@request == sosDescribeSensorName)) {
				stop("Wrong input! Require classes 'SOS' as service and 'DescribeSensor' as operation.")
				return(FALSE)
			}
				
			# check if sensor in in listed in procedures
			.procedures = sosProcedures(service)
			.dsOperation <- sosOperation(service, sosDescribeSensorName)

			.procContained <- FALSE
			for (x in .procedures) {
				if(x == operation@procedure)
					.procContained <- TRUE
			}
			if(!.procContained)
				warning("Requested procedure ist not listed in capablities, service might return error!")
			
			
			# check if output format is supported by sos
			.oFSupported <- FALSE
			.supportedFormats <- .dsOperation@parameters[["outputFormat"]];
			.format <- gsub(operation@outputFormat, pattern = "&quot;",
					replacement = '"')
			
			if(!any(sapply(.supportedFormats,
							"==",
							.format),
					na.rm = TRUE)) {
				stop(paste("outputformat has to be one of",
								paste(.supportedFormats, sep = ", ",
										collapse = " ")))
			}
			else {
				.oFSupported <- TRUE
			}
			
			# check if method is supported
			.methodSupported <- FALSE
			if(service@method == .sosConnectionMethodPost) {
				if(!is.na(.dsOperation@DCPs["Post"]))
					.methodSupported <- TRUE
			}
			else if(service@method == .sosConnectionMethodGet) {
				if(!is.na(.dsOperation@DCPs["Get"]))
					.methodSupported <- TRUE
			}
			if(!.methodSupported)
				warning("Requested method type ist not listed in capablities for this operation, service might return error!")
			
			if(verbose) {
				cat("Checks: procedure contained=", .procContained,
						", output supported=", .oFSupported,
						", method supported", .methodSupported, "\n")
			}
			
			return(.procContained && .oFSupported && .methodSupported)
		})
