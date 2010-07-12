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
# PRINTING FUNCTIONS
print.OwsServiceOperation <- function(x, ...) {
	cat("Object of class OwsServiceOperation\n")
	cat("service: ")
	cat(x@service)
	cat(", version: ")
	cat(x@version)
	cat(", request: ")
	cat(x@request)
	invisible(x)
}

print.OwsGetCapabilities <- function(x, ...) {
	cat("Object of class OwsGetCapabilities\n")
	cat("service: ")
	cat(x@service)
	cat(", owsVersion: ")
	cat(x@owsVersion)
	cat(", acceptVersions: ")
	cat(x@acceptVersions)

	if (is(x, "OwsGetCapabilities_1.1.0")) {
		cat(", sections: ")
		cat(x@sections)
		cat(", acceptFormats: ")
		cat(x@acceptFormats)
		cat(", updateSequence: ")
		cat(x@updateSequence)
	}
	
	if (is(x, "OwsGetCapabilities_2.0.0")) {
		cat(", acceptLanguages: ")
		cat(x@acceptLanguages)
	}
	
	cat(", request: ")
	cat(x@request)
	cat("\n")
	invisible(x)
}

print.OwsOperationsMetadata <- function(x, ...) {
	cat("Object of class OwsOperationsMetadata\n")
	cat("operations:\n")
	print(x@operations)
	cat("parameters:\n")
	print(x@parameters)
	cat(",\nextendedCapabilities:\n")
	print(x@extendedCapabilities)
	invisible(x)
}

print.OwsOperation <- function(x, ...) {
	cat("Object of class OwsOperation\n")
	cat("Name: ")
	cat(x@name)
	cat("\nParameters (names): ")
	cat(paste(names(x@parameters)))
	cat("\nDCPs (types): ")
	print(paste(names(x@DCPs)))
	cat("Constraints: ")
	print(x@constraints)
	cat("Metadata: ")
	print(x@metadata)
	invisible(x)
}

print.OwsServiceIdentification <- function(x, ...) {
	cat("Object of class OwsServiceIdentification")
	cat(":\nServiceType: ")
	cat(x@serviceType)
	cat(", serviceTypeVersion(s): ")
	cat(x@serviceTypeVersion)
	cat(", title(s): ")
	cat(x@title)
	# optional:
	cat("\nProfile(s): ")
	cat(x@profile)
	cat("\nAbstract(s): ")
	cat(x@abstract)
	cat("\nKeywords(s): ")
	cat(x@keywords)
	cat("\nAccessConstraints(s): ")
	cat(x@accessConstraints)
	invisible(x)
}

print.OwsServiceProvider <- function(x, ...) {
	cat("Object of class OwsServiceProvider:\n")
	cat("Provider name: ")
	cat(x@providerName)
	cat(", providerSite: ")
	cat(x@providerSite)
	cat("\nService contact:  (unparsed XML, see @serviceContact for details)")
#	print(x@serviceContact)
	invisible(x)
}

print.OwsContents <- function(x, ...) {
	cat("Object of class OwsContents (wraps unparsed XML, see @xml for details).\n")
#	print("xml:")
#	print(x@xml)
	invisible(x)
}

print.OwsCapabilities <- function(x, ...) {
	cat("Object of class ")
	cat(class(x))
	cat(" -- version: ")
	cat(x@version)
	cat(", owsVersion: ")
	cat(x@owsVersion)
	cat(", updateSequence: ")
	cat(x@updateSequence)
	
	if (is(x, "OwsCapabilities_1.1.0")) {
		cat("\nIdentification: ")
		print(x@identification)
		cat("\nProvider: ")
		print(x@provider)
		cat("Operations (names): ")
		cat(paste(names(x@operations@operations)))
		cat("\nContents: ")
		print(x@contents)
	}
	if (is(x, "OwsCapabilities_2.0.0")) {
		cat("\nLanguages: ")
		print(x@languages)
	}
	if (is(x, "SosCapabilities_1.1.0")) {
		cat("\nFilter Capablities: ")
		print(x@filterCaps)
	}
	
	invisible(x)
}

toString.OwsExceptionReport <- function(x) {
	.s <- paste(
			"Object of class OwsExceptionReport",
			"; version: ",
			x@version,
			", lang: ",
			x@lang,
			paste(", ", length(x@exceptions),
					"exceptions: (code @ locator : text)"),
			sep = "")
	for (e in x@exceptions) {
		.e <- paste("\t", e@exceptionCode, " @ ", e@locator, " : ",
				e@exceptionText, sep = "")
		.s <- paste(.s, .e, sep = "\n")
	}
	return(.s)
}

print.OwsExceptionReport <- function(x, ...) {
	cat(toString.OwsExceptionReport(x))
	invisible(x)
}

print.OwsException <- function(x, ...) {
	cat("Object of class OwsException")
	cat("; exception code: ")
	cat(x@exceptionCode)
	cat(", locator: ")
	cat(x@locator)
	cat("\nException text(s):\n\t")
	print(paste(x@exceptionText, sep = "\n\t"))
	invisible(x)
}

print.OwsRange <- function(x, ...) {
	cat("Object of class OwsRange")
	cat("; spacing: ")
	cat(x@spacing)
	cat(", rangeClosure: ")
	cat(x@rangeClosure)
	cat("\nFROM ")
	cat(x@minimumValue)
	cat(" TO ")
	cat(x@maximumValue)
	invisible(x)
}

print.SOS <- function(x, ...) {
	cat("Object of class SOS")
	cat("; version: ")
	cat(x@version)
	cat(", method: ")
	cat(x@method)
	cat(", url: ")
	cat(x@url)
	cat("\nCapabilities: ")
	print(x@capabilities)
	invisible(x)
}

print.SosFilter_Capabilities <- function(x, ...) {
	cat("Object of class SosFilter_Capabilities (wraps unparsed XML, see @xml for details).\n")
#	print("xml:")
#	print(x@xml)
	invisible(x)
}

print.SosObservationOffering <- function(x, ...) {
	cat("Object of class SosObservationOffering: ")
	cat("id: ")
	cat(x@id)
	cat(", name: ")
	cat(x@name)
	cat(", time: ")
	cat(paste(x@time$beginPosition, " - ", x@time$endPosition))
	cat("\nprocedure(s): ")
	cat(x@procedure)
	cat("\nobservedProperty(s): ")
	cat(x@observedProperty)
	cat("\nfeature(s)OfInterest: ")
	cat(x@featureOfInterest)
	cat("\nresponseFormat(s): ")
	cat(x@responseFormat)
	cat(", responseMode(s): ")
	cat(paste(x@responseMode))
	cat("\nintendedApplication: ")
	cat(x@intendedApplication)
	cat("\nresultModel(s): ")
	cat(x@resultModel)
	cat("\nboundedBy: ")
	cat(paste(x@boundedBy))
	invisible(x)
}

print.SosContents <- function(x, ...) {
	cat("Object of class SosContents with observation offerings (names): ")
	cat(paste(names(x@observationOfferings)))
	invisible(x)
}

print.SensorML <- function(x, ...) {
	cat("Object of class SensorML (wraps unparsed XML, see @xml for details).\n")
#	print("xml:")
#	print(x@xml)
	invisible(x)
}

print.GetObservation <- function(x, ...) {
	cat("Object of class GetObservation: ")
	cat("service: ")
	cat(x@service)
	cat(", version: ")
	cat(x@version)
	cat(", offering: ")
	cat(x@offering)
	cat("\nobservered property: ")
	cat(x@observedProperty)
	cat("\nresponseFormat(s): ")
	cat(x@responseFormat)
	cat(", responseMode(s): ")
	cat(paste(x@responseMode))
	# optionals:
	cat("\nprocedure(s)")
	cat(paste(x@procedure))
	cat("\nfeature(s) of interest")
	cat(paste(x@featureOfInterest))
	cat(", event time: ")
	cat(paste(x@eventTime))
	cat("\nresult: ")
	cat(x@result)
	cat("\nsrsName: ")
	cat(x@srsName)
	cat("\nevent time: ")
	cat(x@eventTime)
	cat("\nresultModel(s): ")
	cat(x@resultModel)
	cat("\nbounding box: ")
	cat(paste(x@BBOX))
	invisible(x)
}

print.GetObservationById <- function(x, ...) {
	cat("Object of class GetObservationById: ")
	cat("service: ")
	cat(x@service)
	cat(", version: ")
	cat(x@version)
	cat("\nObsvervation ID: ")
	cat(x@observationId)
	cat("\nResponseFormat(s): ")
	cat(x@responseFormat)
	cat(", responseMode(s): ")
	cat(paste(x@responseMode))
	# optionals:
	cat(", srsName: ")
	cat(x@srsName)
	cat(", resultModel(s): ")
	cat(x@resultModel)
	invisible(x)
}


print.DescribeSensor <- function(x, ...) {
	cat("Object of class DescribeSensor: ")
	cat("service: ")
	cat(x@service)
	cat(", version: ")
	cat(x@version)
	cat(", outputFormat: ")
	cat(x@outputFormat)
	cat("\nProcedure: ")
	cat(x@procedure)
	cat("\n")
	invisible(x)
}


################################################################################
# SHOW FUNCTIONS
setMethod("show", "OwsServiceOperation", function(object) print.OwsServiceOperation(object))
setMethod("show", "OwsGetCapabilities", function(object) print.OwsGetCapabilities(object))
setMethod("show", "OwsGetCapabilities_1.1.0", function(object) print.OwsGetCapabilities(object))
setMethod("show", "OwsGetCapabilities_2.0.0", function(object) print.OwsGetCapabilities(object))
setMethod("show", "OwsOperationsMetadata", function(object) print.OwsOperationsMetadata(object))
setMethod("show", "OwsOperation", function(object) print.OwsOperation(object))
setMethod("show", "OwsServiceIdentification", function(object) print.OwsServiceIdentification(object))
setMethod("show", "OwsServiceProvider", function(object) print.OwsServiceProvider(object))
setMethod("show", "OwsContents", function(object) print.OwsContents(object))
setMethod("show", "OwsCapabilities", function(object) print.OwsCapabilities(object))
setMethod("show", "OwsCapabilities_1.1.0", function(object) print.OwsCapabilities(object))
setMethod("show", "OwsCapabilities_2.0.0", function(object) print.OwsCapabilities(object))
setMethod("show", "OwsExceptionReport", function(object) print.OwsExceptionReport(object))
setMethod("show", "OwsException", function(object) print.OwsException(object))
setMethod("show", "OwsRange", function(object) print.OwsRange(object))
setMethod("show", "SOS", function(object) print.SOS(object))
setMethod("show", "SosFilter_Capabilities", function(object) print.SosFilter_Capabilities(object))
setMethod("show", "SosObservationOffering", function(object) print.SosObservationOffering(object))
setMethod("show", "SosContents", function(object) print.SosContents(object))
setMethod("show", "SensorML", function(object) print.SensorML(object))
setMethod("show", "GetObservation", function(object) print.GetObservation(object))
setMethod("show", "GetObservationById", function(object) print.GetObservationById(object))
setMethod("show", "DescribeSensor", function(object) print.DescribeSensor(object))

################################################################################
# SUMMARY FUNCTIONS
summary.OwsGetCapabilities <- function(object, ...) {
	obj = list()
	obj[["class"]] = class(object)
	obj[["service"]] = object@service
	obj[["owsVersion"]] = object@owsVersion
	obj[["request"]] = object@request
	
	if (is(object, "OwsGetCapabilities_1.1.0")) {
		obj[["sections"]] = object@sections
		obj[["acceptFormats"]] = object@acceptFormats
		obj[["updateSequence"]] = object@updateSequence
	}
		
	if (is(object, "OwsGetCapabilities_2.0.0")) {
		obj[["acceptLanguages"]] = object@acceptLanguages
	}
	
	class(obj) = "summary.OwsGetCapabilities"
	return(obj)
}
setMethod("summary", "OwsGetCapabilities", summary.OwsGetCapabilities)

# missing: OwsServiceOperation, OwsGetCapabilities

################################################################################
# STR FUNCTIONS
