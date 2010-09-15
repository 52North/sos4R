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
	cat("; serviceTypeVersion(s): ")
	cat(paste(x@serviceTypeVersion, collapse = ", "))
	cat("; title(s): ")
	cat(paste(x@title, collapse = "; "))
	# optional:
	cat("\nProfile(s): ")
	cat(paste(x@profile, collapse = "; "))
	cat("\nAbstract(s): ")
	cat(paste(x@abstract, collapse = "; "))
	cat("\nKeywords(s): ")
	cat(paste(x@keywords, collapse = "; "))
	cat("\nAccessConstraints(s): ")
	cat(paste(x@accessConstraints, collapse = "; "))
	invisible(x)
}

print.OwsServiceProvider <- function(x, ...) {
	cat("Object of class OwsServiceProvider:\n")
	cat("Provider name: ")
	cat(x@providerName)
	cat("; providerSite: ")
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
	
#	if (is(x, "OwsCapabilities_1.1.0")) {
#		cat("\nIdentification: ")
#		print(x@identification)
#		cat("\nProvider: ")
#		print(x@provider)
#		cat("Operations (names): ")
#		cat(paste(names(x@operations@operations)))
#		cat("\nContents: ")
#		print(x@contents)
#	}
#	if (is(x, "OwsCapabilities_2.0.0")) {
#		cat("\nLanguages: ")
#		print(x@languages)
#	}
#	if (is(x, "SosCapabilities_1.1.0")) {
#		cat("\nFilter Capablities: ")
#		print(x@filterCaps)
#	}

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
	return(paste(.s, "\n"))
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
	cat("Object of class SOS -- version: ")
	cat(x@version)
	cat(", method: ")
	cat(x@method)
	cat(", url: ")
	cat(x@url)
	cat("\n\tCapabilities: ")
	print(x@capabilities)
	cat("\n")
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
	cat(", time class (inspect via @time): ")
	cat(class(x@time))
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

toString.SensorML <- function(x) {
	.s <- ("Object of class SensorML (wraps unparsed XML, see @xml for details).\n")
	return(.s)
}

print.SensorML <- function(x, ...) {
	cat(toString(x))
	invisible(x)
}

toString.GetObservation <- function(x) {
	.s <- paste("Object of class GetObservation: ",
			"service: ",
			x@service,
			", version: ",
			x@version,
			", offering: ",
			x@offering,
			"\nobservered property: ",
			x@observedProperty,
			"\nresponseFormat(s): ",
			x@responseFormat,
			", responseMode(s): ",
			paste(x@responseMode),
			# optionals:
			"\nprocedure(s)",
			paste(x@procedure),
			"\nfeature(s) of interest",
			paste(x@featureOfInterest),
			", event time: ",
			paste(x@eventTime),
			"\nresult: ",
			x@result,
			"\nsrsName: ",
			x@srsName,
			"\nevent time: ",
			x@eventTime,
			"\nresultModel(s): ",
			x@resultModel,
			"\nbounding box: ",
			paste(x@BBOX),
			"\n")
	return(.s)
}

print.GetObservation <- function(x, ...) {
	cat(toString(x))
	invisible(x)
}

toString.GetObservationById <- function(x) {
	.s <- paste("Object of class GetObservationById: ",
			"service: ",
			x@service,
			", version: ",
			x@version,
			"\nObsvervation ID: ",
			x@observationId,
			"\nResponseFormat(s): ",
			x@responseFormat,
			", responseMode(s): ",
			paste(x@responseMode),
			# optionals:
			", srsName: ",
			x@srsName,
			", resultModel(s): ",
			x@resultModel)
	return(.s)
}

print.GetObservationById <- function(x, ...) {
	cat(toString(x))
	invisible(x)
}


toString.DescribeSensor <- function(x) {
	.s <- paste("Object of class DescribeSensor: ",
			"service: ",
			x@service,
			", version: ",
			x@version,
			", outputFormat: ",
			x@outputFormat,
			"\nProcedure: ",
			x@procedure,
			"\n")
	return(.s)
}

print.DescribeSensor <- function(x, ...) {
	cat(toString(x))
	invisible(x)
}

toString.OmMeasurement <- function(x) {
	.s <- paste(
			"Object of class OmMeasurement",
			", procedure ",
			x@procedure,
			", observedProperty: ",
			x@observedProperty,
			", foi: ",
			x@featureOfInterest,
			"; samplingTime: ",
			toString(x@samplingTime),
			";\n\tresult: ",
			toString(x@result),
			"\n", sep = "")
	return(.s)
}

print.OmMeasurement <- function(x, ...) {
	cat(toString.OmMeasurement(x))
	invisible(x)
}

toString.OmObservation <- function(x) {
	.s <- paste(
			"Object of class OmObservation",
			", procedure ",
			toString(x@procedure),
			", observedProperty: ",
			toString(x@observedProperty),
			", foi: ",
			toString(x@featureOfInterest),
			"; samplingTime: ",
			toString(x@samplingTime),
			";\n\tresult: ",
			toString(x@result),
			sep = "")
	return(.s)
}

print.OmObservation <- function(x, ...) {
	cat(toString.OmObservation(x), "\n")
	invisible(x)
}

toString.OmMeasure <- function(x) {
	.s <- paste(
			"Object of class OmMeasure",
			"; value: ",
			x@value,
			"; uom: ",
			x@uom,
			sep = "")
	return(.s)
}

print.OmMeasure <- function(x, ...) {
	cat(toString.OmMeasure(x), "\n")
	invisible(x)
}

toString.SwePhenomenonProperty <- function(x) {
	.s <- paste("Object of class SwePhenomenonProperty",
			"; href: ",
			x@href,
			"; phenomenon: ",
			toString(x@phenomenon),
			sep = "")
	return(.s)
}

print.SwePhenomenonProperty <- function(x) {
	cat(toString.SwePhenomenonProperty(x), "\n")
	invisible(x)
}

print.SwePhenomenon <- function(x) {
	cat("Object of class SwePhenomenon",
		"; id: ",
		x@id,
		"; name: ",
		x@name,
		"; description: ",
		x@description,
		sep = "")
	invisible(x)
}

toString.SweCompositePhenomenon <- function(x) {
	.s <- paste("Object of class SweCompositePhenomenon",
		"; id: ",
		x@id,
		"; name: ",
		x@name,
		"; description: ",
		x@description,
		"; dimension: ",
		x@dimension,
		"; base: ",
		toString(x@base),
		";\ncomponents:\t",
		sapply(sapply(x@components, toString), paste, "\t\t\t"),
		sep = "")
	return(.s)
}

print.SweCompositePhenomenon <- function(x) {
	cat(toString.SweCompositePhenomenon(x), "\n")
	invisible(x)
}

toString.SweTextBlock <- function(x) {
	.s <- paste("Object of class SweTextBlock",
			" '",
			x@tokenSeparator,
			" ",
			x@blockSeparator,
			" ",
			x@decimalSeparator,
			"'; id: ",
			x@id,
			sep = "")
	return(.s)
}

print.SweTextBlock <- function(x) {
	cat(toString.SweTextBlock(x), "\n")
	invisible(x)
}

toString.GmlTimePosition <- function(x) {
	.s <- paste("Object of class GmlTimePosition",
			"; time: ",
			x@time,
			"; frame: ",
			x@frame,
			"; calendarEraName: ",
			x@calendarEraName,
			"; indeterminatePosition: ",
			x@indeterminatePosition,
			sep = "")
	return(.s)
}

print.GmlTimePosition <- function(x) {
	cat(toString.GmlTimePosition(x))
	invisible(x)
}

toString.GmlTimeInstant <- function(x) {
	.s <- paste("Object of class GmlTimeInstant",
			"; timePosition: ",
			toString(x@timePosition),
			sep = "")
	return(.s)
}

print.GmlTimeInstant <- function(x) {
	cat(toString.GmlTimeInstant(x))
	invisible(x)
}

toString.GmlTimeInstantProperty <- function(x) {
	.s <- paste("Object of class GmlTimeInstantProperty",
			"; href: ",
			x@href,
			"; time: ",
			x@time,
			sep = "")
	return(.s)
}

print.GmlTimeInstantProperty <- function(x) {
	cat(toString.GmlTimeInstantProperty(x))
	invisible(x)
}

toString.GmlTimeInterval <- function(x) {
	.s <- paste("Object of class GmlTimeInterval",
			"; interval: ",
			x@interval,
			"; unit: ",
			x@unit,
			"; radix: ",
			x@radix,
			"; factor: ",
			x@factor,
			sep = "")
	return(.s)
}

print.GmlTimeInterval <- function(x) {
	cat(toString.GmlTimeInterval(x))
	invisible(x)
}

toString.GmlTimePeriod <- function(x) {
	.s <- paste("Object of class GmlTimePeriod",
			"; duraction: ",
			x@duration,
			", timeInterval: ",
			x@timeInterval,
			";\n\tbegin: ",
			toString(x@begin),
			" -- end: ",
			toString(x@end),
			";\n\tbeginPosition: ",
			toString(x@beginPosition),
			" -- endPosition: ",
			toString(x@endPosition),
			sep = "")
	return(.s)
}

print.GmlTimePeriod <- function(x) {
	cat(toString.GmlTimePeriod(x))
	invisible(x)
}

toString.GmlFeatureProperty <- function(x) {
	.s <- paste("Object of class GmlFeatureProperty",
			", href: ",
			x@href,
			", feature: ",
			x@feature,
			"\n", sep = "")
	return(.s)
}

print.GmlFeatureProperty <- function(x) {
	cat(toString.GmlFeatureProperty(x))
	invisible(x)
}

toString.GmlDirectPosition <- function(x) {
	.s <- paste("Object of class GmlDirectPosition",
			"; pos: ",
			x@pos,
			"; srsName: ",
			x@srsName,
			", srsDimension: ",
			x@srsDimension,
			", srsDimension: ",
			x@axisLabels,
			", uomLabels: ",
			x@uomLabels,
			"\n", sep = "")
	return(.s)
}

print.GmlDirectPosition <- function(x) {
	cat(toString.GmlDirectPosition(x))
	invisible(x)
}

toString.GmlPoint <- function(x) {
	.s <- paste("Object of class GmlPoint",
			"; pos: ",
			toString(x@pos),
			";\nsrsName: ",
			x@srsName,
			", srsDimension: ",
			x@srsDimension,
			", srsDimension: ",
			x@axisLabels,
			", uomLabels: ",
			x@uomLabels,
			"\n", sep = "")
	return(.s)
}

print.GmlPoint <- function(x) {
	cat(toString.GmlPoint(x))
	invisible(x)
}

toString.GmlPointProperty <- function(x) {
	.s <- paste("Object of class GmlPointProperty",
			"; href: ",
			x@href,
			"; point: ",
			toString(x@point),
			"\n", sep = "")
	return(.s)
}

print.GmlPointProperty <- function(x) {
	cat(toString.GmlPointProperty(x))
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

setMethod("show", "SaSamplingPoint", function(object) print.SaSamplingPoint(object))
setMethod("show", "SaSamplingSurface", function(object) print.SaSamplingSurface(object))

setMethod("show", "SwePhenomenon", function(object) print.SwePhenomenon(object))
setMethod("show", "SwePhenomenonProperty", function(object) print.SwePhenomenonProperty(object))
setMethod("show", "SweCompositePhenomenon", function(object) print.SweCompositePhenomenon(object))
setMethod("show", "SweTextBlock", function(object) print.SweTextBlock(object))

setMethod("show", "OmObservation", function(object) print.OmObservation(object))
setMethod("show", "OmObservationProperty", function(object) print.OmObservationProperty(object))
setMethod("show", "OmMeasure", function(object) print.OmMeasure(object))
setMethod("show", "OmMeasurement", function(object) print.OmMeasurement(object))

# no show, print and toString fucntions for VIRTUAL classes
setMethod("show", "GmlTimePosition", function(object) print.GmlTimePosition(object))
setMethod("show", "GmlTimeInstant", function(object) print.GmlTimeInstant(object))
setMethod("show", "GmlTimeInterval", function(object) print.GmlTimeInterval(object))
setMethod("show", "GmlTimePeriod", function(object) print.GmlTimePeriod(object))
setMethod("show", "GmlFeatureProperty", function(object) print.GmlFeatureProperty(object))
setMethod("show", "GmlDirectPosition", function(object) print.GmlDirectPosition(object))
setMethod("show", "GmlPoint", function(object) print.GmlPoint(object))
setMethod("show", "GmlPointProperty", function(object) print.GmlPointProperty(object))

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
