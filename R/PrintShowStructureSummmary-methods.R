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

toString.OwsGetCapabilities <- function(x) {
	.s <- paste("Object of class OwsGetCapabilities; service: ",
		x@service, ", request: ", x@request,
		", owsVersion: ", x@owsVersion,
		", acceptVersions: ", toString(x@acceptVersions), sep = " ")

	if (is(x, "OwsGetCapabilities_1.1.0")) {
		.s <- paste(.s, "\n\t sections: ", toString(x@sections),
			", acceptFormats: ", toString(x@acceptFormats),
			", updateSequence: ", x@updateSequence, sep = " ")
			
		if (is(x, "OwsGetCapabilities_2.0.0")) {
			.s <- paste(.s, "\n\tacceptLanguages: ", 
				x@acceptLanguages, sep = " ")
		}
	}
	
	return(.s)
}


toString.OwsGetCapabilities_1.1.0 <- function(x) {
	return(toString.OwsGetCapabilities(x))
}

print.OwsGetCapabilities <- function(x, ...) {
	cat(toString.OwsGetCapabilities(x))
	invisible(x)
}

print.OwsGetCapabilities_1.1.0 <- function(x, ...) {
	cat(toString.OwsGetCapabilities(x))
	invisible(x)
}

print.OwsOperationsMetadata <- function(x, ...) {
	cat("Object of class OwsOperationsMetadata:\n")
	cat("-- operations:\n")
	print(x@operations)
	cat("-- parameters:\n")
	print(x@parameters)
	cat("-- extendedCapabilities:\n")
	print(x@extendedCapabilities)
	invisible(x)
}

print.OwsOperation <- function(x, ...) {
	cat("Object of class OwsOperation: ")
	cat("Name: ")
	cat(x@name)
	cat("\n\tParameters (names): ")
	cat(paste(names(x@parameters)))
	cat("\n\tDCPs (types): ")
	cat(paste(names(x@DCPs)))
	cat("\n\tConstraints: ")
	cat(toString(x@constraints))
	cat("\n\tMetadata: ")
	print(toString(x@metadata))
	invisible(x)
}

print.OwsServiceIdentification <- function(x, ...) {
	cat("Object of class OwsServiceIdentification:")
	cat("\n\tServiceType: ")
	cat(x@serviceType)
	cat("; serviceTypeVersion(s): ")
	cat(paste(x@serviceTypeVersion, collapse = ", "))
	cat("\n\ttitle(s): ")
	cat(paste(x@title, collapse = "; "))
	# optional:
	cat("\n\tProfile(s): ")
	cat(paste(x@profile, collapse = "; "))
	cat("\n\tAbstract(s): ")
	cat(paste(x@abstract, collapse = "; "))
	cat("\n\tKeywords(s): ")
	cat(paste(x@keywords, collapse = "; "))
	cat("\n\tAccessConstraints(s): ")
	cat(paste(x@accessConstraints, collapse = "; "))
	cat("\n")
	invisible(x)
}

print.OwsServiceProvider <- function(x, ...) {
	cat("Object of class OwsServiceProvider:")
	cat("\n\tProvider name: ")
	cat(x@providerName)
	cat("; providerSite: ")
	cat(x@providerSite)
	cat("\n\tService contact:  (unparsed XML, see @serviceContact for details)")
	cat("\n")
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
	
	cat("\n")
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
					"exceptions (code @ locator : text):"),
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

toString.SosFilter_Capabilities <- function(x) {
	.s <- paste("Object of class SosFilter_Capabilities;",
			"\n\tSpatial_Capabilities:\t",
			toString(x@spatial[[ogcGeometryOperandsName]]),
			";",
			toString(x@spatial[[ogcSpatialOperatorsName]]),
			"\n\tTemporal_Capablities:\t",
			toString(x@temporal[[ogcTemporalOperandsName]]),
			";",
			toString(x@spatial[[ogcTemporalOperatorsName]]),
			"\n\tScalar_Capablities:\t\t",
			toString(x@scalar),
			"\n\tId_Capabilities\t\t\t",
			toString(x@id))
	return(.s)
}

print.SosFilter_Capabilities <- function(x, ...) {
	cat(toString.SosFilter_Capabilities(x), "\n")
	invisible(x)
}

print.SosObservationOffering <- function(x, ...) {
	cat("Object of class SosObservationOffering; ")
	cat("id: ")
	cat(x@id)
	cat(", name: ")
	cat(x@name)
	cat("\n\ttime: ")
	cat(.addTabIndent(toString(x@time)))
	cat("\n\tprocedure(s): ")
	cat(paste(x@procedure))
	cat("\n\tobservedProperty(s): ")
	cat(paste(x@observedProperty))
	cat("\n\tfeature(s)OfInterest: ")
	cat(paste(x@featureOfInterest))
	cat("\n\tresponseFormat(s): ")
	cat(paste(x@responseFormat))
	cat(", responseMode(s): ")
	cat(paste(x@responseMode))
	cat("\n\tintendedApplication: ")
	cat(paste(x@intendedApplication))
	cat("\n\tresultModel(s): ")
	cat(paste(x@resultModel))
	cat("\n\tboundedBy: ")
	cat(paste(x@boundedBy))
	invisible(x)
}

print.SosContents <- function(x, ...) {
	cat("Object of class SosContents with observation offerings (names): ")
	cat(paste(names(x@observationOfferings)))
	invisible(x)
}

toString.SosEventTime <- function(x) {
	.s <- paste("Object of class SosEventTime: ",
			class(x@temporalOps),": ",
			toString(x@temporalOps@time), sep = "")
	return(.s)
}

print.SosEventTime <- function(x) {
	cat(toString.SosEventTime(x), "\n")
	invisible(x)
}

toString.SosEventTimeLatest <- function(x) {
	.s <- paste("Object of class SosEventTimeLatest; temporalOps value:",
			x@temporalOps)
	return(.s)
}

print.SosEventTime <- function(x) {
	cat(toString.SosEventTimeLatest(x), "\n")
	invisible(x)
}

toString.SosFeatureOfInterest <- function(x) {
	.s <- paste("Object of class SosFeatureOfInterest",
			";\n\tobjectIDs: ",
			toString(x@objectIDs),
			";\n\tspatialOps: ",
			toString(x@spatialOps),
			sep = "")
	return(.s)
}

print.SosFeatureOfInterest <- function(x) {
	cat(toString.SosFeatureOfInterest(x), "\n")
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
			"\n\tfeature(s) of interest",
			toString(x@featureOfInterest),
			"\n\tevent time: ",
			toString(x@eventTime),
			"\n\tresult: ",
			class(x@result),
			"\nsrsName: ",
			x@srsName,
			"\nresultModel(s): ",
			x@resultModel)
	return(.s)
}

print.GetObservation <- function(x, ...) {
	cat(toString(x), "\n")
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
	cat(toString(x), "\n")
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
			x@procedure)
	return(.s)
}

print.DescribeSensor <- function(x, ...) {
	cat(toString(x), "\n")
	invisible(x)
}

toString.OmMeasurement <- function(x) {
	.s <- paste(
			"Object of class OmMeasurement",
			", procedure ",
			toString(x@procedure),
			", observedProperty: ",
			toString(x@observedProperty),
			";\n\tfeatureOfInterest: ",
			toString(x@featureOfInterest),
			";\n\tsamplingTime: ",
			toString(x@samplingTime),
			";\n\tresult: ",
			toString(x@result),
			sep = "")
	return(.s)
}

print.OmMeasurement <- function(x, ...) {
	cat(toString.OmMeasurement(x), "\n")
	invisible(x)
}

toString.OmObservation <- function(x) {
	.s <- paste(
			"Object of class OmObservation",
			"; procedure: ",
			toString(x@procedure),
			"\n\tobservedProperty: ",
			toString(x@observedProperty),
			"\n\tfoi: ",
			toString(x@featureOfInterest),
			"\n\tsamplingTime: ",
			.addTabIndent(toString(x@samplingTime)),
			"\n\tresult dimensions: ",
			toString(dim(x@result)),
			sep = "")
	return(.s)
}

print.OmObservation <- function(x, ...) {
	cat(toString.OmObservation(x), "\n")
	invisible(x)
}

toString.OmObservationProperty <- function(x) {
	.s <- paste("Object of class OmObservationProperty",
			"; href: ",
			x@href,
			"; observation: ",
			toString(x@obs),
			sep = "")
	return(.s)
}

print.OmObservationProperty <- function(x) {
	cat(toString.OmObservationProperty(x), "\n")
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

toString.SwePhenomenon <- function(x) {
	.s <- paste("Object of class SwePhenomenon",
		"; id: ",
		x@id,
		"; name: ",
		x@name,
		"; description: ",
		x@description,
		sep = "")
	invisible(x)
}

print.SwePhenomenon <- function(x) {
	cat(toString(x), "\n")
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
	.s <- paste(
			#"Object of class GmlTimeInstant",
			#"; timePosition:",
			toString(x@timePosition))
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
	.s <- ""

	if(!is.na(x@duration)) {
		.s <- paste(.s, "; duration: ", x@duration)
	}
	if(!is.null(x@timeInterval)) {
		.s <- paste(.s, ", timeInterval: ", toString(x@timeInterval), ";")
	}
	
	if(!is.null(x@begin) && !is.null(x@end)) {
		.s <- paste(.s, "begin: ", toString(x@begin), " -- end: ",
				toString(x@end))
	}
	else {
		.s <- paste("beginPosition: ", toString(x@beginPosition),
				" -- endPosition: ", toString(x@endPosition))
	}
	
	.s <- paste("Object of class GmlTimePeriod:", .s)
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
			toString(x@feature),
			sep = "")
	return(.s)
}

print.GmlFeatureProperty <- function(x) {
	cat(toString.GmlFeatureProperty(x), "\n")
	invisible(x)
}

toString.GmlFeatureCollection <- function(x) {
	.s <- paste("Object of class GmlFeatureCollection",
			"; id: ",
			x@id,
			"; ",
			length(x@featureMembers),
			" featureMembers: ",
			toString(x@featureMembers),
			sep = "")
	return(.s)
}

print.GmlFeatureCollection <- function(x) {
	cat(toString.GmlFeatureCollection(x), "\n")
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
			", srsAxisLabels: ",
			x@axisLabels,
			", uomLabels: ",
			x@uomLabels,
			sep = "")
	return(.s)
}

print.GmlDirectPosition <- function(x) {
	cat(toString.GmlDirectPosition(x), "\n")
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
			sep = "")
	return(.s)
}

print.GmlPoint <- function(x) {
	cat(toString.GmlPoint(x), "\n")
	invisible(x)
}

toString.GmlPointProperty <- function(x) {
	.s <- paste("Object of class GmlPointProperty",
			"; href: ",
			x@href,
			"; point: ",
			toString(x@point),
			sep = "")
	return(.s)
}

print.GmlPointProperty <- function(x) {
	cat(toString.GmlPointProperty(x), "\n")
	invisible(x)
}

.tempOpToString <- function(obj) {
	.s <- paste("propertyName:", obj@propertyName,
			"time:", toString(obj@time))
	return(.s)
}

toString.GmlGeometry <- function(x) {
	.s <- paste("Object of class GmlGeometry",
			"; id: ",
			x@id,
			";\nsrsName: ",
			x@srsName,
			", srsDimension: ",
			x@srsDimension,
			", srsDimension: ",
			x@axisLabels,
			", uomLabels: ",
			x@uomLabels,
			sep = "")
	return(.s)
}

print.GmlGeometry <- function(x) {
	cat(toString.GmlGeometry(x), "\n")
	invisible(x)
}

toString.GmlEnvelope <- function(x) {
	.s <- paste("Object of class GmlEnvelope",
			"; srsName: ",
			x@srsName,
			", srsDimension: ",
			x@srsDimension,
			", srsDimension: ",
			x@axisLabels,
			", uomLabels: ",
			x@uomLabels,
			";\n\tlowerCorner: ",
			toString(x@lowerCorner),
			";\n\tupperCorner: ",
			toString(x@upperCorner),
			sep = "")
	return(.s)
}

print.GmlEnvelope <- function(x) {
	cat(toString.GmlEnvelope(x), "\n")
	invisible(x)
}

toString.TM_After <- function(x) {
	.s <- paste("Object of class TM_After;",
			.tempOpToString(x))
	return(.s)
}

print.TM_After <- function(x) {
	cat(toString.TM_After(x), "\n")
	invisible(x)
}

toString.TM_Before <- function(x) {
	.s <- paste("Object of class TM_Before;",
			.tempOpToString(x))
	return(.s)
}

print.TM_Before <- function(x) {
	cat(toString.TM_Before(x), "\n")
	invisible(x)
}

toString.TM_During <- function(x) {
	.s <- paste("Object of class TM_During;",
			.tempOpToString(x))
	return(.s)
}

print.TM_During <- function(x) {
	cat(toString.TM_During(x), "\n")
	invisible(x)
}

toString.TM_Equals <- function(x) {
	.s <- paste("Object of class TM_Equals;",
			.tempOpToString(x))
	return(.s)
}


print.TM_Equals <- function(x) {
	cat(toString.TM_Equals(x), "\n")
	invisible(x)
}

toString.OgcBBOX <- function(x) {
	.s <- paste("Object of class OgcBBOX; propertyName: ",
			x@propertyName,
			"; envelope: ",
			toString(x@envelope),
			sep = "")
	return(.s)
}

print.OgcBBOX <- function(x) {
	cat(toString.OgcContains(x), "\n")
	invisible(x)
}

.binSpatOpToString <- function(x) {
	.s <- paste("propertyName:",
			x@propertyName,
			";\n\tgeometry: ",
			toString(x@geometry),
			";\n\tenvelope: ",
			toString(x@envelope),
			sep = "")
	return(.s)
}

toString.OgcContains <- function(x) {
	.s <- paste("Object of class OgcContains;",
			.binSpatOpToString(x))
	return(.s)
}

print.OgcContains <- function(x) {
	cat(toString.OgcContains(x), "\n")
	invisible(x)
}

toString.OgcIntersects <- function(x) {
	.s <- paste("Object of class OgcIntersects;",
			.binSpatOpToString(x))
	return(.s)
}

print.OgcIntersects <- function(x) {
	cat(toString.OgcIntersects(x), "\n")
	invisible(x)
}

toString.OgcOverlaps <- function(x) {
	.s <- paste("Object of class OgcOverlaps;",
			.binSpatOpToString(x))
	return(.s)
}

print.OgcOverlaps <- function(x) {
	cat(toString.OgcOverlaps(x), "\n")
	invisible(x)
}

toString.SaSamplingPoint <- function(x) {
	.s <- paste("Object of class SaSamplingPoint",
			"; id: ",
			x@id,
			"; position: ",
			toString(x@position),
			", relatedObservation: ",
			toString(x@relatedObservation),
			", relatedSamplingFeature: ",
			toString(x@relatedSamplingFeature),
			", surveyDetails: ",
			toString(x@surveyDetails),
			";\n\tsampledFeatures: ",
			toString(x@sampledFeatures),
			sep = "")
	return(.s)
}

print.SaSamplingPoint <- function(x) {
	cat(toString.SaSamplingPoint(x), "\n")
	invisible(x)
}

toString.SaSamplingSurface <- function(x) {
	.s <- paste("Object of class SaSamplingSurface",
			"; id: ",
			x@id,
			"; shape: ",
			toString(x@shape),
			", relatedObservation: ",
			toString(x@relatedObservation),
			", relatedSamplingFeature: ",
			toString(x@relatedSamplingFeature),
			", surveyDetails: ",
			toString(x@surveyDetails),
			", position: ",
			toString(x@position),
			";\n\tsampledFeatures: ",
			toString(x@sampledFeatures),
			sep = "")
	return(.s)
}

print.SaSamplingSurface <- function(x) {
	cat(toString.SaSamplingSurface(x), "\n")
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
setMethod("show", "SosEventTime", function(object) print.SosEventTime(object))
setMethod("show", "SosFeatureOfInterest", function(object) print.SosFeatureOfInterest(object))
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
setMethod("show", "GmlFeatureCollection", function(object) print.GmlFeatureCollection(object))
setMethod("show", "GmlDirectPosition", function(object) print.GmlDirectPosition(object))
setMethod("show", "GmlPoint", function(object) print.GmlPoint(object))
setMethod("show", "GmlPointProperty", function(object) print.GmlPointProperty(object))
setMethod("show", "GmlGeometry", function(object) print.GmlGeometry(object))
setMethod("show", "GmlEnvelope", function(object) print.GmlEnvelope(object))

setMethod("show", "TM_After", function(object) print.TM_After(object))
setMethod("show", "TM_Before", function(object) print.TM_Before(object))
setMethod("show", "TM_During", function(object) print.TM_During(object))
setMethod("show", "TM_Equals", function(object) print.TM_Equals(object))
setMethod("show", "OgcBBOX", function(object) print.OgcBBOX(object))
setMethod("show", "OgcContains", function(object) print.OgcContains(object))
setMethod("show", "OgcIntersects", function(object) print.OgcIntersects(object))
setMethod("show", "OgcOverlaps", function(object) print.OgcOverlaps(object))

################################################################################
# SUMMARY FUNCTIONS

################################################################################
# STR FUNCTIONS

################################################################################
# utils
.addTabIndent <- function(str) {
	.s <- gsub(pattern = "\t",  replacement = "\t\t", x = str)
	return(.s)
}

