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
	
	if (is(object, "OwsGetCapabilities_1.1.0")) {
		cat(", sections: ")
		cat(x@sections)
		cat(", acceptFormats: ")
		cat(x@acceptFormats)
		cat(", updateSequence: ")
		cat(x@updateSequence)
	}
	
	if (is(object, "OwsGetCapabilities_2.0.0")) {
		cat(", acceptLanguages: ")
		cat(x@acceptLanguages)
	}

	cat(", request: ")
	cat(x@request)
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
	cat("Object of class OwsServiceIdentification (see @xml)")
#	print("xml:")
#	print(x@xml)
	invisible(x)
}

print.OwsServiceProvider <- function(x, ...) {
	cat("Object of class OwsServiceProvider (see @xml)")
#	print("xml:")
#	print(x@xml)
	invisible(x)
}

print.OwsContents <- function(x, ...) {
	cat("Object of class OwsContents (see @xml)")
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
		cat("\nOperations (names): ")
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

print.SOS <- function(x, ...) {
	cat("Object of class SOS")
	cat("; version: ")
	cat(x@version)
	cat(", method: ")
	cat(x@method)
	cat(", url: ")
	cat(x@url)
	
	cat("\ncapabilities: ")
	print(x@capabilities)
	
	invisible(x)
}

print.SosFilter_Capabilities <- function(x, ...) {
	cat("Object of class SosFilter_Capabilities (see @xml)")
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
	
	cat("\nprocedure: ")
	cat(x@procedure)
	cat("\nobservedProperty: ")
	cat(x@observedProperty)
	cat("\nfeatureOfInterest: ")
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
	cat("Object of class SosContents with observation offerings: ")
	cat(paste(names(x@observationOfferings)))
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

# TODO add other print functions from above!

################################################################################
# STR FUNCTIONS
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
