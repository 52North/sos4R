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
# Created: 2010-09-21                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

################################################################################
# TODO read data function
# 
# convenience function using non-SWE names for stuff
#
# parameters:
#		sos:
#				the sos to query
#		
# 		time = c():	
#				either one time as POSIXt (or character which is tried to be
#				parsed to POSIXt) and forms a time instant, or two times
#				(character or POSIXt) that form an interval
#		mergeResult = FALSE:
#				boolean flag that turns on merging of the result into one
#				dataframe, the single observations cannot be accesses,
#				essentially only gives all [measurement,observation]@result
#		addLocation = FALSE
#				trigger if the location from the  should be added to the result
#				table fom the featureOfInterest
#
# * Function does not include "offering", which is/are instead automatically
#	detected... but what if one sensor is in several offerings?
#
# * Function returns data.frame, NOT OmObservation or OmMeasurement classes
#
# * if querying several procedures with different positions, put in them into 
#	one data.frame, or a list?
#
read.sos <- function(sos,
		sensors = NA_character_,
		phenomena = NA_character_,
		bbox = NA_character_, # one, or several?
		times = c(NA), # one, or several?
		mergeResult = FALSE,
		addLocation = FALSE,
		verbose = FALSE) {
	warning("Method is not implemented yet!")
}


#
# convenience info function using non-SWE names for stuff
#
info.sos <- function(sos,
		# select the info to return, if > 1 as named list
		phenomena = TRUE,
		bbox = TRUE,
		timePeriod = TRUE,
		# 
		locations = FALSE,
		all = FALSE,
		sensors = FALSE,
		offerings = FALSE,
		features = FALSE,
		metadata = FALSE,
		operations = FALSE,
		UOMs = FALSE,
		# select "filters", only one at a time and
		sensor = NA_character_,
		phenomenon = NA_character_,
		offering = NA_character_,
		# data is an index of the list of the info.data operation
		series = NA_integer_) {
	#standardGeneric("info.sos")
	warning("Method is not implemented yet!")
}


#
# sensors = procedure
# phenomena = observedProperty
# metadata: url, method, version, identification, provider, timeFormat
# offerings: shortly the name, the bounding box, the time period - DEFAULT!
# operations: just the names and how to get more info

#
# method creates a list (whose items can are named and can be used to read data,
# or just used in function read.sos)
# for all available and valid combinations of off/foi/phen/proc
#
info.series <- function(sos) {
	warning("Method is not implemented yet!")
}

#
# method returns information about sensors (procedures) matching the given
# criteria
#
info.sensors <- function(sos,
		phenomena = c(NA_character_),
		offerings = c(NA_character_),
		UOMs = c(NA_character_)) {
	warning("Method is not implemented yet!")
}

#
#
#
info.phenomena <- function(sos) {
	warning("Method is not implemented yet!")
}

#
#
#
info.features <- function(sos) {
	warning("Method is not implemented yet!")
}

#
#
#
info.offerings <- function(sos) {
	warning("Method is not implemented yet!")
}


################################################################################
# accessor functions
if (!isGeneric("sosCaps"))
	setGeneric(name = "sosCaps", def = function(sos) {
				standardGeneric("sosCaps")
			})
setMethod(f = "sosCaps", signature = signature(sos = "SOS"),
		def = function(sos) {
			return(sos@capabilities)
		})

if (!isGeneric("sosFilter_Capabilities"))
	setGeneric(name = "sosFilter_Capabilities", def = function(sos) {
				standardGeneric("sosFilter_Capabilities")
			})
setMethod(f = "sosFilter_Capabilities", signature = signature(sos = "SOS"),
		def = function(sos) {
			return(sos@capabilities@filterCapabilities)
		})

if (!isGeneric("sosServiceIdentification"))
	setGeneric(name = "sosServiceIdentification", def = function(sos) {
				standardGeneric("sosServiceIdentification")
			})
setMethod(f = "sosServiceIdentification", signature = signature(sos = "SOS"),
		def = function(sos) {
			return(sos@capabilities@identification)
		})

if (!isGeneric("sosServiceProvider"))
	setGeneric(name = "sosServiceProvider", def = function(sos) {
				standardGeneric("sosServiceProvider")
			})
setMethod(f = "sosServiceProvider", signature = signature(sos = "SOS"),
		def = function(sos) {
			return(sos@capabilities@provider)
		})

if (!isGeneric("sosOperationsMetadata"))
	setGeneric(name = "sosOperationsMetadata", def = function(sos) {
				standardGeneric("sosOperationsMetadata")
			})
setMethod(f = "sosOperationsMetadata", signature = signature(sos = "SOS"),
		def = function(sos) {
			return(sos@capabilities@operations)
		})

if (!isGeneric("sosOperations"))
	setGeneric(name = "sosOperations", def = function(obj) {
				standardGeneric("sosOperations")
			})
setMethod(f = "sosOperations", signature = signature(obj = "SOS"),
		def = function(obj) {
			return(sosOperations(obj@capabilities))
		})
setMethod(f = "sosOperations",
		signature = signature(obj = "SosCapabilities_1.0.0"),
		def = function(obj) {
			return(obj@operations@operations)
		})
# required to handle the first capabilities request:
setMethod(f = "sosOperations",
		signature = signature(obj = "OwsCapabilities"),
		def = function(obj) {
			return(NULL)
		})

if (!isGeneric("sosContents"))
	setGeneric(name = "sosContents", def = function(sos) {
				standardGeneric("sosContents")
			})
setMethod(f = "sosContents", signature = signature(sos = "SOS"),
		def = function(sos) {
			return(sosCaps(sos)@contents)
		})

if (!isGeneric("sosUrl"))
	setGeneric(name = "sosUrl", def = function(sos) {
				standardGeneric("sosUrl")
			})
setMethod(f = "sosUrl", signature = signature(sos = "SOS_1.0.0"),
		def = function(sos) {
			return(sos@url)
		})

if (!isGeneric("sosVersion"))
	setGeneric(name = "sosVersion", def = function(sos) {
				standardGeneric("sosVersion")
			})
setMethod(f = "sosVersion", signature = signature(sos = "SOS"),
		def = function(sos) {
			return(sos@version)
		})

if (!isGeneric("sosMethod"))
	setGeneric(name = "sosMethod", def = function(sos) {
				standardGeneric("sosMethod")
			})
setMethod(f = "sosMethod", signature = signature(sos = "SOS_1.0.0"),
		def = function(sos) {
			return(sos@method)
		})

if (!isGeneric("sosProcedures"))
	setGeneric(name = "sosProcedures", def = function(obj) {
				standardGeneric("sosProcedures")
			})
setMethod(f = "sosProcedures", signature = signature(obj = "SOS"),
		def = function(obj) {
			.offerings <- sosOfferings(obj)
			.p <- lapply(.offerings, sosProcedures)
			names(.p) <- names(.offerings)
			return(.p)
		})
setMethod(f = "sosProcedures",
		signature = signature(obj = "SosObservationOffering"),
		def = function(obj) {
			.p <- as.character(obj@procedure)
			return(.p)
		})
setMethod(f = "sosProcedures",
		signature = signature(obj = "list"),
		def = function(obj) {
			.p <- sapply(obj, sosProcedures)
			return(.p)
		})
setMethod(f = "sosProcedures",
		signature = signature(obj = "OmObservationCollection"),
		def = function(obj) {
			.p <- sapply(obj@members, sosProcedures)
			return(.p)
		})
setMethod(f = "sosProcedures",
		signature = signature(obj = "OmObservation"),
		def = function(obj) {
			.p <- as.character(obj@procedure)
			return(.p)
		})
setMethod(f = "sosProcedures",
		signature = signature(obj = "OmMeasurement"),
		def = function(obj) {
			.p <- as.character(obj@procedure)
			return(.p)
		})

if (!isGeneric("sosObservedProperties"))
	setGeneric(name = "sosObservedProperties", def = function(obj, ...) {
				standardGeneric("sosObservedProperties")
			})
setMethod(f = "sosObservedProperties", signature = signature(obj = "SOS"),
		def = function(obj) {
			.op <- lapply(sosOfferings(obj), sosObservedProperties)
			return(.op)
		})
setMethod(f = "sosObservedProperties", signature = signature(
				obj = "SosObservationOffering"),
		def = function(obj) {
			.op <- obj@observedProperty
			return(.op)
		})
setMethod(f = "sosObservedProperties", signature = signature(
				obj = "OmObservationCollection"),
		def = function(obj) {
			.op <- lapply(obj@members, sosObservedProperties)
#			if(removeDuplicates)
#				.op <- unique(.op)[[1]]
			return(.op)
		})
setMethod(f = "sosObservedProperties", signature = signature(
				obj = "list"),
		def = function(obj) {
			.op <- lapply(obj, sosObservedProperties)
			return(.op)
		})
setMethod(f = "sosObservedProperties", signature = signature(
				obj = "OmObservation"),
		def = function(obj) {
			.op <- sosObservedProperties(obj@observedProperty)
			return(.op)
		})
setMethod(f = "sosObservedProperties", signature = signature(
				obj = "SwePhenomenonProperty"),
		def = function(obj) {
			if(!is.na(obj@href)) {
				return(obj@href)
			}
			else {
				.op <- sosObservedProperties(obj@phenomenon)
				return(.op)
			}
		})
setMethod(f = "sosObservedProperties", signature = signature(
				obj = "SweCompositePhenomenon"),
		def = function(obj) {
			.op <- sapply(obj@components, sosObservedProperties)
			return(.op)
		})
setMethod(f = "sosObservedProperties", signature = signature(
				obj = "SwePhenomenonProperty"),
		def = function(obj) {
			return(obj@href)
		})

if (!isGeneric("sosBoundedBy"))
	setGeneric(name = "sosBoundedBy", def = function(obj, ...) {
				standardGeneric("sosBoundedBy")
			})
setMethod(f = "sosBoundedBy", signature = signature(
				obj = "SosObservationOffering"),
		def = function(obj, bbox = FALSE) {
			return(.boundedBy(obj, bbox))
		})
setMethod(f = "sosBoundedBy", signature = signature(obj = "list"),
		def = function(obj) {
			.bb <- lapply(obj, sosBoundedBy)
			return(.bb)
		})
setMethod(f = "sosBoundedBy",
		signature = signature(obj = "OmObservationCollection"),
		def = function(obj, bbox = FALSE) {
			return(.boundedBy(obj, bbox))
		})
.boundedBy <- function(obj, bbox) {
	.bb <- NA
	
	if(bbox) {
		.lC <- strsplit(x = obj@boundedBy[[gmlLowerCornerName]],
				split = " ")[[1]]
		.uC <- strsplit(x = obj@boundedBy[[gmlUpperCornerName]],
				split = " ")[[1]]
		min1 <- as.double(.lC[[1]])
		min2 <- as.double(.lC[[2]])
		max1 <- as.double(.uC[[1]])
		max2 <- as.double(.uC[[2]])
		
		.bb <- matrix(c(min1, min2, max1, max2), ncol = 2,
				dimnames = list(c("coords.lat", "coords.lon"),
						c("min", "max")))
	}
	else {
		.bb <- obj@boundedBy
	}
	
	return(.bb)
}

if (!isGeneric("sosOfferings"))
	setGeneric(name = "sosOfferings", def = function(obj, ...) {
				standardGeneric("sosOfferings")
			})
setMethod(f = "sosOfferings", signature = signature(obj = "SOS"),
		def = function(obj, offeringIDs = c(), name = NA) {
			.offerings <- obj@capabilities@contents@observationOfferings
			if(!is.na(name)) {
				for (.o in .offerings) {
					.currentName <- sosName(.o)
					if(.currentName == name)
						return(.o)
				}
			}
			if(length(offeringIDs) > 0)
				return(.offerings[offeringIDs])
			
			return(.offerings)
		})

if (!isGeneric("sosOfferingIds"))
	setGeneric(name = "sosOfferingIds", def = function(sos) {
				standardGeneric("sosOfferingIds")
			})
setMethod(f = "sosOfferingIds", signature = signature(sos = "SOS"),
		def = function(sos) {
			return(names(sosOfferings(sos)))
		})

if (!isGeneric("sosFeaturesOfInterest"))
	setGeneric(name = "sosFeaturesOfInterest", def = function(obj, ...) {
				standardGeneric("sosFeaturesOfInterest")
			})
setMethod(f = "sosFeaturesOfInterest", signature = signature(obj = "SOS"),
		def = function(obj, offerings = sosOfferingIds(obj)) {
			# via observation offering
			.offerings <- sosOfferings(obj)
			.offerings <- .offerings[offerings]
			.wantedOfferings <- lapply(.offerings, slot,
					name = "featureOfInterest")
			return(.wantedOfferings)
		})
setMethod(f = "sosFeaturesOfInterest",
		signature = signature(obj = "SosObservationOffering"),
		def = function(obj) {
			return(obj@featureOfInterest)
		})
setMethod(f = "sosFeaturesOfInterest",
		signature = signature(obj = "OmObservation"),
		def = function(obj) {
			.foi <- obj@featureOfInterest
			if(is.list(.foi) && length(.foi) == 1)
				return(.foi[[1]])
			return(.foi)
		})
setMethod(f = "sosFeaturesOfInterest",
		signature = signature(obj = "OmMeasurement"),
		def = function(obj) {
			.foi <- obj@featureOfInterest
			if(is.list(.foi) && length(.foi) == 1)
				return(.foi[[1]])
			return(.foi)
		})
setMethod(f = "sosFeaturesOfInterest",
		signature = signature(obj = "OmObservationCollection"),
		def = function(obj) {
			.fois <- sapply(obj@members, sosFeaturesOfInterest)
			return(.fois)
		})
setMethod(f = "sosFeaturesOfInterest",
		signature = signature(obj = "list"),
		def = function(obj) {
			.fois <- sapply(obj, sosFeaturesOfInterest)
			return(.fois)
		})
setMethod(f = "sosFeaturesOfInterest",
		signature = signature(obj = "GmlFeatureCollection"),
		def = function(obj) {
			.fois <- sapply(obj@featureCollection, sosFeaturesOfInterest)
			return(.fois)
		})

if (!isGeneric("sosFeatureIds"))
	setGeneric(name = "sosFeatureIds", def = function(obj, ...) {
				standardGeneric("sosFeatureIds")
			})
setMethod(f = "sosFeatureIds",
		signature = signature(obj = "list"),
		def = function(obj) {
			.fois <- lapply(obj, sosFeatureIds)
			return(.fois)
		})
setMethod(f = "sosFeatureIds",
		signature = signature(obj = "OmObservationCollection"),
		def = function(obj) {
			.fois <- sapply(obj@members, sosFeatureIds)
			return(.fois)
		})
setMethod(f = "sosFeatureIds",
		signature = signature(obj = "OmObservation"),
		def = function(obj) {
			.fois <- sosFeatureIds(obj@featureOfInterest)
			return(.fois)
		})
setMethod(f = "sosFeatureIds",
		signature = signature(obj = "OmMeasurement"),
		def = function(obj) {
			.fois <- sosFeatureIds(obj@featureOfInterest)
			return(.fois)
		})
setMethod(f = "sosFeatureIds",
		signature = signature(obj = "GmlFeatureCollection"),
		def = function(obj) {
			.fois <- sosFeatureIds(obj@featureMembers)
			return(.fois)
		})
setMethod(f = "sosFeatureIds",
		signature = signature(obj = "GmlFeatureProperty"),
		def = function(obj) {
			if(!is.null(obj@feature)) {
				.id <- sosFeatureIds(obj@feature)
				return(.id)
			}
			else {
				return(obj@href)
			}
		})
setMethod(f = "sosFeatureIds",
		signature = signature(obj = "SaSamplingPoint"),
		def = function(obj) {
			return(obj@id)
		})

if (!isGeneric("sosOperation"))
	setGeneric(name = "sosOperation", def = function(sos, operationName) {
				standardGeneric("sosOperation")
			})
setMethod(f = "sosOperation",
		signature = signature(sos = "SOS", operationName = "character"),
		def = function(sos, operationName) {
			.caps <- sosCaps(sos)
			return(.caps@operations@operations[[operationName]])
		})

if (!isGeneric("sosResponseFormats"))
	setGeneric(name = "sosResponseFormats", def = function(obj, ...) {
				standardGeneric("sosResponseFormats")
			})
setMethod(f = "sosResponseFormats", signature = signature(obj = "SOS"),
		def = function(obj, unique = FALSE) {
#			.caps <- sosCaps(obj)
#			.getOb <- .caps@operations@operations[[sosGetObservationName]]
#			return(.getOb@parameters$responseFormat)
			.rf <- sapply(sosOperations(obj), sosResponseFormats)
			if(unique) {
				.c <- do.call(c, .rf)
				.rf <- unique(.c)
			}
			return(.rf)
		})
setMethod(f = "sosResponseFormats",
		signature = signature(obj = "SosObservationOffering"),
		def = function(obj) {
			return(obj@responseFormat)
		})
setMethod(f = "sosResponseFormats",
		signature = signature(obj = "OwsOperation"),
		def = function(obj) {
			return(obj@parameters$responseFormat)
		})

if (!isGeneric("sosResponseMode"))
	setGeneric(name = "sosResponseMode", def = function(obj, ...) {
				standardGeneric("sosResponseMode")
			})
setMethod(f = "sosResponseMode", signature = signature(obj = "SOS"),
		def = function(obj, unique = FALSE) {
#			.caps <- sosCaps(obj)
#			.getOb <- .caps@operations@operations[[sosGetObservationName]]
#			return(.getOb@parameters$responseMode)
			.rf <- sapply(sosOperations(obj), sosResponseMode)
			if(unique) {
				.c <- do.call(c, .rf)
				.rf <- unique(.c)
			}
			return(.rf)
		})
setMethod(f = "sosResponseMode",
		signature = signature(obj = "SosObservationOffering"),
		def = function(obj) {
			return(obj@responseMode)
		})
setMethod(f = "sosResponseMode",
		signature = signature(obj = "OwsOperation"),
		def = function(obj) {
			return(obj@parameters$responseMode)
		})

if (!isGeneric("sosResultModels"))
	setGeneric(name = "sosResultModels", def = function(obj, ...) {
				standardGeneric("sosResultModels")
			})
setMethod(f = "sosResultModels", signature = signature(obj = "SOS"),
		def = function(obj, unique = FALSE) {
#			.caps <- sosCaps(obj)
#			.getOb <- .caps@operations@operations[[sosGetObservationName]]
#			return(.getOb@parameters$resultModel)
			.rf <- sapply(sosOperations(obj), sosResultModels)
			if(unique) {
				.c <- do.call(c, .rf)
				.rf <- unique(.c)
			}
			return(.rf)
		})
setMethod(f = "sosResultModels",
		signature = signature(obj = "SosObservationOffering"),
		def = function(obj) {
			return(obj@resultModel)
		})
setMethod(f = "sosResultModels",
		signature = signature(obj = "OwsOperation"),
		def = function(obj) {
			return(obj@parameters$resultModel)
		})

if (!isGeneric("sosTime"))
	setGeneric(name = "sosTime", def = function(obj, ...) {
				standardGeneric("sosTime")
			})
setMethod(f = "sosTime", signature = signature(obj = "SOS"),
		def = function(obj) {
			.caps <- sosCaps(obj)
			.getOb <- .caps@operations@operations[[sosGetObservationName]]
			.time <- .getOb@parameters$eventTime
			if(is.list(.time) && length(.time) == 1)
				return(.time[[1]])
			return(.time)
		})
setMethod(f = "sosTime", signature = signature(
				obj = "SosObservationOffering"),
		def = function(obj, convert = FALSE) {
			if(!convert)
				return(obj@time)
			
			.time <- obj@time
			if(is(.time, "GmlTimePeriod")) {
				return(sosTime(.time))
			}
			
			warning("Could not convert time to R objects.")
			return(obj@time)
		})
setMethod(f = "sosTime", signature = signature(obj = "GmlTimePeriod"),
		def = function(obj) {
			.start <- NA
			.end <- NA
			
			if(!is.null(obj@begin) && !is.null(obj@end)) {
#				print("begin and end!")
				.start <- sosTime(obj@begin)
				.end <- sosTime(obj@end)
			}
			
			if(!is.null(obj@beginPosition) && !is.null(obj@endPosition)) {
#				print("positions!")
				.start <- sosTime(obj@beginPosition)
				.end <- sosTime(obj@endPosition)
			}
			
			.period <- list(.start, .end)
			names(.period) <- c("begin", "end")
			return(.period)
		})
setMethod(f = "sosTime", signature = signature(obj = "GmlTimePosition"),
		def = function(obj) {
			.time <- obj@time
			.newAttrs <- list("frame" = obj@frame,
					"calendarEraName" = obj@calendarEraName,
					"indeterminatePosition" = obj@indeterminatePosition)
			attributes(.time) <- c(attributes(.time), .newAttrs)
			return(.time)
		})
setMethod(f = "sosTime", signature = signature(obj = "GmlTimeInstantProperty"),
		def = function(obj) {
			if(is.na(obj@href))
				return(obj@href)
			
			if(!is.null(obj@time))
				return(sosTime(obj@time))
			
			return(NA)
		})
setMethod(f = "sosTime", signature = signature(obj = "GmlTimeInstant"),
		def = function(obj) {
			return(sosTime(obj@timePosition))
		})

if (!isGeneric("sosTimeFormat"))
	setGeneric(name = "sosTimeFormat", def = function(sos) {
				standardGeneric("sosTimeFormat")
			})
setMethod(f = "sosTimeFormat", signature = signature(sos = "SOS"),
		def = function(sos) {
			return(sos@timeFormat)
		})

if (!isGeneric("sosParsers"))
	setGeneric(name = "sosParsers", def = function(sos) {
				standardGeneric("sosParsers")
			})
setMethod(f = "sosParsers", signature = signature(sos = "SOS"),
		def = function(sos) {
			return(sos@parsers)
		})

if (!isGeneric("sosResult"))
	setGeneric(name = "sosResult", def = function(obj, coordinates = FALSE,
					...) {
				standardGeneric("sosResult")
			})
setMethod(f = "sosResult", signature = signature(obj = "OmObservation"),
		def = function(obj, coordinates = FALSE) {
			if(coordinates){
				.coords <- sosCoordinates(obj)
				.data <- merge(x = obj@result, y = .coords)
				return(.data)
			}
			return(obj@result)
		})
setMethod(f = "sosResult", signature = signature(obj = "OmMeasurement"),
		def = function(obj, coordinates = FALSE) {
			
			.obsProp <- sosObservedProperties(obj)
			.value <- obj@result@value
			.uom <- obj@result@uom
			
			.result <- data.frame(.value)
			names(.result) <- .obsProp
			attributes(.result) <- c(attributes(.result), list("UOM" = .uom))
			
			if(coordinates){
				.coords <- sosCoordinates(obj)
				.data <- merge(x = .result, y = .coords)
				return(.data)
			}
			else return(.result)
		})
setMethod(f = "sosResult", signature = signature(obj = "OmObservationProperty"),
		def = function(obj, coordinates = FALSE) {
			if(!is.na(obj@href))
				return(c(href = obj@href))
			else if(!is.null(obj@obs))
				return(sosResult(obj = obj@obs, coordinates = coordinates))
			else return(NA)
		})
setMethod(f = "sosResult",
		signature = signature(obj = "OmObservationCollection"),
		def = function(obj, coordinates = FALSE, bind = TRUE) {
			.l <- lapply(obj@members, sosResult, coordinates = coordinates)
			if(bind)
				.result <- do.call(rbind, .l)
			else .result <- .l
			return(.result)
		})
setMethod(f = "sosResult", signature = signature(obj = "list"),
		def = function(obj, coordinates = FALSE) {
			.l <- lapply(obj, sosResult, coordinates = coordinates)
			.result <- do.call(rbind, .l)
			return(.result)
		})
setMethod(f = "sosResult", signature = signature(obj = "OwsExceptionReport"),
		def = function(obj, coordinates = FALSE) {
			warning("OwsExceptionReport does not have a result set.")
			return(toString(obj))
		})

if (!isGeneric("sosCoordinates"))
	setGeneric(name = "sosCoordinates", def = function(obj) {
				standardGeneric("sosCoordinates")
			})
setMethod(f = "sosCoordinates",
		signature = signature(obj = "OmObservationCollection"),
		def = function(obj) {
			.coords <- sosCoordinates(obj = obj@members)
			return(.coords)
		})
setMethod(f = "sosCoordinates", signature = signature(obj = "OmObservation"),
		def = function(obj) {
			.coords <- sosCoordinates(obj = obj@featureOfInterest)
			return(.coords)
		})
setMethod(f = "sosCoordinates",
		signature = signature(obj = "GmlFeatureCollection"),
		def = function(obj) {
			.list <- lapply(obj@featureMembers, sosCoordinates)
			.coords <- do.call(rbind, .list)
			return(.coords)
		})
setMethod(f = "sosCoordinates",
		signature = signature(obj = "GmlFeatureProperty"),
		def = function(obj) {
			if(!is.null(obj@feature)) {
				.coords <- sosCoordinates(obj = obj@feature)
				return(.coords)
			}
			else {
				warning("[sosCoordinates] Can only return coordinates if GmlFeatureProperty directly contains a feature.")
				return(NA)
			}
		})
setMethod(f = "sosCoordinates", signature = signature(obj = "SaSamplingPoint"),
		def = function(obj) {
			.coords <- sosCoordinates(obj = obj@position)
			.names <- names(.coords)
			.coords[, ncol(.coords)+1] <- sosId(obj)
			names(.coords) <- c(.names, sosDefaultColumnNameFeatureIdentifier)
			return(.coords)
		})
setMethod(f = "sosCoordinates", signature = signature(obj = "GmlPointProperty"),
		def = function(obj) {
			.coords <- sosCoordinates(obj = obj@point)
			return(.coords)
		})
setMethod(f = "sosCoordinates", signature = signature(obj = "GmlPoint"),
		def = function(obj) {
			.coords <- sosCoordinates(obj = obj@pos)
			return(.coords)
		})
setMethod(f = "sosCoordinates",
		signature = signature(obj = "GmlDirectPosition"),
		def = function(obj) {
			.coordinateDoubles <- as.double(
					strsplit(x = obj@pos, split = " ")[[1]])
			.coords <- as.data.frame(list(.coordinateDoubles[[1]],
							.coordinateDoubles[[2]], sosSrsName(obj)))
			names(.coords) <- c(sosDefaultColumnNameLat,
					sosDefaultColumnNameLon,
					sosDefaultColumnNameSRS)
			return(.coords)
		})
setMethod(f = "sosCoordinates",
		signature = signature(obj = "list"),
		def = function(obj) {
			.list <- lapply(obj, sosCoordinates)
			.coords <- do.call(rbind, .list)
			return(.coords)
		})

if (!isGeneric("sosSrsName"))
	setGeneric(name = "sosSrsName", def = function(obj) {
				standardGeneric("sosSrsName")
			})
setMethod(f = "sosSrsName", signature = signature(obj = "SOS"),
		def = function(obj) {
			.caps <- sosCaps(obj)
			.getOb <- .caps@operations@operations[[sosGetObservationName]]
			return(.getOb@parameters$srsName)
		})
setMethod(f = "sosSrsName",
		signature = signature(obj = "GmlDirectPosition"),
		def = function(obj) {
			return(obj@srsName)
		})
setMethod(f = "sosSrsName",
		signature = signature(obj = "GmlPoint"),
		def = function(obj) {
			.self <- obj@srsName
			if(is.na(.self)) {
				return(sosSrsName(obj@pos))
			}
			return(.self)
		})

if (!isGeneric("sosId"))
	setGeneric(name = "sosId", def = function(obj) {
				standardGeneric("sosId")
			})
setMethod(f = "sosId", signature = signature(obj = "GmlFeature"),
		def = function(obj) {
			return(obj@id)
		})
setMethod(f = "sosId", signature = signature(obj = "SosObservationOffering"),
		def = function(obj) {
			return(obj@id)
		})
setMethod(f = "sosId", signature = signature(obj = "list"),
		def = function(obj) {
			return(sapply(obj, sosId))
		})

if (!isGeneric("sosName"))
	setGeneric(name = "sosName", def = function(obj) {
				standardGeneric("sosName")
			})
setMethod(f = "sosName", signature = signature(obj = "list"),
		def = function(obj) {
			lapply(obj, sosName)
		})
setMethod(f = "sosName", signature = signature(obj = "SosObservationOffering"),
		def = function(obj) {
			return(obj@name)
		})
setMethod(f = "sosName", signature = signature(obj = "OwsServiceProvider"),
		def = function(obj) {
			return(obj@providerName)
		})
setMethod(f = "sosName", signature = signature(obj = "OwsOperation"),
		def = function(obj) {
			return(obj@name)
		})
setMethod(f = "sosName", signature = signature(obj = "SosDescribeSensor"),
		def = function(obj) {
			return(sosDescribeSensorName)
		})
setMethod(f = "sosName", signature = signature(obj = "SosGetObservation"),
		def = function(obj) {
			return(sosDescribeSensorName)
		})
setMethod(f = "sosName", signature = signature(obj = "SosGetObservationById"),
		def = function(obj) {
			return(sosGetObservationByIdName)
		})
setMethod(f = "sosName", signature = signature(obj = "OwsGetCapabilities"),
		def = function(obj) {
			return(sosGetCapabilitiesName)
		})


################################################################################
# conversion methods and accessor function
sosConvertTime <- function(x, sos) {
	.t <- as.POSIXct(strptime(x = x, format = sosTimeFormat(sos = sos)))
	return(.t)
}
sosConvertDouble <- function(x, sos) {
	return(as.double(x = x))
}
sosConvertString <- function(x, sos) {
	return(as.character(x = x))
}
sosConvertLogical <- function(x, sos) {
	return(as.logical(x = x))
}

if (!isGeneric("sosEncoders"))
	setGeneric(name = "sosEncoders", def = function(sos) {
				standardGeneric("sosEncoders")
			})
setMethod(f = "sosEncoders", signature = signature(sos = "SOS"),
		def = function(sos) {
			return(sos@encoders)
		})

if (!isGeneric("sosDataFieldConverters"))
	setGeneric(name = "sosDataFieldConverters", def = function(sos) {
				standardGeneric("sosDataFieldConverters")
			})
setMethod(f = "sosDataFieldConverters", signature = signature(sos = "SOS"),
		def = function(sos) {
			return(sos@dataFieldConverters)
		})


################################################################################
# convenience functions
if (!isGeneric("sosCreateTimeInstant"))
	setGeneric(name = "sosCreateTimeInstant", def = function(sos, time,
					frame = as.character(NA),
					calendarEraName = as.character(NA),
					indeterminatePosition = as.character(NA)) {
				standardGeneric("sosCreateTimeInstant")
			}
	)
setMethod(f = "sosCreateTimeInstant",
		signature = signature(sos = "SOS", time = "POSIXt"),
		def = function(sos, time, frame, calendarEraName,
				indeterminatePosition) {
			.time <- format(time, sos@timeFormat)
			.timePos <- GmlTimePosition(time = strptime(.time, sos@timeFormat),
					frame = frame, calendarEraName = calendarEraName,
					indeterminatePosition = indeterminatePosition)
			.ti <- GmlTimeInstant(timePosition = .timePos)
			return(.ti)
		}
)

if (!isGeneric("sosCreateTimePeriod"))
	setGeneric(name = "sosCreateTimePeriod",
			def = function(sos, begin, end, frame = as.character(NA),
					calendarEraName = as.character(NA),
					indeterminatePosition = as.character(NA),
					duration = as.character(NA),
					timeInterval = NULL) {
				standardGeneric("sosCreateTimePeriod")
			}
	)
setMethod(f = "sosCreateTimePeriod",
		signature = signature(sos = "SOS", begin = "POSIXt", end = "POSIXt"),
		def = function(sos, begin, end, frame, calendarEraName,
				indeterminatePosition, duration, timeInterval) {
			.beginPos <- GmlTimePosition(
					time = strptime(
							format(begin, sos@timeFormat),
							sos@timeFormat),
					frame = frame, calendarEraName = calendarEraName,
					indeterminatePosition = indeterminatePosition
			)
			.endPos <- GmlTimePosition(
					time = strptime(
							format(end, sos@timeFormat),
							sos@timeFormat),
					frame = frame, calendarEraName = calendarEraName,
					indeterminatePosition = indeterminatePosition
			)
			.tp <- GmlTimePeriod(beginPosition = .beginPos,
					endPosition = .endPos, duration = duration,
					timeInterval = timeInterval)
			return(.tp)
		}
)

#
#
#
if (!isGeneric("sosCreateEventTimeList"))
	setGeneric(name = "sosCreateEventTimeList",
			def = function(time, operator = sosDefaultTemporalOperator) {
				standardGeneric("sosCreateEventTimeList")
			})
setMethod(f = "sosCreateEventTimeList",
		signature = signature(time = "GmlTimeGeometricPrimitive"),
		def = function(time, operator) {
			.et <- list(sosCreateEventTime(time = time, operator = operator))
			return(.et)
		}
)
if (!isGeneric("sosCreateEventTime"))
	setGeneric(name = "sosCreateEventTime",
			def = function(time, operator = sosDefaultTemporalOperator) {
				standardGeneric("sosCreateEventTime")
			})
setMethod(f = "sosCreateEventTime",
		signature = signature(time = "GmlTimeGeometricPrimitive"),
		def = function(time, operator) {
			
			if(operator == ogcTempOpTMAfterName) {
				.tOps <- TM_After(time = time)
			}
			else if(operator == ogcTempOpTMBeforeName) {
				.tOps <- TM_Before(time = time)
			}
			else if(operator == ogcTempOpTMDuringName) {
				.tOps <- TM_During(time = time)
			}
			else if(operator == ogcTempOpTMEqualsName) {
				.tOps <- TM_Equals(time = time)
			}
			else {
				stop(paste("Given operator", operator, "is not supported,",
								"choose one of",
								toString(SosSupportedTemporalOperators())))
			}
			
			.et <- SosEventTime(.tOps)
			return(.et)
		}
)

#
#
#
if (!isGeneric("sosCreateFeatureOfInterest"))
	setGeneric(name = "sosCreateFeatureOfInterest",
			def = function(objectIDs = list(NA), spatialOps = NULL, bbox = NULL,
					srsName = NA_character_) {
				standardGeneric("sosCreateFeatureOfInterest")
			})
setMethod(f = "sosCreateFeatureOfInterest",
		signature = signature(),
		def = function(objectIDs, spatialOps, bbox, srsName) {
			# switch cases, either objectIDs or one of the spatialOps shortcuts
			if(!any(is.na(objectIDs))) {
				.foi <- SosFeatureOfInterest(objectIDs = objectIDs)
			}
			else if (!is.null(spatialOps)) {
				.foi <- SosFeatureOfInterest(spatialOps = spatialOps)
			}
			else if(!is.null(bbox)) {
				if(is.matrix(bbox)) {
					.env <- GmlEnvelope(
							lowerCorner = GmlDirectPositionLatLon(lat = bbox[2,1],
									lon = bbox[1,1]),
							upperCorner = GmlDirectPositionLatLon(lat = bbox[2,2],
									lon = bbox[1,2]),
							srsName = srsName)
					.bbox <- OgcBBOX(envelope = .env)
					.foi <- SosFeatureOfInterest(spatialOps = .bbox)
				}
				else {
					stop("bbox must be matrix!")
				}
			}
			else {
				stop("At least one of objectIDs or spatialOps has to be set!")
			}
			
			return(.foi)
		}
)
		
#
#
#
if (!isGeneric("sosCreateBBOX"))
	setGeneric(name = "sosCreateBBOX",
			def = function(lowLat, lowLon, uppLat, uppLon, srsName,
					srsDimension = NA_integer_, axisLabels = NA_character_,
					uomLabels = NA_character_,
					propertyName = sosDefaultSpatialOpPropertyName) {
				standardGeneric("sosCreateBBOX")
			})
setMethod(f = "sosCreateBBOX",
		signature = signature(lowLat = "numeric", lowLon = "numeric",
				uppLat = "numeric", uppLon = "numeric"),
		def = function(lowLat, lowLon, uppLat, uppLon, srsName,
			srsDimension = NA_integer_, axisLabels = NA_character_,
			uomLabels = NA_character_,
			propertyName = sosDefaultSpatialOpPropertyName) {
		.env <- GmlEnvelope(
				lowerCorner = GmlDirectPosition(
						pos = paste(lowLat, lowLon, sep = " ")),
				upperCorner = GmlDirectPosition(
						pos = paste(uppLat, uppLon, sep = " ")),
				srsName = srsName, srsDimension = srsDimension,
				axisLabels = axisLabels, uomLabels = uomLabels)
		
		.bbox <- OgcBBOX(propertyName = propertyName, envelope = .env)
		return(.bbox)
		}
)

#
#
#
if (!isGeneric("sosCreateBBoxMatrix"))
	setGeneric(name = "sosCreateBBoxMatrix",
			def = function(lowLat, lowLon, uppLat, uppLon) {
				standardGeneric("sosCreateBBoxMatrix")
			})
setMethod(f = "sosCreateBBoxMatrix",
		signature = signature(lowLat = "numeric", lowLon = "numeric",
				uppLat = "numeric", uppLon = "numeric"),
		def = function(lowLat, lowLon, uppLat, uppLon) {
			.m <- matrix(data = c(lowLon, lowLat, uppLon, uppLat),
					nrow = 2, ncol = 2,
					dimnames = list(
							c("longitude", "latitude"),
							c("lowerCorner", "upperCorner")))
			return(.m)
		}
)

################################################################################
# MISC

#
#
#
if (!isGeneric("sosCapabilitiesDocumentOriginal"))
	setGeneric(name = "sosCapabilitiesDocumentOriginal", def = function(sos) {
				standardGeneric("sosCapabilitiesDocumentOriginal")
			})
setMethod(f = "sosCapabilitiesDocumentOriginal",
		signature = signature(sos = "SOS"),
		def = function(sos) {
			.gc <- OwsGetCapabilities(service = sosService,
					acceptVersions = c(sos@version))
			.responseString = sosRequest(sos = sos, request = .gc,
					verbose = sos@verboseOutput, inspect = FALSE)
			.response <- xmlParseDoc(.responseString, asText = TRUE)
			return(.response)
		}
)

#
# helper methods for exception response handling
#
.isExceptionReport <- function(document) {
	if(owsExceptionReportName == xmlName(xmlRoot(document)))
		return(TRUE)
	else
		return(FALSE)
}
.handleExceptionReport <- function(sos, obj) {
	if(sos@verboseOutput) warning("Received ExceptionReport!")
	
	.parsingFunction <- sosParsers(sos)[[owsExceptionReportName]]
	.er <- .parsingFunction(obj)
	if(class(.er) == "OwsExceptionReport")
		warning(toString(.er))
	return(.er)
}

#
#
#
.createLatestEventTime <- function(verbose = FALSE) {
	if(verbose) cat("Creating non-standard event time 'latest'\n")
	.et <- SosEventTimeLatest()
	return(.et)
}

#
# encoding functions that just pass given content along...
#
setMethod(f = "encodeXML", signature = signature(obj = "XMLNode", sos = "SOS"),
		def = function(obj, sos, verbose = FALSE) {
			if(verbose) {
				cat("ENCODE XML from XMLNode\n")
			}
			return(obj)
		}
)
setMethod(f = "encodeXML", signature = signature(obj = "XMLInternalElementNode", sos = "SOS"),
		def = function(obj, sos, verbose = FALSE) {
			if(verbose) {
				cat("ENCODE XML from XMLInternalElementNode\n")
			}
			return(obj)
		}
)
setMethod(f = "encodeXML", signature = signature(obj = "character", sos = "SOS"),
		def = function(obj, sos, addNamespaces = FALSE, verbose = FALSE) {
			if(verbose) cat("ENCODE XML from character string\n")
			
			if(isXMLString(obj)) {
				#FIXME this just won't work, see testing.R, section "encode xml character string (again)"
				if(addNamespaces) {
					if(verbose) cat("Namespace hack for character string, trying to replace 'result>'!\n")
					.hack <- 'result xmlns:sos="http://www.opengis.net/sos/1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ows="http://www.opengis.net/ows/1.1" xmlns:om="http://www.opengis.net/om/1.0" xmlns:ogc="http://www.opengis.net/ogc" xmlns:gml="http://www.opengis.net/gml">'
					.hackedString <- sub(pattern = "result>",
							replacement = .hack,
							x = obj)
					.xml <- xmlTreeParse(.hackedString, asText = TRUE,
							useInternalNodes = FALSE)
				}
				else {
					.xml <- xmlParseString(obj)
				}
								
				if(verbose) {
					cat("Created XML from string:\n", toString(.xml))
				}
				return(.xml)
			}
			else {
				warning(paste("Could not encode given character string as XML!",
								" Character string: '", obj, "'", sep = ""))
			}
		}
)

################################################################################
# Helper functions for OWS exceptions, e.g. to get the meaning of an exception
# code.
#
setMethod(f = "sosExceptionCodeMeaning",
		signature = c(exceptionCode = "character"),
		def = function(exceptionCode) {
			.meaning <- as.character(
					.owsStandardExceptions[
							.owsStandardExceptions$exceptionCode==exceptionCode,
							2])
			return(.meaning)
		}
)

################################################################################
#
setMethod(f = "sosGetCRS",
		signature = c(obj = "character"),
		def = function(obj) {
			require("sp")
			
			# get the position of EPSG
			.split <- strsplit(as.character(obj), split = ":")
			.idx <- which(.split[[1]]=="EPSG")
			if(length(.idx) == 0) {
				warning(paste("Could not create CRS from the given object:", obj))
				return(NULL)
			}
			.epsg <- .split[[1]][[length(.split[[1]])]]
			
			.initString <- paste("+init=epsg", .epsg, sep = ":")
			.crs <- CRS(.initString)
			return(.crs)
		}
)

setMethod(f = "sosGetCRS",
		signature = c(obj = "OmObservation"),
		def = function(obj) {
			.char <- as.vector(sosCoordinates(obj)[[sosDefaultColumnNameSRS]])
			.l <- sapply(X = .char, FUN = sosGetCRS)
			.l <- unique(.l)
			
			if(length(.l) == 1)
				return(.l[[1]])
			else return(.l)
		}
)

#
#
#
setMethod(f = "sosGetDCP",
		signature = c(sos = "SOS", operation = "character"),
		def = function(sos, operation, type = NA) {
			.ops <- sosOperations(sos)
			
			if(is.null(.ops)) return(NULL)
			
			.dcps <- .ops[[operation]]@DCPs
			
			if(!is.na(type)) {
				return(.dcps[[type]])
			}
			else return(.dcps)
		}
)

