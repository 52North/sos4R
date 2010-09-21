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
# Created: 2010-09-21                                                         #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

################################################################################
# accessor functions
if (!isGeneric("sosCaps"))
	setGeneric(name = "sosCaps", def = function(sos) {
				standardGeneric("sosCaps")
			})
setMethod(f = "sosCaps", signature = signature(sos = "SOS"), def = function(sos) {
			return(sos@capabilities)
		}
)

if (!isGeneric("sosUrl"))
	setGeneric(name = "sosUrl", def = function(sos) {
				standardGeneric("sosUrl")
			})
setMethod(f = "sosUrl", signature = signature(sos = "SOS"), def = function(sos) {
			return(sos@url)
		})

if (!isGeneric("sosVersion"))
	setGeneric(name = "sosVersion", def = function(sos) {
				standardGeneric("sosVersion")
			})
setMethod(f = "sosVersion", signature = signature(sos = "SOS"), def = function(sos) {
			return(sos@version)
		})

if (!isGeneric("sosMethod"))
	setGeneric(name = "sosMethod", def = function(sos) {
				standardGeneric("sosMethod")
			})
setMethod(f = "sosMethod", signature = signature(sos = "SOS"), def = function(sos) {
			return(sos@method)
		})

if (!isGeneric("sosProcedures"))
	setGeneric(name = "sosProcedures", def = function(sos) {
				standardGeneric("sosProcedures")
			})
setMethod(f = "sosProcedures", signature = signature(sos = "SOS"), def = function(sos) {
			.caps <- sosCaps(sos)
			.ds <- .caps@operations@operations[["GetObservation"]]
			return(.ds@parameters$procedure)
		})

if (!isGeneric("sosObservedProperties"))
	setGeneric(name = "sosObservedProperties", def = function(sos) {
				standardGeneric("sosObservedProperties")
			})
setMethod(f = "sosObservedProperties", signature = signature(sos = "SOS"),
		def = function(sos) {
			.caps <- sosCaps(sos)
			.getOb <- .caps@operations@operations[[sosGetObservationName]]
			return(.getOb@parameters[[sosObservedPropertyName]])
		})

if (!isGeneric("sosOfferings"))
	setGeneric(name = "sosOfferings", def = function(sos) {
				standardGeneric("sosOfferings")
			})
setMethod(f = "sosOfferings", signature = signature(sos = "SOS"), def = function(sos) {
			.offerings <- sos@capabilities@contents@observationOfferings
			return(.offerings)
		})
if (!isGeneric("sosOffering"))
	setGeneric(name = "sosOffering", def = function(sos, offeringId) {
				standardGeneric("sosOffering")
			})
setMethod(f = "sosOffering", signature = signature(sos = "SOS", 
				offeringId = "character"),
		def = function(sos, offeringId) {
			.offerings <- sos@capabilities@contents@observationOfferings
			return(.offerings[[offeringId]])
		})
if (!isGeneric("sosOfferingIds"))
	setGeneric(name = "sosOfferingIds", def = function(sos) {
				standardGeneric("sosOfferingIds")
			})
setMethod(f = "sosOfferingIds", signature = signature(sos = "SOS"),
		def = function(sos) {
			return(names(sosOfferings(sos)))
		})

if (!isGeneric("sosFOIs"))
	setGeneric(name = "sosFOIs", def = function(sos) {
				standardGeneric("sosFOIs")
			})
setMethod(f = "sosFOIs", signature = signature(sos = "SOS"), def = function(sos) {
			.caps <- sosCaps(sos)
			
			# via GetFeatureOfInterest
			.gfoi <- .caps@operations@operations[["GetFeatureOfInterest"]]
			if(!is.null(.gfoi)) {
				return(.gfoi@parameters$featureOfInterestId)
			}
			else return("GetFeatureOfInterest-Operation not supported!")
			
			return(.fois)
		})

if (!isGeneric("sosOperationInfo"))
	setGeneric(name = "sosOperationInfo", def = function(sos, operationName) {
				standardGeneric("sosOperationInfo")
			})
setMethod(f = "sosOperationInfo",
		signature = signature(sos = "SOS", operationName = "character"),
		def = function(sos, operationName) {
			.caps <- sosCaps(sos)
			return(.caps@operations@operations[[operationName]])
		})

if (!isGeneric("sosResponseFormats"))
	setGeneric(name = "sosResponseFormats", def = function(sos) {
				standardGeneric("sosResponseFormats")
			})
setMethod(f = "sosResponseFormats", signature = signature(sos = "SOS"),
		def = function(sos) {
			.caps <- sosCaps(sos)
			.getOb <- .caps@operations@operations[[sosGetObservationName]]
			return(.getOb@parameters$responseFormat)
		})

if (!isGeneric("sosResponseMode"))
	setGeneric(name = "sosResponseMode", def = function(sos) {
				standardGeneric("sosResponseMode")
			})
setMethod(f = "sosResponseMode", signature = signature(sos = "SOS"),
		def = function(sos) {
			.caps <- sosCaps(sos)
			.getOb <- .caps@operations@operations[[sosGetObservationName]]
			return(.getOb@parameters$responseMode)
		})

if (!isGeneric("sosSrsName"))
	setGeneric(name = "sosSrsName", def = function(sos) {
				standardGeneric("sosSrsName")
			})
setMethod(f = "sosSrsName", signature = signature(sos = "SOS"),
		def = function(sos) {
			.caps <- sosCaps(sos)
			.getOb <- .caps@operations@operations[[sosGetObservationName]]
			return(.getOb@parameters$srsName)
		})

if (!isGeneric("sosResultModels"))
	setGeneric(name = "sosResultModels", def = function(sos) {
				standardGeneric("sosResultModels")
			})
setMethod(f = "sosResultModels", signature = signature(sos = "SOS"),
		def = function(sos) {
			.caps <- sosCaps(sos)
			.getOb <- .caps@operations@operations[[sosGetObservationName]]
			return(.getOb@parameters$resultModel)
		})

if (!isGeneric("sosTimePeriod"))
	setGeneric(name = "sosTimePeriod", def = function(obj) {
				standardGeneric("sosTimePeriod")
			})
setMethod(f = "sosTimePeriod", signature = signature(obj = "SOS"),
		def = function(obj) {
			.caps <- sosCaps(obj)
			.getOb <- .caps@operations@operations[[sosGetObservationName]]
			return(.getOb@parameters$eventTime)
		})
setMethod(f = "sosTimePeriod", signature = signature(obj = "SosObservationOffering"),
		def = function(obj) {
			return(obj@time)
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

if (!isGeneric("sosFieldConverters"))
	setGeneric(name = "sosFieldConverters", def = function(sos) {
				standardGeneric("sosFieldConverters")
			})
setMethod(f = "sosFieldConverters", signature = signature(sos = "SOS"),
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
# sosCreateEventTime(SosSupportedTemporalOperators()[["TM_During"]], p1)
#
if (!isGeneric("sosCreateEventTime"))
	setGeneric(name = "sosCreateEventTime",
			def = function(time, operator = sosDefaultTemporalOperator) {
				standardGeneric("sosCreateEventTime")
			})
setMethod(f = "sosCreateEventTime",
		signature = signature(time = "GmlTimeGeometricPrimitive",
				operator = "ANY"),
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
								" choose one of",
								SosSupportedTemporalOperators()))
			}
			
			.et <- list(SosEventTime(.tOps))
			return(.et)
		}
)

#
# new("SosFeatureOfInterest", objectIDs = objectIDs, spatialOps = spatialOps)
#
sosCreateFeatureOfInterest <- function(objectIDs = list(NA),
		spatialOps = NULL, bbox = NULL, srsName = NA) {
	
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

#
#
#
sosCreateBBOX <- function(lowLat, lowLon, uppLat, uppLon, srsName,
		srsDimension = NA_integer_, axisLabels = NA_character_,
		uomLabels = NA_character_,
		propertyName = sosDefaultSpatialOpPropertyName) {
	.env <- GmlEnvelope(
			lowerCorner = GmlDirectPosition(lat = lowLat, lon = lowLon),
			upperCorner = GmlDirectPosition(lat = uppLat, lon = uppLon),
			srsName = srsName, srsDimension = srsDimension,
			axisLabels = axisLabels, uomLabels = uomLabels)
	OgcBBOX(propertyName = propertyName, envelope = .env)
}

#
#
#
sosCreateBBoxMatrix <- function(lowLon, lowLat, uppLon, uppLat) {
	.m <- matrix(data = c(lowLon, lowLat, uppLon, uppLat),
			nrow = 2, ncol = 2,
			dimnames = list(
					c("longitude", "latitude"),
					c("lowerCorner", "upperCorner")))
	return(.m)
}


################################################################################
# OTHER

#
# set parsers, encoders and convertes to default
#
if (!isGeneric("sosSetFunctionsToDefault"))
	setGeneric(name = "sosSetFunctionsToDefault", def = function(sos) {
				standardGeneric("sosSetFunctionsToDefault")
			})
setMethod(f = "sosSetFunctionsToDefault", signature = signature(sos = "SOS"),
		def = function(sos) {
			.defaultMethods <- sosInitDefaults()
			sos@parsers <- .defaultMethods[[1]]
			sos@encoders <- .defaultMethods[[2]]
			sos@dataFieldConverters <- .defaultMethods[[3]]
		})

#
# helper methods for exception response handling
#
.isExceptionReport <- function(document) {
	if(sosOwsExceptionReportRootName == xmlName(xmlRoot(document)))
		return(TRUE)
	else
		return(FALSE)
}
.handleExceptionReport <- function(sos, obj) {
	if(sos@verboseOutput) warning("Received ExceptionReport!")
	
	.parsingFunction <- sosParsers(sos)[[sosOwsExceptionReportRootName]]
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
# encoding function that just passes given content along...
#
setMethod("encodeXML", "XMLAbstractNode", 
		function(obj, verbose = FALSE) {
			if(verbose) {
				cat("ENCODE XML from XMLAbstractNode\n")
			}
			return(obj)
		}
)