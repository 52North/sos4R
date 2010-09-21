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
# Created: 2010-09-08                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

#
# construction methods
#
GmlTimeInstant <- function(timePosition, id = as.character(NA),
		relatedTimes = list(NA), frame = as.character(NA)) {
	new("GmlTimeInstant", timePosition = timePosition, id = id,
			relatedTimes = relatedTimes, frame = frame)
}

GmlTimePeriod <- function(begin = NULL, beginPosition = NULL, end = NULL,
		endPosition = NULL, duration = as.character(NA), timeInterval = NULL,
		id = as.character(NA), relatedTimes = list(NA),
		frame = as.character(NA)) {
	new("GmlTimePeriod", begin = begin, beginPosition = beginPosition,
			end = end, endPosition = endPosition, duration = duration,
			timeInterval = timeInterval, id = id, relatedTimes = relatedTimes,
			frame = frame)
}

GmlTimePeriod <- function(begin, end, duration = as.character(NA),
		timeInterval = NULL, id = as.character(NA), relatedTimes = list(NA),
		frame = as.character(NA)) {
	new("GmlTimePeriod", begin = begin, beginPosition = NULL,
			end = end, endPosition = NULL, duration = duration,
			timeInterval = timeInterval, id = id, relatedTimes = relatedTimes,
			frame = frame)
}

GmlTimePeriod <- function(beginPosition, endPosition,
		duration = as.character(NA), timeInterval = NULL, id = as.character(NA),
		relatedTimes = list(NA), frame = as.character(NA)) {
	new("GmlTimePeriod", begin = NULL, beginPosition = beginPosition,
			end = NULL, endPosition = endPosition, duration = duration,
			timeInterval = timeInterval, id = id, relatedTimes = relatedTimes,
			frame = frame)
}

GmlTimePosition <- function(time, frame = as.character(NA),
		calendarEraName = as.character(NA),
		indeterminatePosition = as.character(NA)) {
	new("GmlTimePosition", time = time, frame = frame,
			calendarEraName = calendarEraName,
			indeterminatePosition = indeterminatePosition)
}

GmlTimeInterval <- function(interval, unit, radix = NA, factor = NA) {
	new("GmlTimeInterval", interval = interval, unit = unit, radix = radix,
			factor = factor)
}

GmlFeatureProperty <- function(href = as.character(NA), feature = NA) {
	new("GmlFeatureProperty", href = href, feature = feature)	
}

GmlDirectPosition <- function(pos, srsName = as.character(NA),
		srsDimension = NA_integer_, axisLabels = as.character(NA), 
		uomLabels = as.character(NA)) {
	new("GmlDirectPosition", pos = pos, srsName = srsName,
			srsDimension = srsDimension, axisLabels = axisLabels,
			uomLabels = uomLabels)
}

GmlDirectPositionLatLon <- function(lat, lon, srsName = as.character(NA),
		srsDimension = NA_integer_, axisLabels = as.character(NA), 
		uomLabels = as.character(NA)) {
	new("GmlDirectPosition", pos = paste(lat, lon, sep = " "),
			srsName = srsName, srsDimension = srsDimension,
			axisLabels = axisLabels, uomLabels = uomLabels)
}

GmlPoint <- function(pos, id = as.character(NA), srsName = as.character(NA),
		srsDimension = NA_integer_, axisLabels = as.character(NA),
		uomLabels = as.character(NA)) {
	new("GmlPoint", pos = pos, id = id, srsName = srsName,
			srsDimension = srsDimension, axisLabels = axisLabels,
			uomLabels = uomLabels)
}

GmlPointProperty <- function(href = as.character(NA), point = NA) {
	new("GmlPointProperty", href = href, point = point)
}

GmlTimeInstantProperty <- function(href = as.character(NA), time = NA) {
	new("GmlTimeInstantProperty", href = href, time = time)
}

GmlEnvelope <- function(lowerCorner, upperCorner, srsName = as.character(NA),
		srsDimension = NA_integer_, axisLabels = as.character(NA),
		uomLabels = as.character(NA)) {
	new("GmlEnvelope", lowerCorner = lowerCorner, upperCorner = upperCorner,
			srsName = srsName, srsDimension = srsDimension,
			axisLabels = axisLabels, uomLabels = uomLabels)
}


################################################################################
# encoding methods

setMethod(f = "encodeXML",
		signature = signature(obj = "GmlTimeInstantProperty"),
		def = function(obj, verbose) {
			if(verbose) cat("Encoding XML GmlTimeInstantProperty with", toString(obj))
			
			stop("Function not implemented yet!")
		}
)

setMethod(f = "encodeXML",
		signature = signature(obj = "GmlTimeInstant"),
		def = function(obj, verbose) {
			if(verbose) cat("Encoding XML GmlTimeInstant with", toString(obj))
			
			.ti <- xmlNode(name = gmlTimeInstantName,
					namespace = gmlNamespacePrefix)
			.time <- encodeXML(obj@timePosition)
			.ti$children[[1]] <- .time
			
			return(.ti)
		}
)

setMethod(f = "encodeXML",
		signature = signature(obj = "GmlTimePosition"),
		def = function(obj, verbose) {
			if(verbose) cat("Encoding XML GmlTimePosition with", toString(obj))
			
			.tpos <- xmlNode(name = gmlTimePositionName,
					namespace = gmlNamespacePrefix)
			
			if( !is.na(obj@frame)) {
				.tpos <- addAttributes(node = .tpos,
						.attrs = c("frame" = obj@frame), append = TRUE)
			}
			if( !is.na(obj@calendarEraName)) {
				.tpos <- addAttributes(node = .tpos,
						.attrs = c("calendarEraName" = obj@calendarEraName),
						append = TRUE)
			}
			if( !is.na(obj@indeterminatePosition)) {
				.tpos <- addAttributes(node = .tpos,
						.attrs = c("indeterminatePosition" =
										obj@indeterminatePosition),
						append = TRUE)
			}
			
			# FIXME take time format from sos, also for kvp stuff!
			xmlValue(.tpos) <- format(x = obj@time,
					format = sosDefaultKVPTimeFormat)
			
			return(.tpos)
		}
)

setMethod(f = "encodeXML",
		signature = signature(obj = "GmlTimePeriod"),
		def = function(obj, verbose) {
			if(verbose) cat("Encoding XML GmlTimePeriod with", toString(obj))
			
			.tp <- xmlNode(name = gmlTimePeriodName,
					namespace = gmlNamespacePrefix)
			
			# switch cases: begin and end
			if(!is.null(obj@begin) && !is.null(obj@end)) {
				.begin <- xmlNode(name = gmlBeginName,
						namespace = gmlNamespacePrefix)
				.begin$children[[1]] <- encodeXML(obj@begin)
				.end <- xmlNode(name = gmlEndName,
						namespace = gmlNamespacePrefix)
				.end$children[[1]] <- encodeXML(obj@end)
				.tp <- addChildren(node = .tp, kids = list(.begin, .end))
			}
			# beginPosition and endPosition
			else if(!is.null(obj@beginPosition) && !is.null(obj@endPosition)) {
				.beginPosition <- encodeXML(obj@beginPosition)
				xmlName(.beginPosition) <- gmlBeginPositionName
				.endPosition <- encodeXML(obj@endPosition)
				xmlName(.endPosition) <- gmlEndPositionName
				.tp <- addChildren(node = .tp,
						kids = list(.beginPosition, .endPosition))
			}
			
			# time duration stuff: prefer duration over timeInterval
			if( !is.na(obj@duration) && !is.null(obj@timeInterval))
				warning("Can only add either duration or timeInterval to gml:TimePeriod, using durcation!")
			if( !is.na(obj@duration)) {
				.timeLength <- xmlNode(name = gmlTimeLengthName, 
						namespace = gmlNamespacePrefix)
				.duration <-  xmlNode(name = gmlDurationName, 
						namespace = gmlNamespacePrefix)
				xmlValue(.duration) <- obj@duration
				.timeLength$children[[1]] <- .duration
				.tp <- addChildren(node = .tp, kids = list(.timeLength))
			}
			else if( !is.null(obj@timeInterval)) {
				.timeLength <- xmlNode(name = gmlTimeLengthName, 
						namespace = gmlNamespacePrefix)
				.timeInterval <-  xmlNode(name = gmlTimeIntervalName, 
						namespace = gmlNamespacePrefix)
				
				print("lala here")
				print(obj)
				
				xmlValue(.timeInterval) <- obj@timeInterval@interval
				.timeInterval <- addAttributes(node = .timeInterval,
						.attrs = list("unit" = obj@timeInterval@unit))
				
				if(!is.na(obj@timeInterval@radix)) {
					.timeInterval <- addAttributes(node = .timeInterval,
							.attrs = list("radix" = obj@timeInterval@radix),
							append = TRUE)
				}
				if(!is.na(obj@timeInterval@factor)) {
					.timeInterval <- addAttributes(node = .timeInterval,
							.attrs = list("factor" = obj@timeInterval@factor),
							append = TRUE)
				}
				
				.timeLength$children[[1]] <- .timeInterval
				.tp <- addChildren(node = .tp, kids = list(.timeLength))
			}
			
			return(.tp)
		}
)

setMethod(f = "encodeXML",
		signature = signature(obj = "GmlEnvelope"),
		def = function(obj, verbose) {
			if(verbose) cat("Encoding XML GmlEnvelope with", toString(obj))
			
			.env <- xmlNode(name = gmlEnvelopeName,
					namespace = gmlNamespacePrefix)
			
			if( !is.na(obj@srsName)) {
				.env <- addAttributes(node = .env,
						.attrs = c("srsName" = obj@srsName), append = TRUE)
			}
			if( !is.na(obj@srsDimension)) {
				.env <- addAttributes(node = .env,
						.attrs = c("srsDimension" = obj@srsDimension),
						append = TRUE)
			}
			if( !is.na(obj@axisLabels)) {
				.env <- addAttributes(node = .env,
						.attrs = c("axisLabels" = obj@axisLabels),
						append = TRUE)
			}
			if( !is.na(obj@uomLabels)) {
				.env <- addAttributes(node = .env,
						.attrs = c("uomLabels" = obj@uomLabels),
						append = TRUE)
			}
			
			.lC <- encodeXML(obj@lowerCorner)
			xmlName(.lC) <- gmlLowerCornerName
			.uC <- encodeXML(obj@upperCorner)
			xmlName(.uC) <- gmlUpperCornerName
			
			.env$children[[1]] <- .lC
			.env$children[[2]] <- .uC
			
			return(.env)
		}
)

setMethod(f = "encodeXML",
		signature = signature(obj = "GmlDirectPosition"),
		def = function(obj, verbose) {
			if(verbose) cat("Encoding XML GmlDirectPosition with", toString(obj))
			
			.pos <- xmlNode(name = gmlPosName,
					namespace = gmlNamespacePrefix)
			xmlValue(.pos) <- obj@pos
			
			if( !is.na(obj@srsName)) {
				.pos <- addAttributes(node = .pos,
						.attrs = c("srsName" = obj@srsName), append = TRUE)
			}
			if( !is.na(obj@srsDimension)) {
				.pos <- addAttributes(node = .pos,
						.attrs = c("srsDimension" = obj@srsDimension),
						append = TRUE)
			}
			if( !is.na(obj@axisLabels)) {
				.pos <- addAttributes(node = .pos,
						.attrs = c("axisLabels" = obj@axisLabels),
						append = TRUE)
			}
			if( !is.na(obj@uomLabels)) {
				.pos <- addAttributes(node = .pos,
						.attrs = c("uomLabels" = obj@uomLabels), append = TRUE)
			}
			
			return(.pos)
		}
)

setMethod(f = "encodeXML",
		signature = signature(obj = "GmlPoint"),
		def = function(obj, verbose) {
			if(verbose) cat("Encoding XML GmlPoint with", toString(obj))
			
			.point <- xmlNode(name = gmlPointName,
					namespace = gmlNamespacePrefix)
			.pos <- encodeXML(obj@pos)
			.point$children[[1]] <- .pos
					
			return(.point)
		}
)

setMethod(f = "encodeXML",
		signature = signature(obj = "GmlLineString"),
		def = function(obj, verbose) {
			if(verbose) cat("Encoding XML GmlPoint with", toString(obj))
			
			warning("FUNCTION NOT IMPLEMENTED!")
		}
)

setMethod(f = "encodeXML",
		signature = signature(obj = "GmlPolygon"),
		def = function(obj, verbose) {
			if(verbose) cat("Encoding XML GmlPoint with", toString(obj))
			
			warning("FUNCTION NOT IMPLEMENTED!")
		}
)

setMethod(f = "encodeXML",
		signature = signature(obj = "GmlPointProperty"),
		def = function(obj, verbose) {
			if(verbose) cat("Encoding XML GmlPoint with", toString(obj))
			
			warning("FUNCTION NOT IMPLEMENTED!")
		}
)

