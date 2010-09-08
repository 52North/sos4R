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

parseOM <- function(om) {
	result <- NULL
	
	# ...
	if(inherits(om, "XMLInternalDocument"))
		root <- xmlRoot(om)
	else root <- om
	
	# switch submethods based on name
	rootName <- xmlName(root)
	print(paste("parsing O&M", rootName))
	
	if(rootName == "ObservationCollection") {
		result <- .parseObservationCollection(root)
	}
	else if(rootName == "Observation") {
		result <- .parseObservation(root)
	}
	else if(rootName == "Measurement") {
		result <- .parseMeasurement(root)
	}
	else if(rootName == "member") {
		result <- .parseMember(root)
	}
	else {
		warning(paste("No parsing function for given OM element ", rootName))
	}
		
	return(result)
}

#
#
#
.parseObservationCollection <- function(oc) {
	return(xmlApply(oc, parseOM))
}

#
#
#
.parseMember <- function(member) {
	return(xmlApply(member, parseOM))
}

#
#
#
.parseMeasurement <- function(m) {
	.samplingTime <- .parseSamplingTime(m[["samplingTime"]])
	.procedure <-
	.observedProperty <-
	.featureOfInterest <-
	
	
	# must be OmMeasure
	.result <-
	
	# TODO optionals elements for OmMeasurement
	.metadata
	.resultTime
	.resultQuality
	.parameter
	
	.measurement <- OmMeasurement(samplingTime = .samplingTime,
			procedure = .procedure, observedProperty = .observedProperty,
			featureOfInterest = .featureOfInterest, result = .result)
	
	print(.measurement)
	
	.measurement
}

#
# create according GmlTimeObject from om:samplingTime
#
.parseSamplingTime <- function(st) {
	if(xmlName(st) != "samplingTime") {
		warning(paste("Illegal argument for .parseSamplingTime: ", st))
		return(NULL)
	}
	.timeObject = NULL
	
	.ti <- xmlChildren(st)[["TimeInstant"]]
	.tp <- xmlChildren(st)[["TimePeriod"]]
	if(.ti != NULL) {
		.timeObject <- .parseTimePosition(.ti)
		
		#optionals
		.attrs <- xmlAttrs(.ti)
		
		.id = as.character(NA)
		.relatedTime = list(NA)
		.frame = as.character(NA)
		
		if(!is.na(.attrs["id"]))
			.id <- is.na(.attrs["id"])
		if(!is.na(.attrs["frame"]))
			.frame <- is.na(.attrs["frame"])
		.relatedTime <- xmlChildren(.ti)["relatedTime"]
		
		
		.timeObject <- GmlTimeInstant(timePosition = .timeObject, id = .id,
				relatedTime = .relatedTime, frame = .frame)
	}
	else if(.tp != NULL) {
		# TODO parse time period
	}
	
	print(.timeObject)
	
	return(.timeObject)
}

.parseTimePosition <- function(tp) {
	.attrs <- xmlAttrs(tp)
	
	# TODO remove "T" ???
	.time <- as.POSIXct(xmlValue(tp))
	
	# optional:
	.frame = as.character(NA)
	.calendarEraName = as.character(NA)
	.indeterminatePosition = as.character(NA)
	
	if(!is.null(.attrs)) {
		if(!is.na(.attrs[["frame"]]))
			.frame <- is.na(.attrs[["frame"]])
		if(!is.na(.attrs["calendarEraName"]))
			.calendarEraName <- is.na(.attrs["calendarEraName"])
		if(!is.na(.attrs["indeterminatePosition"]))
			.indeterminatePosition <- is.na(.attrs["indeterminatePosition"])
	}
	
	.timePosition <- GmlTimePosition(time = .time, frame = .frame,
			calendarEraName = .calendarEraName,
			indeterminatePosition = .indeterminatePosition)
}