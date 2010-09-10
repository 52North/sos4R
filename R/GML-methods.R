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

GmlTimePeriod <- function(begin = NA, beginPosition = NA, end = NA,
		endPosition = NA, duration = as.character(NA), timeInterval = NA,
		id = as.character(NA), relatedTimes = list(NA), frame = as.character(NA)) {
	new("GmlTimePeriod", begin = begin, beginPosition = beginPosition,
			end = end, endPosition = endPosition, duration = duration,
			timeInterval = timeInterval, id = id, relatedTimes = relatedTimes,
			frame = frame)
}

GmlTimePeriod <- function(begin, end, duration = as.character(NA),
		timeInterval = NA, id = as.character(NA), relatedTimes = list(NA),
		frame = as.character(NA)) {
	new("GmlTimePeriod", begin = begin, beginPosition = NA,
			end = end, endPosition = NA, duration = duration,
			timeInterval = timeInterval, id = id, relatedTimes = relatedTimes,
			frame = frame)
}

GmlTimePeriod <- function(beginPosition, endPosition,
		duration = as.character(NA), timeInterval = NA, id = as.character(NA),
		relatedTimes = list(NA), frame = as.character(NA)) {
	new("GmlTimePeriod", begin = NA, beginPosition = beginPosition,
			end = NA, endPosition = endPosition, duration = duration,
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
	new("GmlTimeInteval", interval = interval, unit = unit, radix = radix,
			factor = factor)
}

GmlFeatureProperty <- function(href = as.character(NA), feature = NA) {
	new("GmlFeatureProperty", href = href, feature = feature)	
}

GmlDirectPosition <- function(pos, srsName = as.character(NA),
		srsDimension = as.character(NA), axisLabels = as.character(NA), 
		uomLabels = as.character(NA)) {
	new("GmlDirectPosition", pos = pos, srsName = srsName,
			srsDimension = srsDimension, axisLabels = axisLabels,
			uomLabels = uomLabels)
}

GmlPoint <- function(pos, id = as.character(NA), srsName = as.character(NA),
		srsDimension = as.character(NA), axisLabels = as.character(NA),
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
