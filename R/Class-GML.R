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

# TODO add reference to GML spec

#
# abstract super classes of the actually needed TimeInstant and TimePeriod
#
setClass("GmlTimeObject",
		representation(# optional:
				id = "character"),
		contains = c("VIRTUAL"),
		validity = function(object) {
			print("Entering validation: GmlTime")
			# TODO implement validity function
			return(TRUE)
		}
)

setClass("GmlTimePrimitive",
		representation(# optional:
				relatedTimes = "list"),
		prototype = list(relatedTimes = list()),
		contains = c("VIRTUAL", "GmlTimeObject"),
		validity = function(object) {
			print("Entering validation: GmlTimePrimitive")
			# TODO implement validity function
			# related times must be GmlTimePrimitive
			return(TRUE)
		}
)

setClass("GmlTimeGeometricPrimitive",
		representation(# optional:
				frame = "character"),
		contains = c("VIRTUAL", "GmlTimePrimitive"),
		validity = function(object) {
			print("Entering validation: GmlTimeGeometricPrimitive")
			# TODO implement validity function
			return(TRUE)
		}
)

#
# GmlTimePosition wraps a POSIXct
#
setClass("GmlTimePosition",
		representation(time = "POSIXt",
				# optional:
				frame = "character",
				calendarEraName = "character",
				indeterminatePosition = "character"
		),
		prototype = list(timePosition = as.character(NA)),
		validity = function(object) {
			print("Entering validation: GmlTimePosition")
			# TODO implement validity function
			# time needs to be set			
			return(TRUE)
		}		
)

#
#
#
setClass("GmlTimeInstant",
		representation(timePosition = "GmlTimePosition"),
		prototype = list(timePosition = NULL),
		contains = "GmlTimeGeometricPrimitive",
		validity = function(object) {
			print("Entering validation: GmlTimeInstant")
			# TODO implement validity function
			# timePosition needs to be set			
			return(TRUE)
		}		
)

#
#
#
setClass("GmlTimeInstantProperty",
		representation(href = "character", time = "ANY"),
		prototype = list(href = as.character(NA), time = NA),
		contains = "GmlTimeGeometricPrimitive",
		validity = function(object) {
			print("Entering validation: GmlTimeInstant")
			# TODO implement validity function
			# timePosition needs to be set			
			return(TRUE)
		}
)

#
#
#
setClass("GmlTimeInterval",
		representation(interval = "character",
				unit = "character",
				# optional:
				radix = "integer",
				factor = "integer"
		),
		prototype = list(interval = as.character(NA), unit = as.character(NA)),
		validity = function(object) {
			print("Entering validation: GmlTimeInterval")
			# TODO implement validity function
			# interval and unit need to be set, radix must be positive
			return(TRUE)
		}		
)

#
#
#
setClass("GmlTimePeriod",
		representation(begin = "ANY",
				beginPosition = "ANY",
				end = "ANY",
				endPosition = "ANY",
				# optional:
				# for brevity, the TimeDurationType layer is removed here
				duration = "character",
				timeInterval = "ANY"
		),
		prototype = list(begin = NA, beginPosition = NA, end = NA, 
				endPosition = NA),
		contains = "GmlTimeGeometricPrimitive",
		validity = function(object) {
			print("Entering validation: GmlTimeInstant")
			# TODO implement validity function
			# either both begin and end, or beginPosition and endPosition need to be set.
			# only one of the optional duration and timeInterval can be set!
			# begin and end need to be of class GmlTimeInstantProperty
			# beginPosition and endPosition need to be of class GmlTimePosition
			return(TRUE)
		}		
)

#
#
#
setClass("GmlFeature",
		representation(id = "character"),
		contains = "VIRTUAL",
		#prototype = list(),
		validity = function(object) {
			print("Entering validation: GmlFeature")
			return(TRUE)
		}
)

#
#
#
setClass("GmlFeatureProperty",
		representation(href = "character",	
				feature = "ANY"),
		prototype = list(href = as.character(NA), feature = NA),
		validity = function(object) {
			print("Entering validation: GmlFeatureProperty")
			# TODO implement validity function
			# one of parameters has to be set
			return(TRUE)
		}
)

#
#
#
setClass("GmlDirectPosition",
		representation(pos = "character",
				# optional:
				srsName = "character",
				srsDimension = "integer",
				axisLabels = "character",
				uomLabels = "character"),
		prototype = list(pos = as.character(NA)),
		validity = function(object) {
			print("Entering validation: GmlDirectPosition")
			# TODO implement validity function
			# pos string is required
			return(TRUE)
		}
)

#
# gml:coordinates and gml:coord are deprecated
#
setClass("GmlPoint",
		representation(pos = "GmlDirectPosition",
				# optional:
				id = "character",
				srsName = "character",
				srsDimension = "integer",
				axisLabels = "character",
				uomLabels = "character"),
		prototype = list(pos = NULL),
		validity = function(object) {
			print("Entering validation: GmlPoint")
			# TODO implement validity function
			# one of parameters has to be set
			return(TRUE)
		}
)

#
#
#
setClass("GmlPointProperty",
		representation(href = "character",	
				point = "ANY"),
		#prototype = list(),
		validity = function(object) {
			print("Entering validation: GmlPointProperty")
			# TODO implement validity function
			# one of parameters has to be set, point has to be NA or GmlPoint
			return(TRUE)
		}
)
