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

#
# See SOS specification, OGC 06-009r6, section 8.4
#
setClass("GetObservation",
		representation(
				offering = "character",
				observedProperty = "list",
				responseFormat = "character",
				srsName = "character",
				eventTime = "list", 
				procedure = "list", 
				featureOfInterest = "ANY", # SosFeatureOfInterest 
				result = "character",
				resultModel = "character",
				responseMode = "character",
				BBOX = "character"),
		prototype = list(
				service = as.character(NA),
				version = as.character(NA),
				offering = as.character(NA),
				observedProperty = list(NA),
				responseFormat = as.character(NA)), # prototype should not pass validity
		contains = "OwsServiceOperation",
		validity = function(object) {
			#print("Entering validation: GetObservation")
			# TODO implement validity function
			
			# service, version, offering, observedProperty, and responseFormat are mandatory
			if(is.na(object@service))
				return("service parameter must be given")
			if(is.na(object@version))
				return("version must be given")
			if(is.na(object@offering))
				return("offering parameter must be given")
			if(is.na(object@responseFormat))
				return("responseFormat parameter must be given")
			if(length(object@observedProperty) < 1)
				return("at least one observedProperty is mandatory")
			
			# if version is there, it hast to be in a certain format, see ows common
			
			# srsName, offering, procedure, observedProperty are anyURIs
			
			# eventTime is a list of ogc:temporalOps
			
			# featureOfInterest is a SosFeatureOfInterest element
			
			# result is an ogc:comparisonOps element
			
			# responseFormat must be MIME content type
			
			# resultModel must be a QName
			
			# TODO responseMode must be one of inline, out-of-band, attached, or resultTemplate
			
			return(TRUE)
		}
)

#
# See SOS specification, OGC 06-009r6, section 10.1
#
setClass("GetObservationById",
		representation(
				observationId = "character",
				responseFormat = "character",
				srsName = "character",
				resultModel = "character",
				responseMode = "character"),
		prototype = list(
				observationId = as.character(NA),
				responseFormat = as.character(NA)),
		contains = "OwsServiceOperation",
		validity = function(object) {
			#print("Entering validation: GetObservationById")
			# TODO implement validity function
			
			# service, version, observationId, and responseFormat are mandatory
			if(is.na(object@service))
				return("service parameter must be given")
			if(is.na(object@version))
				return("version must be given")
			if(is.na(object@observationId))
				return("observationId parameter must be given")
			if(is.na(object@responseFormat))
				return("responseFormat parameter must be given")
			
			# if version is there, it hast to be in a certain format, see ows common
			
			# responseFormat must be MIME content type
			# resultModel must be a QName
			# TODO responseMode must be one of inline, out-of-band, attached, or resultTemplate
			
			return(TRUE)
		}
)
