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
#
#
setClass("SOS",
		representation(url = "character", method = "character",
				version = "character", capabilities = "OwsCapabilities",
				verboseOutput = "logical"),
		prototype = list(
				url = as.character(NA),
				method = as.character(NA),
				version = as.character(NA)),
		validity = function(object) {
			print("Entering validation: SOS")
			
			# method has to be one of GET, POST, SOAP
			.allowedMethods = c("GET" , "POST", "SOAP")
			if(!any(sapply(.allowedMethods, "==", object@method), na.rm = TRUE))
				return(paste("method has to be one of", paste(.allowedMethods, sep=", ", collapse = " ")))
				
			# version has to be one of the listed ones
			.allowedVersions = c("1.0.0")
			if(!any(sapply(.allowedVersions, "==", object@version), na.rm = TRUE))
				return(paste("version has to be one of", paste(.allowedVersions, sep=", ", collapse = " ")))
			
			# url has to match an URL pattern
			.urlPattern = "(?:https?://(?:(?:(?:(?:(?:[a-zA-Z\\d](?:(?:[a-zA-Z\\d]|-)*[a-zA-Z\\d])?)\\.)*(?:[a-zA-Z](?:(?:[a-zA-Z\\d]|-)*[a-zA-Z\\d])?))|(?:(?:\\d+)(?:\\.(?:\\d+)){3}))(?::(?:\\d+))?)(?:/(?:(?:(?:(?:[a-zA-Z\\d$\\-_.+!*'(),]|(?:%[a-fA-F\\d]{2}))|[;:@&=])*)(?:/(?:(?:(?:[a-zA-Z\\d$\\-_.+!*'(),]|(?:%[a-fA-F\\d]{2}))|[;:@&=])*))*)(?:\\?(?:(?:(?:[a-zA-Z\\d$\\-_.+!*'(),]|(?:%[a-fA-F\\d]{2}))|[;:@&=])*))?)?)"
			.result = regexpr(.urlPattern, object@url)
			if (.result == -1)
				return("url not matching URL-pattern (http://www.example.com)")
			
			# test for complete match removed, does not work yet
			#.urlLength = nchar(object@url)
			#if (.urlLength == attr(.result, "match.length"))
			#	return("url not completely matching URL-pattern")
				
			return(TRUE)
		}
)

#
#
#
setClass("SosFilter_Capabilities",
		representation(xml = "XMLAbstractNode"),
		prototype = list(xml = NULL),
		validity = function(object) {
			print("Entering validation: SosFilter_Capabilities")
			# TODO implement validity function
			return(TRUE)
		}
)

#
# See OWS Common 1.1.0, OGC 06-121r3
#
setClass("SosCapabilities_1.1.0",
		representation(filterCaps = "SosFilter_Capabilities"),
		prototype = list(filterCaps = NULL),
		contains = "OwsCapabilities_1.1.0",
		validity = function(object) {
			print("Entering validation: SosCapabilities_1.1.0")
			# TODO implement validity function
			return(TRUE)
		}
)

#
# See OGC 06-009r6, clause 8.2.3.2
#
setClass("SosObservationOffering",
		representation(id = "character", name = "character",
				time = "vector", procedure = "vector",
				observedProperty = "vector", featureOfInterest = "vector",
				responseFormat = "vector", intendedApplication = "character",
				resultModel = "vector", responseMode = "vector",
				boundedBy = "vector"),
		prototype = list(id = as.character(NA), time = as.character(NA),
				procedure = c(NA), observedProperty = c(NA),
				featureOfInterest = c(NA), responseFormat = c(NA)),
		validity = function(object) {
			print("Entering validation: ObservationOffering")
			# TODO implement validity function
			
			# time is required
			# procedure, observedProperty, featureOfInterest, responseFormat are all "one or more"
			
			return(TRUE)
		}
)


#
# See OGC 06-009r6, clause 8.2.3.2
#
setClass("SosContents",
		representation(observationOfferings = "vector"),
		prototype = list(observationOfferings = c(NA), xml = xmlNode(NA)),
		contains = "OwsContents",
		validity = function(object) {
			print("Entering validation: SosContents")
			# TODO implement validity function
			return(TRUE)
		}
)
