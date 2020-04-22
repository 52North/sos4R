############################################################################## #
# Copyright (C) 2019 by 52 North                                               #
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
# Created: 2013-08-28                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #


#
# SOS_2.0.0 ----
#
setClass("SOS_2.0.0",
         representation(url = "character",
                        binding = "character"),
         prototype = list(
           url = as.character(NA),
           binding = as.character(NA),
           version = as.character(NA)),
         contains = c("SOS"),
         validity = function(object) {
           #print("Entering validation: SOS")

           if (!any(sapply(SosSupportedBindings(), "==", object@binding), na.rm = TRUE)) {
             return(paste("Binding has to be one of",
                          toString(SosSupportedBindings()),
                          "- given:", object@binding))
           }

           if (object@version != sos200_version)
             return(paste0("Version must be 2.0.0 but is", object@version))

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
setClassUnion(name = "SOS_versioned",	members = "SOS_2.0.0")

#
# SosCapabilities_2.0.0 ----
#
setClass("SosCapabilities_2.0.0",
         representation(filterCapabilities = "SosFilter_CapabilitiesOrNULL"),
         contains = "OwsCapabilities_1.1.0",
         validity = function(object) {
           #print("Entering validation: SosCapabilities_1.0.0")
           # TODO implement validity function
           return(TRUE)
         }
)

#
# SosObservationOffering_2.0.0 ----
#
# See OGC 12-006, clause 8.1.2.2
#
# If possible, character vectors are used for slots if elements can be
# represented by character strings.
# TODO: FeatureRelationship
setClass("SosObservationOffering_2.0.0",
         representation(id = "character",
                        name = "character",
                        resultTime = "GmlTimeGeometricPrimitive",
                        phenomenonTime = "GmlTimeGeometricPrimitive",
                        procedure = "character",
                        observableProperty = "list",
                        featureOfInterestType = "list",
                        observationType = "list",
                        observedArea = "list",
                        procedureDescriptionFormat = "list",
                        responseFormat = "list"),
         prototype = list(id = as.character(NA),
                          name = as.character(NA),
                          resultTime = NULL,
                          phenomenonTime = NULL,
                          procedure = as.character(NA),
                          observableProperty = list(NA),
                          featureOfInterestType = list(NA),
                          observationType = list(NA),
                          observedArea = NULL,
                          procedureDescriptionFormat = list(NA),
                          responseFormat = list(NA)),
         validity = function(object) {
           #print("Entering validation: ObservationOffering")
           # TODO implement validity function

           # time is required
           # procedure, observedProperty, featureOfInterest, responseFormat are all "one or more"
           return(TRUE)
         }
)

#
# SosGetObservation_2.0.0 ----
#
# See SOS specification, OGC 06-009r6, section 8.4
#
setClass("SosGetObservation_2.0.0",
         representation(
           offering = "list",
           observedProperty = "list",
           responseFormat = "character",
           temporalFilter = "list",
           procedure = "list",
           featureOfInterest = "list",
           spatialFilter = "SamsShapeOrNULL",
           valueReferenceTemporalFilter = "character"),
         prototype = list(
           service = as.character(NA),
           version = as.character(NA)
           ),
         contains = "OwsServiceOperation",
         validity = function(object) {
           if (is.na(object@service))
             return("service parameter must be given")
           if (is.na(object@version))
             return("version must be given")

           # all temporalFilter items must be GmlTimeObject

           return(TRUE)
         }
)

#
# SosGetFeatureOfInterest_2.0.0 ----
#
# See SOS specification, OGC 12-006, section 9.1
#
setClass("SosGetFeatureOfInterest_2.0.0",
         representation(
           featureOfInterest = "character"),
         prototype = list(
           service = as.character(NA),
           version = as.character(NA),
           featureOfInterest = as.character(NA)),
         contains = "OwsServiceOperation",
         validity = function(object) {
           # service, version are mandatory
           if (is.na(object@service))
             return("service parameter must be given")
           if (is.na(object@version))
             return("version must be given")

           return(TRUE)
         }
)
