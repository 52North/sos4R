################################################################################
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
# Created: 2010-06-18                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
################################################################################

#
# The layer for swe:TimeObjectProperty for om:resultTime and om:samplingTime is
# removed in this class, as is om:featureOfInterest where gml:_Feature is used.
#
setClass("OmOM_Observation",
         representation(
           phenomenonTime = "GmlTimeObjectOrNULL",
           procedure = "ANY",
           observedProperty = "SwePhenomenonPropertyOrNULL",
           featureOfInterest = "GmlFeatureOrGmlFeaturePropertyOrNULL",
           result = "ANY",
           # optional:
           metadata = "ANY",
           resultTime = "GmlTimeObjectOrNULL",
           resultQuality = "ANY",
           parameter = "ANY"),
         #type?!
         prototype = list(phenomenonTime = NULL, procedure = as.character(NA),
                          observedProperty = NULL, featureOfInterest = NULL,
                          result = NULL),
         validity = function(object) {
           #print("Entering validation: OmObservation")
           # TODO implement validity function
           # result time should be GmlTimeObject
           return(TRUE)
         }
)
setClassUnion(name = "OmOM_ObservationOrNULL",
              members = c("OmOM_Observation", "NULL"))

#
#
#
setClass("OmObservationProperty",
         representation(href = "character",
                        obs = "OmObservationOrNULL"),
         #prototype = list(),
         validity = function(object) {
           #print("Entering validation: OmObservationProperty")
           # TODO implement validity function
           # one of parameters has to be set
           return(TRUE)
         }
)

#
#
#
setClass("OmMeasurement",
         representation(result = "GmlMeasure"),
         contains = "OmObservation",
         validity = function(object) {
           #print("Entering validation: OmMeasurement")
           # TODO implement validity function
           return(TRUE)
         }
)

setClassUnion(name = "FoiOrNULL",
              members = c("GmlFeatureOrGmlFeaturePropertyOrNULL", "MonitoringPoint", "NULL"))

