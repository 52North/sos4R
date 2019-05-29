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
# Author: Benjamin Pross (b.pross@52north.org)                                 #
# Created: 2016-01-27                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

#
# Classes are based on WaterML 2.0
#
# http://www.opengeospatial.org/standards/waterml
#

#
# MonitoringPoint ----
# http://schemas.opengis.net/waterml/2.0/monitoringPoint.xsd
#
setClass("WmlMonitoringPoint",
         representation(sampledFeatures = "character",
                        id = "character",
                        identifier = "character",
                        names = "character",
                        shape = "SamsShape",
                        verticalDatums = "ANY",
                        timeZone = "ANY"),
         prototype = list(sampledFeatures = c(NA_character_), shape = NULL),
         contains = "GmlFeature",
         validity = function(object) {
           #print("Entering validation: MonitoringPoint")

           if (!class(slot(object, "verticalDatums")) %in% c("xml_nodeset", "xml_missing")) {
             return("'verticalDatums' must be an XML node set or missing.")
           }
           if (!class(slot(object, "timeZone")) %in% c("xml_node", "xml_missing")) {
             return("'verticalDatums' must be an XML node or missing.")
           }

           return(TRUE)
         }
)

#
# MeasurementTimeseries and Timeseries ----
# http://schemas.opengis.net/waterml/2.0/timeseries.xsd
#
setClass("WmlTimeseries",
         representation(),
         prototype = list(),
         validity = function(object) {
           return(TRUE)
         }
)

setClass("WmlMeasurementTimeseries",
         representation(),
         prototype = list(),
         contains = "WmlTimeseries",
         validity = function(object) {
           return(TRUE)
         }
)
