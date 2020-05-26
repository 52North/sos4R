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
# visit the Free Software Foundation web page, http://www.fsf.org              #
#                                                                              #
# Author: Eike Hinderk Jürrens (e.h.juerrens@52north.org)                      #
# Created: 2018-11-23                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
#
# SosGetDataAvailability_1.0.0 ----
#
# See SOS 2.0 Hydrology profile specification, OGC 14-004r1, section 7.4
#
setClass("SosGetDataAvailability_1.0.0",
         representation(
           procedures = "list",
           observedProperties = "list",
           featuresOfInterest = "list",
           offerings = "list"),
         prototype = list(
           service = as.character(NA),
           version = as.character(NA),
           procedures = list(NA),
           observedProperties = list(NA),
           featuresOfInterest = list(NA),
           offerings = list(NA)
          ),
         contains = "OwsServiceOperation",
         validity = function(object) {
           #print("Entering validation: SosGetDataAvailability_1.0.0")

           # service, version are mandatory
           if(is.na(object@service))
             return("service parameter must be given")
           if(is.na(object@version))
             return("version must be given")

           return(TRUE)
         }
)

.toString.SosGetDataAvailability_1.0.0 <- function(x, ...) {
  paste("Object of class SosGetDataAvailability_1.0.0:",
        "\n  Procedures           : ", paste0(x@procedures, collapse = ", "),
        "\n  Observed Properties  : ", paste0(x@observedProperties, collapse = ", "),
        "\n  Features Of Interest : ", paste0(x@featuresOfInterest, collapse = ", "),
        "\n  Offerings            : ", paste0(x@offerings, collapse = ", "))
}
setMethod("toString", "SosGetDataAvailability_1.0.0", function(x, ...) .toString.SosGetDataAvailability_1.0.0(x, ...))

#
# convience constructor
#
SosGetDataAvailability_1.0.0 <- function(
  service,
  version,
  observedProperties = list(NA),
  procedures = list(NA),
  featuresOfInterest = list(NA),
  offerings = list(NA)) {
  new("SosGetDataAvailability_1.0.0",
      request = sosGetDataAvailabilityName,
      service = service,
      version = version,
      observedProperties = observedProperties,
      procedures = procedures,
      featuresOfInterest = featuresOfInterest,
      offerings = offerings)
}

#
# class DataAvailabilityMember ----
#
# See SOS 2.0 Hydrology profile specification, OGC 14-004r1, section 7.4, requirement 12
#
setClass("DataAvailabilityMember",
         representation(
           procedure = "character",
           observedProperty = "character",
           featureOfInterest = "character",
           phenomenonTime = "GmlTimePeriod"),
         prototype = list(
           procedure = as.character(NA),
           observedProperty = as.character(NA),
           featureOfInterest = as.character(NA),
           phenomenonTime = NULL
         ),
         validity = function(object) {
           #print("Entering validation: DataAvailabilityMember")

           if(is.na(object@procedure))
             return("procedure parameter must be given")
           if(is.na(object@observedProperty))
             return("observed property must be given")
           if (is.na(object@featureOfInterest))
             return("feature of interest must be given")
           if(is.null(object@phenomenonTime))
             return("phenomenon time must be given")

           return(TRUE)
         }
)

.toString.DataAvailabilityMember <- function(x, ...) {
paste("Object of class DataAvailabilityMember:",
      "\n  Procedure            : ", paste0(x@procedure),
      "\n  Observed Property    : ", paste0(x@observedProperty),
      "\n  Features Of Interest : ", paste0(x@featureOfInterest),
      "\n  Phenomenon Time      : ", toString(x@phenomenonTime))
}
.print.DataAvailabilityMember <- function(x, ...) {
  cat(.toString.DataAvailabilityMember(x, ...), "\n")
  invisible(x)
}
setMethod("toString", "DataAvailabilityMember", function(x, ...) .toString.DataAvailabilityMember(x, ...))
setMethod("print", "DataAvailabilityMember", function(x, ...) .print.DataAvailabilityMember(x, ...))
setMethod("show", "DataAvailabilityMember", function(object) .print.DataAvailabilityMember(object))

#
# convience constructor
#
DataAvailabilityMember <- function(procedure = NA,
                                   observedProperty = NA,
                                   featureOfInterest = NA,
                                   phenomenonTime = NULL) {
  new("DataAvailabilityMember",
      procedure = procedure,
      observedProperty = observedProperty,
      featureOfInterest = featureOfInterest,
      phenomenonTime = phenomenonTime)
}
