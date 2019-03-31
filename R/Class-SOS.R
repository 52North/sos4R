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
# Created: 2013-08-28                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
################################################################################

#
# SOS ----
#
setClass("SOS",
         representation(version = "character",
                        capabilities = "OwsCapabilities", parsers = "list",
                        encoders = "list", dataFieldConverters = "list",
                        timeFormat = "character", verboseOutput = "logical",
                        switchCoordinates = "logical", useDCPs = "logical",
                        dcpFilter = "list", additionalKVPs = "list"),
         contains = c("VIRTUAL"))

#
# SOS_Test ----
# class for local testing, i.e. without an URL and default verbose output
#
setClass("SOS_Test",
         representation(name = "character", binding = "character"),
         prototype = list(name = as.character(NA)),
         contains = c("SOS"),
         validity = function(object) {
           #print("Entering validation: SOS_Test")
           return(TRUE)
         }
)

SOS_Test <- function(name = "test",
                     binding = SosDefaultBinding(),
                     version = sos100_version,
                     parsers = SosParsingFunctions(),
                     encoders = SosEncodingFunctions(),
                     dataFieldConverters = SosDataFieldConvertingFunctions(),
                     timeFormat = sosDefaultTimeFormat,
                     verboseOutput = FALSE,
                     switchCoordinates = FALSE,
                     useDCPs = TRUE,
                     dcpFilter = SosDefaultDCPs(),
                     additionalKVPs = list(),
                     ...) {

  .sos <- new("SOS_Test",
              name = name,
              binding = binding,
              version = version,
              capabilities = new("OwsCapabilities", version = "NA",
                                 updateSequence = as.character(NA),
                                 owsVersion = sosDefaultGetCapOwsVersion),
              parsers = parsers,
              encoders = encoders,
              dataFieldConverters = dataFieldConverters,
              timeFormat = timeFormat,
              verboseOutput = verboseOutput,
              switchCoordinates = switchCoordinates,
              useDCPs = useDCPs,
              dcpFilter = dcpFilter,
              additionalKVPs = additionalKVPs)

  if(verboseOutput) cat("[SOS] Created new SOS_Test:\n", toString(.sos), "\n")
  return(.sos)
}


#
# SosFeatureOfInterest ----
#
setClass("SosFeatureOfInterest",
         representation(
           objectIDs = "list",
           spatialOps = "OgcSpatialOpsOrNULL"),
         prototype = list(
           objectIDs = list(NA),
           spatialOps = NULL),
         validity = function(object) {
           #print("Entering validation: GetObservationById")
           # TODO implement validity function
           # one of objectIDs or spatialOps has to be set
           return(TRUE)
         }
)
setClassUnion(name = "SosFeatureOfInterestOrNULL",
              members = c("SosFeatureOfInterest", "NULL"))


#
# SosDescribeSensor ----
# See OGC 06-009r6
#
setClass("SosDescribeSensor",
         representation(procedure = "character", outputFormat = "character"),
         prototype = list(service = as.character(NA), version = as.character(NA),
                          procedure = as.character(NA), outputFormat = as.character(NA)),
         contains = "OwsServiceOperation",
         validity = function(object) {
           #print("Entering validation: sosDescribeSensor")
           # TODO implement validity function
           # check format of version, sensorid and outputformat?!
           if(length(object@procedure) > 1)
             return("can only request one procedure at a time!")

           return(TRUE)
         }
)

#
# SosGetObservation ----
# See SOS specification, OGC 06-009r6, section 8.4
# Includes extensions for SOS 2.0
#
setClass("SosGetObservation",
         representation(
           offering = "character",
           observedProperty = "list",
           responseFormat = "character",
           srsName = "character",
           eventTime = "list",
           procedure = "character",
           featureOfInterest = "SosFeatureOfInterestOrNULL",
           result = "OgcComparisonOpsOrXMLOrNULL",
           resultModel = "character",
           responseMode = "character",
           BBOX = "character",
           valueReferenceTemporalFilter = "character" # SOS 2.0, Requirement 117
         ),
         prototype = list(
           service = as.character(NA),
           version = as.character(NA),
           offering = as.character(NA),
           observedProperty = list(NA),
           responseFormat = as.character(NA)),
         contains = "OwsServiceOperation",
         validity = function(object) {
           #print("Entering validation: SosGetObservation")
           # TODO implement validity function

           # service, version, offering, observedProperty, and responseFormat are mandatory
           if(is.na(object@service))
             return("service parameter must be given")
           if(is.na(object@version))
             return("version must be given")
           if(is.na(object@offering))
             return("offering parameter must be given")
           # responseFormat is optional for GET
           #if(is.na(object@responseFormat))
           #	return("responseFormat parameter must be given")
           if(length(object@observedProperty) < 1)
             return("at least one observedProperty is mandatory")

           # if version is there, it hast to be in a certain format, see ows common
           # srsName, offering, procedure, observedProperty are anyURIs
           if (length(object@eventTime) > 0)
             if (!all(sapply(object@eventTime, inherits, what = "SosEventTime")))
               return("all elements of the eventTime list must extend SosEventTime")

           # featureOfInterest is null or a SosFeatureOfInterest element

           # result is null or an ogc:comparisonOps element
           cls <- class(slot(object, "result"))
           #			print(paste("class of result slot: ", cls))
           if ( !any(cls %in% c("OgcComparisonOps", "xml_document", "xml_node", "NULL"))) {
             return("'response' argument does not have allowed class!")
           }

           # responseFormat must be MIME content type
           # resultModel must be a QName
           # responseMode must be one of inline, out-of-band, attached, or resultTemplate
           return(TRUE)
         }
)
