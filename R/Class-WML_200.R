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
#         Eike Hinderk Jürrens (e.h.juerrens@52north.org)                      #
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
# class MonitoringPoint ----
#
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
# constructor WmlMonitoringPoint
#
WmlMonitoringPoint <- function(sampledFeatures,
                               id,
                               identifier,
                               names,
                               shape,
                               verticalDatums = xml2::xml_missing(),
                               timeZone = xml2::xml_missing()) {
  new("WmlMonitoringPoint",
      sampledFeatures = sampledFeatures,
      id = id,
      identifier = identifier,
      names = names,
      shape = shape,
      verticalDatums = verticalDatums,
      timeZone = timeZone)
}
#
# class WmlMeasurementTimeseriesMetadata ----
#
setClass("WmlMeasurementTimeseriesMetadata",
         representation(
           temporalExtent = "GmlTimeGeometricPrimitive"
         ),
         prototype = list(),
         validity = function(object) {
           stopifnot(isS4(object@temporalExtent), is(object@temporalExtent, "GmlTimeGeometricPrimitive"))
           return(TRUE)
         }
)
#
# constructor WmlMeasurementTimeseriesMetadata
#
WmlMeasurementTimeseriesMetadata <- function(temporalExtent) {
  new("WmlMeasurementTimeseriesMetadata",
      temporalExtent = temporalExtent)
}
#
# class WmlMeasurementTVP ----
#
setClass("WmlMeasurementTVP",
         representation(
           time = "POSIXct",
           value = "numeric"
         ),
         prototype = list(),
         validity = function(object) {
           stopifnot(!is.na(object@time), is(object@time, "POSIXct"))
           stopifnot(!is.na(object@value), is(object@value, "numeric"), !is.nan(object@value))
           return(TRUE)
         }
)
#
# constructor WmlMeasurementTVP
#
WmlMeasurementTVP <- function(time, value) {
  new("WmlMeasurementTVP",
      time = time,
      value = value)
}
#
# class WmlInterpolationType ----
#
setClass("WmlInterpolationType",
         representation(
           href = "character",
           title = "character"
         ),
         prototype = list(),
         validity = function(object) {
           stopifnot(!is.na(object@href), is(object@href, "character"), nchar(object@href) > 0)
           stopifnot(!is.na(object@title), is(object@title, "character"), nchar(object@title) > 0)
           return(TRUE)
         }
)
#
# constructor WmlInterpolationType
#
WmlInterpolationType <- function(href, title) {
  new("WmlInterpolationType",
      href = href,
      title = title)
}
#
# class WmlDefaultTVPMeasurementMetadata ----
#
setClass("WmlDefaultTVPMeasurementMetadata",
         representation(
           uom = "character",
           interpolationType = "WmlInterpolationType"
         ),
         prototype = list(),
         validity = function(object) {
           stopifnot(!is.na(object@uom), is(object@uom, "character"), nchar(object@uom) > 0)
           stopifnot(isS4(object@interpolationType), is(object@interpolationType, "WmlInterpolationType"))
           return(TRUE)
         }
)
#
# constructor WmlDefaultTVPMeasurementMetadata
#
WmlDefaultTVPMeasurementMetadata <- function(uom, interpolationType) {
  new("WmlDefaultTVPMeasurementMetadata",
      uom = uom,
      interpolationType = interpolationType)
}
#
# class WmlTimeseries ----
#
# http://schemas.opengis.net/waterml/2.0/timeseries.xsd
#
setClass("WmlTimeseries",
         representation(
           id = "character",
           metadata = "ANY",
           defaultPointMetadata = "ANY",
           point = "ANY"
         ),
         prototype = list(),
         validity = function(object) {
           return(TRUE)
         }
)
#
# class WmlMeasurementTimeseries ----
#
setClass("WmlMeasurementTimeseries",
         representation(
           metadata = "WmlMeasurementTimeseriesMetadata",
           defaultPointMetadata = "WmlDefaultTVPMeasurementMetadata",
           points = "list"
         ),
         prototype = list(),
         contains = "WmlTimeseries",
         validity = function(object) {
           stopifnot(is(object@metadata, "WmlMeasurementTimeseriesMetadata"))
           stopifnot(is(object@defaultPointMetadata, "WmlDefaultTVPMeasurementMetadata"))
           stopifnot(is(object@points, "list"), length(object@points) > 0, all(sapply(object@points, inherits, what = "WmlMeasurementTVP")))
           return(TRUE)
         }
)
#
# constructor WmlMeasurementTimeseries
#
WmlMeasurementTimeseries <- function(id, metadata, defaultPointMetadata, points) {
  new("WmlMeasurementTimeseries",
      id = id,
      metadata = metadata,
      defaultPointMetadata = defaultPointMetadata,
      points = points)
}