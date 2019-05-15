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
# Author: Benjamin Pross (b.pross@52north.org)                                 #
# Created: 2016-01-27                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
################################################################################

#
# contruction function ----
#
MonitoringPoint <- function(sampledFeatures,
                            id,
                            identifier,
                            names,
                            shape,
                            verticalDatums = xml2::xml_missing(),
                            timeZone = xml2::xml_missing()) {
  new("MonitoringPoint",
      sampledFeatures = sampledFeatures,
      id = id,
      identifier = identifier,
      names = names,
      shape = shape,
      verticalDatums = verticalDatums,
      timeZone = timeZone)
}

#
# parsing functions ----
#
parseMonitoringPoint <- function(obj, sos, verbose = FALSE) {
  sampledFeaturesXml <- xml2::xml_find_all(x = obj, xpath = samSampledFeatureName, ns = sos@namespaces)
  sampledFeatures <- sapply(X = sampledFeaturesXml, FUN = function(feature) {
    link <- xml2::xml_attr(x = feature, attr = "xlink:href", ns = sos@namespaces)
    title <- xml2::xml_attr(x = feature, attr = "xlink:title", ns = sos@namespaces)
    names(link) <- title
    return(link)
  })
  id <- xml2::xml_attr(x = obj, attr = "gml:id", default = NA_character_, ns = sos@namespaces)
  names <- xml2::xml_text(xml2::xml_find_all(x = obj, xpath = gmlNameName, ns = sos@namespaces))
  identifier <- xml2::xml_text(xml2::xml_find_all(x = obj, xpath = gmlIdentifierName, ns = sos@namespaces))
  shape <- parseSamsShape(obj = xml2::xml_find_all(x = obj, xpath = samsShapeName, ns = sos@namespaces), sos = sos)
  verticalDatums <- xml2::xml_find_all(x = obj, xpath = wmlVerticalDatumName, ns = sos@namespaces)
  timeZone <-   xml2::xml_find_first(x = obj, xpath = wmlTimeZoneName, ns = sos@namespaces)

  mp <- MonitoringPoint(sampledFeatures = sampledFeatures,
                        id = id,
                        identifier = identifier,
                        names = names,
                        shape = shape,
                        verticalDatums = verticalDatums,
                        timeZone = timeZone)

  return(mp)
}

#
# coercion methods ----
#
as.SpatialPoints.MonitoringPoint = function(from) {
  as(from@shape, "SpatialPoints")
}
setAs("MonitoringPoint", "SpatialPoints",
      function(from) {
        as.SpatialPoints.MonitoringPoint(from)
      }
)
setAs("MonitoringPoint", "Spatial",
      function(from) {
        as.SpatialPoints.MonitoringPoint(from)
      }
)
