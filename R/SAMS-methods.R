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
# Author: Benjamin Pross (b.pross@52north.org),                                #
#         Jürrens, Eike Hinderk (e.h.juerrens@52north.org)                     #
# Created: 2016-01-27                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

#
# parsing functions ----
#
parseSamsShape <- function(obj, sos) {
  point <- parsePoint(xml2::xml_child(x = obj, search = gmlPointName, ns = sos@namespaces),
                       sos = sos)

  SamsShape(point = point)
}

parseSams200SamplingFeature <- function(obj, sos) {
  gmlid <- xml2::xml_attr(x = obj, attr = "id")

  identifier <- xml2::xml_text(xml2::xml_child(x = obj, search = gmlIdentifierName, ns = sos@namespaces))
  name <- xml2::xml_text(xml2::xml_child(x = obj, search = gmlNameName, ns = sos@namespaces))
  type <- xml2::xml_attr(x = xml2::xml_child(x = obj, search = samTypeName, ns = sos@namespaces),
                          attr = "href")
  sampledFeature <- xml2::xml_attr(x = xml2::xml_child(x = obj, search = samSampledFeatureName, ns = sos@namespaces),
                                    attr = "href")
  shape <- parseSamsShape(xml2::xml_child(x = obj, search = samsShapeName, ns = sos@namespaces), sos = sos)
  SamsSamplingFeature(id = gmlid,
                      identifier = identifier,
                      name = name,
                      type = type,
                      sampledFeature = sampledFeature,
                      shape = shape)
}

#
# coercion methods ----
#
as.SpatialPoints.SamsShape = function(from) {
  as(from@point, "SpatialPoints")
}
setAs(from = "SamsShape", to = "SpatialPoints",
      def = function(from) {
        as.SpatialPoints.SamsShape(from)
      }
)
setAs(from = "SamsShape", to = "Spatial",
      def = function(from) {
        as.SpatialPoints.SamsShape(from)
      }
)

as.SpatialPoints.SamsSamplingFeature = function(from) {
  as(from@shape, "SpatialPoints")
}
setAs(from = "SamsSamplingFeature", to = "SpatialPoints",
      def = function(from) {
        as.SpatialPoints.SamsSamplingFeature(from)
      }
)
setAs(from = "SamsSamplingFeature", to = "Spatial",
      def = function(from) {
        as.SpatialPoints.SamsSamplingFeature(from)
      }
)

