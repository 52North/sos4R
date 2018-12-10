################################################################################
# Copyright (C) 2016 by 52 North                                               #
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
# Project: sos4R - visit the project web page,                                 #
#      http://52north.org/communities/sensorweb/clients/sos4R/                 #
################################################################################

#
# Classes are based on Observations & Measurements - Part X - Spatial Sampling Features
# 
# http://www.opengeospatial.org/standards/om
#

#
# SamsShape ----
#
setClass("SamsShape",
         slots = c(point = "GmlPoint"),
         # TODO is this class inheritence required? Fields of GmlGeometry are contained in GmlPoint.
         contains = "GmlGeometry"
)

SamsShape <- function(point) {
  new("SamsShape", point = point)
}

.toString.SamsShape <- function(x, ...) {
  paste("Object of class SamsShape; ",
                "\n\tpoint: ", toString(x@point))
}
setMethod("toString", "SamsShape", function(x, ...) .toString.SamsShape(x, ...))

.print.SamsShape <- function(x, ...) {
  cat(.toString.SamsShape(x, ...), "\n")
  invisible(x)
}
setMethod("print", "SamsShape", function(x, ...) .print.SamsShape(x, ...))
setMethod("show", "SamsShape", function(object) .print.SamsShape(object))


#
# SamsSamplingFeature ----
#
# <sams:SF_SpatialSamplingFeature gml:id="ssf_88204F34D0B94590AA1EDE21577C9B5D907F4BAD">
#   <gml:identifier codeSpace="http://www.opengis.net/def/nil/OGC/0/unknown">foi-1</gml:identifier>
#   <gml:name codeSpace="http://www.opengis.net/def/nil/OGC/0/unknown">foi one</gml:name>
#   <sf:type xlink:href="http://www.opengis.net/def/samplingFeatureType/OGC-OM/2.0/SF_SamplingPoint"/>
#   <sf:sampledFeature xlink:href="http://www.52north.org/test/featureOfInterest/world"/>
#   <sams:shape>
#     <ns:Point xmlns:ns="http://www.opengis.net/gml/3.2" ns:id="gml-id-p1">
#       <ns:pos srsName="http://www.opengis.net/def/crs/EPSG/0/4326">51.883906 7.727958</ns:pos>
#     </ns:Point>
#   </sams:shape>
# </sams:SF_SpatialSamplingFeature>
setClass("SamsSamplingFeature",
         slots = c(
           identifier = "list",
           name = "list",
           type = "list",
           sampledFeature = "list",
           shape = "SamsShape"
         ),
         contains = "GmlFeature"
)

SamsSamplingFeature <- function(id, identifier, name, type, sampledFeature, shape) {
  new("SamsSamplingFeature",
      id = id,
      identifier = identifier,
      name = name,
      type = type,
      sampledFeature = sampledFeature,
      shape = shape)
}

.toString.SamsSamplingFeature <- function(x, ...) {
  paste0("Object of class SamsSamplingFeature;",
        "\n             id: ", x@id,
        "\n     identifier: ", x@identifier[[1]],
        "\n           name: ", x@name[[1]],
        "\n           type: ", x@type[[1]],
        "\n sampledFeature: ", x@sampledFeature[[1]],
        "\n          shape: ", x@shape)
}
setMethod("toString", "SamsSamplingFeature", function(x, ...) .toString.SamsSamplingFeature(x, ...))

.print.SamsSamplingFeature <- function(x, ...) {
  cat(.toString.SamsSamplingFeature(x, ...), "\n")
  invisible(x)
}
setMethod("print", "SamsSamplingFeature", function(x, ...) .print.SamsSamplingFeature(x, ...))
setMethod("show", "SamsSamplingFeature", function(object) .print.SamsSamplingFeature(object))
