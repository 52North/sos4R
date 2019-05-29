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
setClassUnion(name = "SamsShapeOrNULL",
              members = c("SamsShape", "NULL"))

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
# TODO change type of identifier, name to something that preservers codespace and value
setClass("SamsSamplingFeature",
         slots = c(
           identifier = "character",
           name = "character",
           type = "character",
           sampledFeature = "character",
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
        "\n             id: ", toString(x@id),
        "\n     identifier: ", toString(x@identifier),
        "\n           name: ", toString(x@name),
        "\n           type: ", toString(x@type),
        "\n sampledFeature: ", toString(x@sampledFeature),
        "\n          shape: ", toString(x@shape))
}
setMethod("toString", "SamsSamplingFeature", function(x, ...) .toString.SamsSamplingFeature(x, ...))

.print.SamsSamplingFeature <- function(x, ...) {
  cat(.toString.SamsSamplingFeature(x, ...), "\n")
  invisible(x)
}
setMethod("print", "SamsSamplingFeature", function(x, ...) .print.SamsSamplingFeature(x, ...))
setMethod("show", "SamsSamplingFeature", function(object) .print.SamsSamplingFeature(object))
