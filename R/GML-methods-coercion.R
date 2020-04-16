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
# Created: 2019-04-29                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

as.SpatialPolygons.GmlEnvelope = function(from) {
  # create clockwise, first row equals last row
  crs <- sosGetCRS(obj = from)

  lowerCorner <- from@lowerCorner
  if (is.na(lowerCorner@srsName))
    lowerCorner@srsName <- from@srsName
  upperCorner <- from@upperCorner
  if (is.na(upperCorner@srsName))
    upperCorner@srsName <- from@srsName

  lower <- as(lowerCorner, "SpatialPoints")
  upper <- as(upperCorner, "SpatialPoints")

  polygon <- sp::Polygon(coords = cbind(c(lower$lon,
                                          lower$lon,
                                          upper$lon,
                                          upper$lon,
                                          lower$lon),
                                        c(lower$lat,
                                          upper$lat,
                                          upper$lat,
                                          lower$lat,
                                          lower$lat)),
                         hole = FALSE)

  sp::SpatialPolygons(Srl = list(sp::Polygons(srl = list(polygon),
                                              ID = "env1")),
                      proj4string = crs)
}
setAs(from = "GmlEnvelope", to = "SpatialPolygons",
      def = function(from) {
        as.SpatialPolygons.GmlEnvelope(from)
      }
)
setAs(from = "GmlEnvelope", to = "Spatial",
      def = function(from) {
        as.SpatialPolygons.GmlEnvelope(from)
      }
)

as.SpatialPoints.GmlPoint = function(from) {
  as(from@pos, "SpatialPoints")
}
setAs(from = "GmlPoint", to = "SpatialPoints",
      def = function(from) {
        as.SpatialPoints.GmlPoint(from)
      }
)
setAs(from = "GmlPoint", to = "Spatial",
      def = function(from) {
        as.SpatialPoints.GmlPoint(from)
      }
)

as.SpatialPoints.GmlPointProperty = function(from) {
  if (!is.na(from@href))
    stop("GML PointProperty with href, cannot coerce reference property.")

  as(from@point, "SpatialPoints")
}
setAs(from = "GmlPointProperty", to = "SpatialPoints",
      def = function(from) {
        as.SpatialPoints.GmlPointProperty(from)
      }
)
setAs(from = "GmlPointProperty", to = "Spatial",
      def = function(from) {
        as.SpatialPoints.GmlPointProperty(from)
      }
)

as.SpatialPoints.GmlDirectPosition = function(from) {
  crs <- sosGetCRS(obj = from)
  if (is.logical(crs))
    stop("CRS is missing in GmlDirectPosition, cannot coerce to Spatial class")
  coords <- sosCoordinates(obj = from)
  sp::SpatialPoints(coords = cbind(lon = coords$lon, lat = coords$lat), proj4string = crs)
}
setAs(from = "GmlDirectPosition", to = "SpatialPoints",
      def = function(from) {
        as.SpatialPoints.GmlDirectPosition(from)
      }
)
setAs(from = "GmlDirectPosition", to = "Spatial",
      def = function(from) {
        as.SpatialPoints.GmlDirectPosition(from)
      }
)

as.SpatialPoints.GmlFeatureProperty = function(from) {
  if (!is.na(from@href))
    stop("GML FeatureProperty with href, cannot coerce reference property.")

  as(from@feature, "SpatialPoints")
}
setAs(from = "GmlFeatureProperty", to = "SpatialPoints",
      def = function(from) {
        as.SpatialPoints.GmlFeatureProperty(from)
      }
)
setAs(from = "GmlFeatureProperty", to = "Spatial",
      def = function(from) {
        as.SpatialPoints.GmlFeatureProperty(from)
      }
)
