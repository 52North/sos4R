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
# Created: 2011-02-11                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

#
#
#
as.SpatialPointsDataFrame.SensorML = function(from) {
  .coords <- sosCoordinates(from, handleNames = TRUE)
  .coordsNames <- names(.coords)

  if (all(dim(.coords) > 0, any(.coordsNames == "y"),
         any(.coordsNames == "x"))) {
    .crds <- .coords[,c("x", "y")]
    .crs <- sosGetCRS(from)

    if (is.null(.crs)) {
      warning("[as.SpatialPointsDataFrame.SensorML] No CRS from sensor ",
              "description (using sosGetCRS(...), using default.")
      .crs <- sosGetCRS(sosDefaultReferenceFrameSensorDescription)
    }

    .notCoordCols <- !colnames(.coords) %in% c("x", "y")
    .otherData <- data.frame(.coords[,.notCoordCols])
    colnames(.otherData) <- colnames(.coords)[.notCoordCols]

    .sp <- sp::SpatialPointsDataFrame(coords = .crds,
                                  data = .otherData,
                                  proj4string = .crs)

    return(.sp)
  }
  else {
    warning(paste0("Cannot coerce SensorML to SpatialPointsDataFrame ",
                   " because no coordinates found for object. Check that ",
                   " sosCoordinates(obj) returns a matrix with colum ",
                   "names 'x' and 'y'."))
    return(sp::SpatialPointsDataFrame(data.frame(0,0),
                                  data = data.frame(NA_character_)))
  }
}
setAs(from = "SensorML", to = "SpatialPointsDataFrame",
      function(from) {
        as.SpatialPointsDataFrame.SensorML(from)
      }
)
setAs(from = "SensorML", to = "SpatialPoints",
      function(from) {
        as.SpatialPointsDataFrame.SensorML(from)
      }
)
setAs(from = "SensorML", to = "Spatial",
      function(from) {
        as.SpatialPointsDataFrame.SensorML(from)
      }
)

