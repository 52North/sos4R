################################################################################
# Copyright (C) 2010 by 52 North                                               #
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
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

#
#
#
as.SensorML.SpatialPointsDataFrame = function(from) {
	.coords <- from@coords
	.crds <- .coords[,c("x", "y")]
	.crs <- sosGetCRS(attributes(.coords)[["referenceFrame"]])
	
	.notCoordCols <- !colnames(.coords)%in%c("x", "y")
	.otherData <- data.frame(.coords[,.notCoordCols])
	colnames(.otherData) <- colnames(.coords)[.notCoordCols]

	.sp <- SpatialPointsDataFrame(coords = .crds,
			data = .otherData,
			proj4string = .crs)
	
	return(.sp)
}
setAs("SensorML", "SpatialPointsDataFrame", 
		function(from) {
			as.SensorML.SpatialPointsDataFrame(from)
		}
)
setAs("SensorML", "Spatial", 
		function(from) {
			as.SensorML.SpatialPointsDataFrame(from)
		}
)

