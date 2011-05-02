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
setMethod(f = "sosId", signature = signature(obj = "SensorML"),
		def = function(obj) {
			return(obj@id)
		})

#
#
#
setMethod(f = "sosName", signature = signature(obj = "SensorML"),
		def = function(obj) {
			return(obj@name)
		})

#
#
#
setMethod(f = "sosAbstract", signature = signature(obj = "SensorML"),
		def = function(obj) {
			return(obj@description)
		})

#
#
#
setMethod(f = "sosGetCRS",
		signature = c(obj = "SensorML"),
		def = function(obj, verbose = FALSE) {
			.coords <- sosCoordinates(obj)
			.crs <- sosGetCRS(attributes(.coords)[["referenceFrame"]],
					verbose = verbose)
			return(.crs)
		}
)

#
#
#
setMethod(f = "sosBoundedBy",
		signature = signature(obj = "SensorML"),
		def = function(obj, sos, verbose = FALSE) {
			return(obj@boundedBy)
		})


#
# extract the coordinates from the SensorML document and return as a data.frame
#
setMethod(f = "sosCoordinates", signature = signature(obj = "SensorML"),
		def = function(obj) {
			return(obj@coords)
		}
)
