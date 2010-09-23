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
# Author: Daniel Nüst (daniel.nuest@uni-muenster.de)                           #
# Created: 2010-09-08                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

#
# construction methods
#
OmObservation <- function(samplingTime, procedure, observedProperty,
		featureOfInterest, result, metadata = NA, resultTime = NULL,
		resultQuality = NA,	parameter = NA) {
	new("OmObservation", samplingTime = samplingTime, procedure = procedure,
			observedProperty = observedProperty,
			featureOfInterest = featureOfInterest, result = result,
			metadata = metadata, resultTime = resultTime,
			resultQuality = resultQuality,
			parameter = parameter)
}

OmObservationProperty <- function(href = as.character(NA), obs = NULL) {
	new("OmObservationProperty", href = href, obs = obs)
}

OmMeasure <- function(value, uom) {
	new("OmMeasure", value = value, uom = uom)
}

OmMeasurement <- function(samplingTime, procedure, observedProperty,
		featureOfInterest, result, metadata = NA, resultTime = NA,
		resultQuality = NA,	parameter = NA) {
	new("OmMeasurement", samplingTime = samplingTime, procedure = procedure,
			observedProperty = observedProperty,
			featureOfInterest = featureOfInterest, result = result,
			metadata = metadata, resultTime = resultTime,
			resultQuality = resultQuality,
			parameter = parameter)
}