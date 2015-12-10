################################################################################
# Copyright (C) 2015 by 52 North                                               #
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
# Created: 2013-03-06                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

#
#
#
setMethod(f = "sosRequest",
		signature = signature(sos = "SOS_2.0.0", request = "OwsServiceOperation",
				verbose = "logical", inspect = "logical"),
		def = function(sos, request, verbose, inspect) {
			.sosRequest_2.0(sos = sos, request = request, verbose = verbose,
					inspect = inspect)
		}
)

#
#
#
setMethod(f = "getCapabilities", signature = signature(sos = "SOS_2.0.0"),
		def = function(sos, verbose, inspect, sections, acceptFormats,
				updateSequence, owsVersion,	acceptLanguages) {
			return(.getCapabilities_2.0.0(sos = sos, verbose = verbose,
							inspect = inspect, sections = sections,
							acceptFormats = acceptFormats,
							updateSequence = updateSequence,
							owsVersion = owsVersion,
							acceptLanguages = acceptLanguages))
			
			#stop("getCapabilities for SOS_2.0.0 not implemented yet!")
		}
)

#
#
#
setMethod(f = "describeSensor",
		signature = signature(sos = "SOS_2.0.0", procedure  = "character"), 
		def = function(sos, procedure, outputFormat, verbose, inspect,
				saveOriginal) {
			stop("describeSensor for SOS_2.0.0 not implemented yet!")
		}
)


#
# 
#
setMethod(f = "getObservationById",
		signature = signature(sos = "SOS_2.0.0", observationId = "character"), 
		def = function(sos, observationId, responseFormat, srsName,
				resultModel, responseMode, verbose, inspect, saveOriginal) {
			stop("getObservationById for SOS_2.0.0 not implemented yet!")
		}
)

#
#
#
setMethod(f = "getObservation",
		signature = signature(sos = "SOS_2.0.0",
				offering = "SosObservationOffering_2.0.0"),
		def = function(sos, offering, observedProperty, responseFormat, srsName,
				eventTime,	procedure, featureOfInterest, result, resultModel,
				responseMode, BBOX, latest, verbose, inspect, saveOriginal) {
			stop("getObservation for SOS_2.0.0 with offering object not implemented yet!")
		}
)

#
#
#
setMethod(f = "getObservation",
		signature = signature(sos = "SOS_2.0.0",
				offering = "character"),
		def = function(sos, offering, observedProperty = list(), responseFormat,
				srsName, eventTime,	procedure, featureOfInterest, result,
				resultModel, responseMode, BBOX, latest, verbose, inspect,
				saveOriginal) {
			stop("getObservation for SOS_2.0.0 with charecter offering not implemented yet!")
		}
)

SosObservationOffering_2.0.0 <- function(id, name = as.character(NA),
                                   time, procedure, observedProperty,
                                   featureOfInterest, responseFormat,
                                   intendedApplication = as.character(NA), resultModel = as.character(NA),
                                   responseMode = as.character(NA), boundedBy = list()) {
  new("SosObservationOffering_2.0.0", id = id, name = name,
      time = time, procedure = procedure,
      observedProperty = observedProperty,
      featureOfInterest = featureOfInterest,
      responseFormat = responseFormat,
      intendedApplication = intendedApplication,
      resultModel = resultModel, responseMode = responseMode,
      boundedBy = boundedBy)
}
