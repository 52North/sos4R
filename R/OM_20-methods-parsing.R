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
# Created: 2010-06-18                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

#
# Function extracts om:OM_Observation elements from sos:observationData elements.
#
parseObservation_2.0 <- function(obj, sos, verbose = FALSE) {
	
  obj <- obj[[om20OM_Observation]]
  
  .id <- xmlGetAttr(node = obj, name = "id",
			default = NA_character_)
	if(verbose) cat("[parseObservation]", .id, "\n")
	
  #TODO adjust the following OM 1.0 parsing functionality
  
	# 52N SOS only returns om:Observation with procedure ids xlink:href
	.procedure <- xmlGetAttr(node = obj[[omProcedureName]], name = "href",
			default = NA_character_)
	
	.observedProperty <- parsePhenomenonProperty(obj[[omObservedPropertyName]],
			sos = sos, verbose = verbose)
	
	if(!is.null(obj[[omSamplingTimeName]])) {
		.samplingTime <- parseSamplingTime(obj = obj[[omSamplingTimeName]],
				format = sosTimeFormat(sos = sos), verbose = verbose)
	} else {
		warning("om:samplingTime is mandatory in om:Observation, but is missing!")
		.samplingTime <- NULL
	}
	
	if(!is.null(obj[[omFeatureOfInterestName]])) {
		.featureOfInterest <- parseFOI(obj[[omFeatureOfInterestName]],
				sos = sos, verbose = verbose)
	} else {
		warning("om:featureOfInterest is mandatory in om:Observation, but is missing!")
		.featureOfInterest <- NULL
	}
	
	# result parser is exchangeable
	.resultParsingFunction <- sosParsers(sos)[[omResultName]]
	.result <- .resultParsingFunction(obj[[omResultName]], sos, verbose)
	
	# optional elements
	if(!is.null(obj[[omResultTimeName]])) {
		.resultTime <- parseSamplingTime(obj = obj[[omResultTimeName]],
				format = sosTimeFormat(sos = sos), verbose = verbose)
	}
	else {
		.resultTime <- NULL
	}
	
	# TODO optionals elements for OmObservation
	#.metadata
	#.resultQuality
	#.parameter
	#.metadata
	
	.obs <- OmObservation(samplingTime = .samplingTime,
			procedure = .procedure, observedProperty = .observedProperty,
			featureOfInterest = .featureOfInterest, result = .result)
	
	return(.obs)
}
