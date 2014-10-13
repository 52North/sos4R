################################################################################
# Copyright (C) 2013 by 52°North                                               #
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
# Created: 2013-08-28                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

#
#
#
setClass("SOS",
		representation(version = "character",
				capabilities = "OwsCapabilities", parsers = "list",
				encoders = "list", dataFieldConverters = "list",
				timeFormat = "character", verboseOutput = "logical",
				switchCoordinates = "logical", useDCPs = "logical",
				dcpFilter = "list", additionalKVPs = "list"),
		contains = c("VIRTUAL"))

#
# SOS class for local testing, i.e. without an URL and default verbose output
#
setClass("SOS_Test",
				 representation(name = "character", binding = "character"),
				 prototype = list(name = as.character(NA)),
				 contains = c("SOS"),
				 validity = function(object) {
				 	#print("Entering validation: SOS_Test")
				 	return(TRUE)
				 }
)

SOS_Test <- function(name = "test",
								binding = SosDefaultBinding(),
								version = "testing",
								parsers = SosParsingFunctions(),
								encoders = SosEncodingFunctions(),
								dataFieldConverters = SosDataFieldConvertingFunctions(),
								timeFormat = sosDefaultTimeFormat,
								verboseOutput = TRUE, 
								switchCoordinates = FALSE,
								useDCPs = TRUE,
								dcpFilter = SosDefaultDCPs(),
								additionalKVPs = list(),
								...) {
	
		.sos <- new("SOS_Test",
								name = name,
								binding = binding,
								version = version,
								capabilities = new("OwsCapabilities", version = "NA",
																	 updateSequence = as.character(NA),
																	 owsVersion = sosDefaultGetCapOwsVersion),
								parsers = parsers,
								encoders = encoders,
								dataFieldConverters = dataFieldConverters,
								timeFormat = timeFormat,
								verboseOutput = verboseOutput,
								switchCoordinates = switchCoordinates,
								useDCPs = useDCPs,
								dcpFilter = dcpFilter,
								additionalKVPs = additionalKVPs)
		
		if(verboseOutput) cat("[SOS] Created new SOS_Test:\n", toString(.sos), "\n")
		return(.sos)
}
