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
# Created: 2013-08-28                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

#
# try re-using 1.0.0 function
#
.sosRequest_2.0 <- function(sos, request, verbose = FALSE, inspect = FALSE) {
	if (verbose) {
		cat("[.sosRequest_2.0] of", sosUrl(sos), "\n")
	}
	
	.result <- .sosRequest_1.0.0(sos, request, verbose = verbose,
			inspect = inspect)
	return(.result)
	
	if (verbose) {
		cat("[.sosRequest_2.0] of", sosUrl(sos), "\n")
	}
}

.getCapabilities_2.0.0 <- function(sos, verbose, inspect, sections,
		acceptFormats, updateSequence, owsVersion,	acceptLanguages) {
	if (verbose) {
		cat("[.getCapabilities_2.0.0] of", sosUrl(sos), "\n")
	}
	
	.gc <- OwsGetCapabilities(service = sosService,
			acceptVersions = c(sosVersion(sos)), sections = sections,
			acceptFormats = acceptFormats, updateSequence = updateSequence,
			owsVersion = owsVersion, acceptLanguages = acceptLanguages)
	if(verbose) cat("[.getCapabilities_2.0.0] REQUEST:\n", toString(.gc), "\n")
	
	.responseString = sosRequest(sos = sos, request = .gc,
			verbose = verbose, inspect = inspect)
	if(verbose){
		cat("[.getCapabilities_2.0.0] RESPONSE:\n", .responseString , "\n")
	}
	
	.response <- xmlParseDoc(file = .responseString, asText = TRUE)
	if(verbose || inspect) {
		cat("[.getCapabilities_2.0.0] RESPONSE DOC:\n")
		print(.response)
	}
	
	if(.isExceptionReport(.response)) {
		return(.handleExceptionReport(sos, .response))
	}
	else {
		.parsingFunction <- sosParsers(sos)[[sosGetCapabilitiesName]]
		.caps <- .parsingFunction(obj = .response, sos = sos)
		if (verbose) {
			cat("[.getCapabilities_2.0.0] DONE WITH PARSING!\n")
		} 
		return(.caps)
	}
}
