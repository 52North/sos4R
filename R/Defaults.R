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
# Created: 2010-06-18                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

#
# This class is inspired by a suggestion from Duncan Murdoch
# (https://stat.ethz.ch/pipermail/r-help/2010-July/245480.html)
# and code of the package SSOAP by Duncan Temple Lang
# (http://www.omegahat.org/SSOAP/)
#

# List of the default parsing functions. The names of the list are the
# names of the respective XML documents set in Constants.R.
.sosDefaultParsers <- list(
		parseSosCapabilities, # functions for different versions not changeable
		parseSensorML,
		parseOM,
		parseOM,
		parseOwsExceptionReport)
names(.sosDefaultParsers) <- list(
		.sosGetCapabilitiesName,
		.sosDescribeSensorName,
		.sosGetObservationName,
		.sosGetObservationByIdName,
		.sosOwsExceptionReportRootName)

# Using a different approach for the encoders here, because there is more than
# one way of encoding something (in contrast to parsing). So the different 
# objects (and versions) override the respective encoding functions. 
.sosDefaultEncoders <- list(
		encodeRequestKVP,
		encodeRequestXML,
		encodeRequestSOAP)
names(.sosDefaultEncoders) <- list(
		.sosConnectionMethodGet,
		.sosConnectionMethodPost,
		.sosConnectionMethodSOAP
		)

################################################################################
# access methods

SOSParsers <- function (..., include = character(0), exclude = character(0)) {
	defaults <- .sosDefaultParsers
	els <- list(...)
	.merge(els, defaults, include, exclude)
}

SOSEncoders <- function (..., include = character(0), exclude = character(0)) {
	defaults <- .sosDefaultEncoders
	els <- list(...)
	.merge(els, defaults, include, exclude)
}

SOSDefaultConnectionMethod <- function() {
	return(.sosConnectionMethodGet)
}

#
# Function originally written by Duncan Temple Lang for the package SSOAP
# (http://www.omegahat.org/SSOAP/), slightly adapted here to include all
# defaults by default, then overwrite if an element with the same name is given.
#
# Order: first replacing defaults, THEN inclusion, THEN exclusion
#
.merge <- function (els, defaults, include = NULL, exclude = NULL) {
	if (length(els) > 0) {
		which <- match(names(defaults), names(els))
		if (any(!is.na(which))) {
			# replace the defaults for all given elements
			defaults[!is.na(which)] <- els[names(defaults)[!is.na(which)]]
			els <- defaults
			# original: els[names(defaults)[!is.na(which)]] <- defaults[!is.na(which)]
		}
	}
	else els <- defaults
	
	if (length(include)) {
		els <- els[include]
	}
	else if (length(exclude)) {
		which <- match(exclude, names(els))
		if (any(!is.na(which))) 
			els <- els[-(which[!is.na(which)])]
	}
	
	return(els)
}

#
# Dummy parsing function if a user wants to inspect the responses unprocessed.
# This works for all but capabilities, as these need to be requested on creating
# a new SOS instance.
#
.parseSosNoParsing <- function(ob) {
	return(ob)	
}
.sosDisabledParsers <- list(
		parseSosCapabilities, # changing this
		.parseSosNoParsing,
		.parseSosNoParsing,
		.parseSosNoParsing,
		.parseSosNoParsing)
names(.sosDisabledParsers) <- list(
		.sosGetCapabilitiesName,
		.sosDescribeSensorName,
		.sosGetObservationName,
		.sosGetObservationByIdName,
		.sosOwsExceptionReportRootName)
SOSDisabledParsers <- function() {
	return(.sosDisabledParsers)
}

################################################################################
# other defaults

.sosDefaultCharacterEncoding <- "UTF-8"
.sosDescribeSensorDefaultOutputFormat <- "text/xml;subtype=&quot;sensorML/1.0.1&quot;"
.sosDefaultGetCapSections <- c("All")
.sosDefaultGetCapAcceptFormats <- c("text/xml")
.sosDefaultGetCapOwsVersion <- "1.1.0"

.sosDefaultSaveXmlPrefix <- '<?xml version="1.0"?>\n'
