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

.sosService <- "SOS"

.sosConnectionMethodGet <- "GET"
.sosConnectionMethodPost <- "POST"
.sosConnectionMethodSOAP <- "SOAP"

.sosGetCapabilitiesName <- "GetCapabilities"
.sosDescribeSensorName <- "DescribeSensor"
.sosGetObservationName <- "GetObservation"
.sosGetObservationByIdName <- "GetObservationById"
.sosOwsExceptionReportRootName <- "ExceptionReport"

.sosSupportedResponseFormats <- c(
		"text/xml;subtype=&quot;om/1.0.0&quot;",
		"text/xml;subtype=&quot;sensorML/1.0.1&quot;")
SosSupportedResponseFormats <- function() {
	return(.sosSupportedResponseFormats)
}

.sosSupportedResultModels <- c("om:Measurement", "om:Observation")
SosSupportedResultModels <- function() {
	return(.sosSupportedResultModels)
}

.sosSupportedResponseModes <- c("inline")
SosSupportedResponseModes <- function() {
	return(.sosSupportedResponseModes)
}

.sosNamespacePrefix <- "sos"
.sosNamespaceDefinitionsAll <- c(sos = "http://www.opengis.net/sos/1.0",
		xsi = "http://www.w3.org/2001/XMLSchema-instance")
.sosNamespaceDefinitionsGetObs <- c(ows = "http://www.opengis.net/ows/1.1",
		om = "http://www.opengis.net/om/1.0",
		ogc = "http://www.opengis.net/ogc")
.sosNamespaceDefinitionsGetCap <- c(ows = "http://www.opengis.net/ows/1.1",
		ogc = "http://www.opengis.net/ogc")

.xsiSchemaLocationAttribute <- c("xsi:schemaLocation" = "http://www.opengis.net/sos/1.0 http://schemas.opengis.net/sos/1.0.0/sosAll.xsd")

.owsNamespacePrefix <- "ows"
.owsNamespace <- c(ows = "http://www.opengis.net/ows/1.1")
.owsCodes = c(
		"OperationNotSupported",
		"MissingParameterValue",
		"InvalidParameterValue",
		"VersionNegotiationFailed",
		"InvalidUpdateSequence",
		"OptionNotSupported",
		"NoApplicableCode")
.owsCodeMeanings = c(
		"Request is for an operation that is not supported by this server",
		"Operation request does not include a parameter value, and this server did not declare a default parameter value for that parameter",
		"Operation request contains an invalid parameter value",
		"List of versions in 'AcceptVersions' parameter value in GetCapabilities operation request did not include any version supported by this server",
		"Value of (optional) updateSequence parameter in GetCapabilities operation request is greater than current value of service metadata updateSequence number",
		"Request is for an option that is not supported by this server",
		"No other exceptionCode specified by this service and server applies to this exception")
.owsCodeLocators = c(
		"Name of operation not supported",
		"Name of missing parameter",
		"Name of parameter with invalid value",
		"None, omit 'locator' parameter",
		"None, omit 'locator' parameter",
		"Identifier of option not supported",
		"None, omit 'locator' parameter")
.httpCode = c("501", "400", "400", "400", "400", "501", "3xx, 4xx, 5xx")
.httpMessage = c("Not Implemented", "Bad request", "Bad request", "Bad request",
		"Bad request", "Not implemented", "Internal Server Error")

.owsStandardExceptions <- data.frame(
		exceptionCode = .owsCodes,
		meaningOfCode = .owsCodeMeanings, 
		locator = .owsCodeLocators,
		httpStatusCode = .httpCode,
		httpMessage = .httpMessage,
		check.rows = TRUE, check.names = TRUE)