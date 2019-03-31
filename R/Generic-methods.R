################################################################################
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
# visit the Free Software Foundation web page, http://www.fsf.org              #
#                                                                              #
# Authors: Daniel Nuest (daniel.nuest@uni-muenster.de)                         #
#          Eike Hinderk Jürrens (e.h.juerrens@52north.org)                     #
# Created: 2010-09-20                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
################################################################################

#
# sosRequest ----
#
if (!isGeneric("sosRequest"))
  setGeneric(name = "sosRequest",
             def = function(sos, request, verbose = sos@verboseOutput,
                            inspect = FALSE) {
               standardGeneric("sosRequest")
             })

#
# getCapabilities ----
#
if (!isGeneric("getCapabilities"))
  setGeneric(name = "getCapabilities",
             signature = signature("sos", "verbose", "inspect"),
             def = function(sos, verbose = sos@verboseOutput, inspect = FALSE,
                            sections = sosDefaultGetCapSections,
                            acceptFormats = sosDefaultGetCapAcceptFormats,
                            updateSequence = c(as.character(NA)),
                            owsVersion = sosDefaultGetCapOwsVersion,
                            acceptLanguages = c(NA)) {
               standardGeneric("getCapabilities")
             })

#
# describeSensor ----
#
if (!isGeneric("describeSensor"))
  setGeneric(name = "describeSensor",
             signature = signature("sos", "procedure", "outputFormat", "verbose",
                                   "inspect", "saveOriginal"),
             def = function(sos, procedure,
                            outputFormat = sosDefaultDescribeSensorOutputFormat,
                            verbose = sos@verboseOutput, inspect = FALSE,
                            saveOriginal = NULL) {
               standardGeneric("describeSensor")
             })

#
# getObservationById ----
#
if (!isGeneric("getObservationById"))
  setGeneric(name = "getObservationById",
             signature = signature("sos", "observationId", "responseFormat",
                                   "srsName", "resultModel", "responseMode", "verbose",
                                   "inspect", "saveOriginal"),
             def = function(sos, observationId,
                            responseFormat = sosDefaultGetObsResponseFormat,
                            srsName = as.character(NA), resultModel = as.character(NA),
                            responseMode = as.character(NA),
                            verbose = sos@verboseOutput, inspect = FALSE,
                            saveOriginal = NULL) {
               standardGeneric("getObservationById")
             })

#
# getObservation ----
#
if (!isGeneric("getObservation"))
  setGeneric(name = "getObservation",
             signature = signature("sos", "offering", "observedProperty",
                                   "responseFormat", "srsName", "eventTime", "procedure",
                                   "featureOfInterest", "result", "resultModel",
                                   "responseMode", "BBOX", "verbose", "inspect",
                                   "saveOriginal"),
             def = function(sos, offering,
                            observedProperty = sosObservedProperties(obj = offering),
                            responseFormat = sosDefaultGetObsResponseFormat,
                            # optional:
                            srsName = as.character(NA),
                            eventTime = list(), # sosCreateEventTimeList(time = sosTime(obj = offering))
                            procedure = as.character(NA), # sosProcedures(obj = offering),
                            featureOfInterest = NULL,
                            result = NULL,
                            resultModel = as.character(NA),
                            responseMode = as.character(NA),
                            BBOX = as.character(NA),
                            verbose = sos@verboseOutput,
                            inspect = FALSE,
                            saveOriginal = NULL) {
               standardGeneric("getObservation")
             })

#
# getFeatureOfInterest ----
#
if (!isGeneric("getFeatureOfInterest"))
  setGeneric(name = "getFeatureOfInterest",
             signature = signature("sos", "featureOfInterest", "verbose", "inspect", "saveOriginal"),
             def = function(sos, featureOfInterest,
                            verbose = sos@verboseOutput, inspect = FALSE, saveOriginal=FALSE) {
               standardGeneric("getFeatureOfInterest")
             })

#
# getDataAvailability ----
#
if (!isGeneric("getDataAvailability")) {
  setGeneric(name = "getDataAvailability",
             signature = signature("sos",
                                   "procedures",
                                   "observedProperties",
                                   "featuresOfInterest",
                                   "offerings",
                                   "verbose",
                                   "inspect",
                                   "saveOriginal"),
             def = function(sos,
                            procedures = list(NA),
                            observedProperties = list(NA),
                            featuresOfInterest = list(NA),
                            offerings = list(NA),
                            verbose = sos@verboseOutput,
                            inspect = FALSE,
                            saveOriginal = NULL) {
               standardGeneric("getDataAvailability")
             })
}

#
# checkRequest ----
#
if (!isGeneric("checkRequest"))
  setGeneric(name = "checkRequest",
             def = function(service, operation, verbose) {
               standardGeneric("checkRequest")
             })

#
# encodeRequestKVP ----
#
if (!isGeneric("encodeRequestKVP"))
  setGeneric(name = "encodeRequestKVP",
             def = function(obj, sos, verbose = FALSE) {
               standardGeneric("encodeRequestKVP")
             })

#
# encodeRequestXML ----
#
if (!isGeneric("encodeRequestXML"))
  setGeneric(name = "encodeRequestXML",
             def = function(obj, sos, verbose = FALSE) {
               standardGeneric("encodeRequestXML")
             })

#
# encodeRequestSOAP ----
#
if (!isGeneric("encodeRequestSOAP"))
  setGeneric(name = "encodeRequestSOAP",
             def = function(obj, sos, verbose = FALSE) {
               standardGeneric("encodeRequestSOAP")
             })

#
# sosExceptionCodeMeaning ----
#
if (!isGeneric("sosExceptionCodeMeaning"))
  setGeneric(name = "sosExceptionCodeMeaning", def = function(exceptionCode) {
    standardGeneric("sosExceptionCodeMeaning")
  })

#
# encodeXML ----
#
if (!isGeneric("encodeXML"))
  setGeneric(name = "encodeXML",
             def = function(obj, sos, verbose = FALSE, ...) {
               standardGeneric("encodeXML")
             })

#
# encodeKVP ----
#
if (!isGeneric("encodeKVP"))
  setGeneric(name = "encodeKVP",
             def = function(obj, sos, verbose = FALSE, ...) {
               standardGeneric("encodeKVP")
             })

#
# sosGetCRS ----
#
if (!isGeneric("sosGetCRS"))
  setGeneric(name = "sosGetCRS",
             def = function(obj, verbose = FALSE) {
               standardGeneric("sosGetCRS")
             })

#
# parseFile ----
#
if (!isGeneric("parseFile"))
  setGeneric(name = "parseFile",
             def = function(sos, file, verbose = FALSE, ...) {
               standardGeneric("parseFile")
             })

#
# sosGetDCP ----
#
if (!isGeneric("sosGetDCP"))
  setGeneric(name = "sosGetDCP",
             def = function(sos, operation, type = NA) {
               standardGeneric("sosGetDCP")
             })

#
# sosCreateEventTime ----
#
if (!isGeneric("sosCreateEventTime"))
  setGeneric(name = "sosCreateEventTime",
             def = function(time, operator = sosDefaultTemporalOperator) {
               standardGeneric("sosCreateEventTime")
             })

#
# sosCreateTimeInstant ----
#
if (!isGeneric("sosCreateTimeInstant"))
  setGeneric(name = "sosCreateTimeInstant", def = function(sos, time,
                                                           frame = as.character(NA),
                                                           calendarEraName = as.character(NA),
                                                           indeterminatePosition = as.character(NA)) {
    standardGeneric("sosCreateTimeInstant")
  }
  )

#
# sosCreateTimePeriod ----
#
if (!isGeneric("sosCreateTimePeriod"))
  setGeneric(name = "sosCreateTimePeriod",
             def = function(sos, begin, end,
                            frame = as.character(NA),
                            calendarEraName = as.character(NA),
                            indeterminatePosition = as.character(NA),
                            duration = as.character(NA),
                            timeInterval = NULL) {
               standardGeneric("sosCreateTimePeriod")
             }
  )

#
# sosCreateEventTimeList ----
#
if (!isGeneric("sosCreateEventTimeList"))
  setGeneric(name = "sosCreateEventTimeList",
             def = function(time, operator = sosDefaultTemporalOperator) {
               standardGeneric("sosCreateEventTimeList")
             })

#
# sosCreateTime ----
#
if (!isGeneric("sosCreateTime"))
  setGeneric(name = "sosCreateTime",
             def = function(sos, time, operator = sosDefaultTemporalOperator) {
               standardGeneric("sosCreateTime")
             })

#
# sosCreateFeatureOfInterest ----
#
if (!isGeneric("sosCreateFeatureOfInterest"))
  setGeneric(name = "sosCreateFeatureOfInterest",
             def = function(objectIDs = list(NA), spatialOps = NULL, bbox = NULL,
                            srsName = NA_character_) {
               standardGeneric("sosCreateFeatureOfInterest")
             })

#
# sosCreateBBOX ----
#
if (!isGeneric("sosCreateBBOX"))
  setGeneric(name = "sosCreateBBOX",
             def = function(lowLat, lowLon, uppLat, uppLon, srsName,
                            srsDimension = NA_integer_, axisLabels = NA_character_,
                            uomLabels = NA_character_,
                            propertyName = sosDefaultSpatialOpPropertyName) {
               standardGeneric("sosCreateBBOX")
             })

#
# sosCreateBBoxMatrix ----
#
if (!isGeneric("sosCreateBBoxMatrix"))
  setGeneric(name = "sosCreateBBoxMatrix",
             def = function(lowLat, lowLon, uppLat, uppLon) {
               standardGeneric("sosCreateBBoxMatrix")
             })

#
# sosCapabilitiesDocumentOriginal ----
#
if (!isGeneric("sosCapabilitiesDocumentOriginal"))
  setGeneric(name = "sosCapabilitiesDocumentOriginal", def = function(sos,
                                                                      verbose = FALSE) {
    standardGeneric("sosCapabilitiesDocumentOriginal")
  })

#
# sosCapabilitiesUrl ----
#
if (!isGeneric("sosCapabilitiesUrl"))
  setGeneric(name = "sosCapabilitiesUrl",
             def = function(sos) {
               standardGeneric("sosCapabilitiesUrl")
             })
