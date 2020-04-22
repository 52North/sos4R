############################################################################## #
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
# visit the Free Software Foundation web page, http://www.fsf.org.             #
#                                                                              #
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)                          #
# Created: 2010-06-18                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

#
# This class is inspired by a suggestion from Duncan Murdoch
# (https://stat.ethz.ch/pipermail/r-help/2010-July/245480.html)
#

# List of the default parsing functions. The names of the list are the
# names of the respective XML documents set in Constants.R.
# For SOS 1.0.0, the matching is done via operation name.
# For SOS 2.0.0, the matching is done via root name of the response document.
.createDefaultParsers <- function() {
  .defP <- list(
    parseSosCapabilities,
    parseOwsExceptionReport,
    #
    parseSensorML,
    parseDescribeSensorResponse,
    #
    parseOM,
    parseOM,
    #
    parseGetDataAvailabilityResponse,
    parseGetFeatureOfInterestResponse,
    parseGetObservationByIdResponse,
    parseGetObservationResponse,
    #
    parseMeasurement,
    parseObservationProperty,
    parseObservation,
    parseObservationCollection,
    parseResult,
    parseDataArray,
    parseElementType,
    parseEncoding,
    parseValues,
    parseSwePosition,
    parseSweLocation,
    parseSweVector,
    parseSweCoordinate,
    #
    parseGeometryObservation,
    parseCategoryObservation,
    parseCountObservation,
    parseTruthObservation,
    parseTemporalObservation,
    parseComplexObservation,
    #
    parseWmlMeasurementTimeseries,
    #
    parseCSV,
    parseOM,
    parseOM)

  names(.defP) <- list(
    sosGetCapabilitiesName,
    owsExceptionReportName,
    #
    sosDescribeSensorName,
    swesDescribeSensorResponseName,
    #
    sosGetObservationName,
    sosGetObservationByIdName,
    #
    gdaGetDataAvailabilityResponseName,
    sosGetFeatureOfInterestResponseName,
    sosGetObservationByIdResponseName,
    sosGetObservationResponseName,
    #
    omMeasurementName,
    omMemberName,
    omObservationName,
    omObservationCollectionName,
    omResultName,
    sweDataArrayName,
    sweElementTypeName,
    sweEncodingName,
    sweValuesName,
    swePositionName,
    sweLocationName,
    sweVectorName,
    sweCoordinateName,
    #
    omGeometryObservationName,
    omCategoryObservationName,
    omCountObservationName,
    omTruthObservationName,
    omTemporalObservationName,
    omComplexObservationName,
    #
    wmlMeasurementTimeseriesName,
    #
    mimeTypeCSV,
    mimeTypeOM,
    mimeTypeXML)

  return(.defP)
}

.sosDefaultParsers <- .createDefaultParsers()

# Using a different approach for the encoders here, because there is more than
# one way of encoding something (in contrast to parsing). So the different
# objects (and versions) override the respective encoding functions.
.sosDefaultEncoders <- list(
  encodeRequestSOAP,
  encodeRequestXML,
  encodeRequestKVP)
names(.sosDefaultEncoders) <- list(
  .sosBindingSOAP,
  .sosBindingPOX,
  .sosBindingKVP
)

#
#
#
.sosDefaultFieldConverters <- list(
  sosConvertDouble, # fallback <- numeric values exptected more often than character values!
  sosConvertTime,
  sosConvertTime,
  sosConvertTime,
  sosConvertTime,
  sosConvertTime,
  sosConvertTime,
  sosConvertTime,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertDouble,
  sosConvertString, # urn:ogc:data:feature
  sosConvertString,
  sosConvertDouble,
  sosConvertTime,
  sosConvertDouble,
  sosConvertDouble,
  # 52N SOS 4.x
  sosConvertTime,
  # R classes
  sosConvertFactor,
  sosConvertDouble,
  sosConvertInteger,
  sosConvertString,
  sosConvertLogical,
  sosConvertTime
)
names(.sosDefaultFieldConverters) <- list(
  "fallBack",
  "urn:ogc:data:time:iso8601",
  "urn:ogc:property:time:iso8601",
  "urn:ogc:phenomenon:time:iso8601",
  "http://www.opengis.net/def/property/OGC/0/SamplingTime",
  "http://www.opengis.net/def/property/OGC/0/PhenomenonTime",
  "urn:ogc:def:parameter:x-istsos:1.0:time:iso8601",
  sosTimeName,
  "m", # meter
  "m2", # square meter
  "m3", # cubic meter
  "m^3/s", # cubic meter per second
  "s", # second
  "ms", # millisecond
  "us", # microsecond
  "g", # gram
  "rad", # radian
  "K", # Kelvin
  "C", # Coulomb
  "cd", # candela
  "%", # percent
  "ppth", # parts per thousand
  "ppm", # parts per million
  "ppb", # parts per billion
  "pptr", # parts per trillion
  "mol", # mole
  "sr", # steradian
  "Hz", # Hertz
  "N", # Newton
  "Pa", # Pascal (pressure)
  "J", # Joule (energy)
  "W", # Watt (power)
  "A", # Ampere (electric current)
  "V", # Volt
  "F", # Farad
  "Ohm", # Ohm
  "S", # Siemens
  "Wb", # Weber
  "Cel", # degree Celsius
  "T", # Tesla (magnetic flux density)
  "H", # Henry (inductance)
  "lm", # lumen (luminous flux)
  "lx", # lux (illuminance)
  "Bq", # Becquerel (radioactivity)
  "Gy", # Gray (energy dose)
  "Sv", # Sievert (dose equivalent)
  "gon", # gon, grade
  "deg", # degree
  "'", # minute
  "''", # second
  "l", # liter
  "L", # liter
  "ar", # are (area)
  "t", # tonne (mass)
  "bar", # bar (pressure)
  "u", # unified atomic mass unit (mass)
  "eV", # electronvolt (energy)
  "AU", # astronomic unit (length)
  "pc", # parsec (length)
  "degF", # degree Fahrenheit
  "hPa", # hektopascal
  "mm", # millimeter
  "nm", # nanometer
  "cm", # centimeter
  "km", # kilometer
  "m/s", # meter per second
  "m2/s", # square meter per second
  "m3/s", # cubic meter per second
  "kg", # kilogramm
  "mg", # milligram
  "uom", # fallback if actual unit is not given
  "urn:ogc:data:feature",
  "http://www.opengis.net/def/property/OGC/0/FeatureOfInterest",
  "ug/m3", # micrograms per cubic meter
  "http://www.opengis.net/def/uom/ISO-8601/0/Gregorian",
  "degC", # degree celsius
  "\u00B0C", # degree Celsius
  # 52N SOS 4.x
  "http://www.opengis.net/def/property/OGC/0/PhenomenonTime",
  # convert R classes
  "factor",
  "numeric",
  "integer",
  "character",
  "logical",
  "POSIXct"
)


#
# access methods
#
SosDataFieldConvertingFunctions <- function (..., include = character(0),
                                             exclude = character(0)) {
  .merge(els = list(...), defaults = .sosDefaultFieldConverters,
         include = include, exclude = exclude)
}

SosDefaultBinding <- function() {
  return(.sosBindingPOX)
}

SosEncodingFunctions <- function (..., include = character(0),
                                  exclude = character(0)) {
  .merge(els = list(...), defaults = .sosDefaultEncoders,
         include = include, exclude = exclude)
}

SosParsingFunctions <- function (..., include = character(0),
                                 exclude = character(0)) {
  .merge(els = list(...), defaults = .sosDefaultParsers,
         include = include, exclude = exclude)
}

#
# Function originally written by Duncan Temple Lang for the package SSOAP
# (http://www.omegahat.org/SSOAP/, file SOAP.S), adapted here to include add all
# defaults if an element with the same name is not given.
#
# Order: 	FIRST adding defaults for not given names,
#			THEN inclusion, THEN exclusion
#
.merge <- function (els, defaults, include = NULL, exclude = NULL) {
  if (length(els) > 0) {
    # which elements are given?
    .which <- match(names(defaults), names(els))
    #		cat("given names: ", names(defaults))
    # add defaults (including names) for all that are not given
    .missing <- is.na(.which)
    .missingNames <- names(defaults)[.missing]
    #		cat("missing names: ", .missingNames)
    els[.missingNames] <- defaults[.missing]
  }
  # no replacements given, base in-/exclusion on all defaults
  else els <- defaults

  if (length(include)) {
    els <- els[include]
  }
  else if (length(exclude)) {
    .which <- match(exclude, names(els))
    if (any(!is.na(.which)))
      els <- els[-(.which[!is.na(.which)])]
  }

  return(els)
}

#
# Dummy parsing function if a user wants to inspect the responses unprocessed.
# This works for all but capabilities, as these need to be requested on creating
# a new SOS instance.
#
parseNoParsing <- function(obj, sos) {
  # sos parameter is not used!
  return(obj)
}
.sosDisabledParsers <- list(
  parseSosCapabilities, # if this is removed, no more SOS instances can be created!
  parseNoParsing,
  parseNoParsing,
  parseNoParsing,
  parseNoParsing)
names(.sosDisabledParsers) <- list(
  sosGetCapabilitiesName,
  sosDescribeSensorName,
  sosGetObservationName,
  sosGetObservationByIdName,
  owsExceptionReportName)
SosDisabledParsers <- function() {
  #	attributes(.sosDisabledParsers) <- list("isDisabledParsers" = TRUE)
  return(.sosDisabledParsers)
}

#
#
#
SosResetParsingFunctions <- function(sos) {
  sos@parsers <- .createDefaultParsers()

  return(sos)
}

#
# other defaults ----
#
sosDefaultCharacterEncoding <- "UTF-8"
sosDefaultDescribeSensorOutputFormat <- SosSupportedResponseFormats()[2]
sosDefaultGetCapSections <- c("All")
sosDefaultGetCapAcceptFormats <- c("text/xml")
sosDefaultGetCapOwsVersion <- "1.1.0"
sosDefaultGetObsResponseFormat <- SosSupportedResponseFormats()[[1]]
sosDefaultTimeFormat <- "%Y-%m-%dT%H:%M:%OS%z"
sosDefaultFilenameTimeFormat <- "%Y-%m-%d_%H-%M-%OS"
sosDefaultTempOpPropertyName <- "om:samplingTime"
sosDefaultTemporalOperator <- SosSupportedTemporalOperators()[[ogcTempOpTMDuringName]]
sosDefaultTemporalValueReference <- "om:phenomenonTime"
sosDefaultSpatialOpPropertyName <- "urn:ogc:data:location"

sos200DefaultGetObsResponseFormat <- mimeTypeOM20

# use for the names created data.frames
sosDefaultColumnNameFeatureIdentifier <- "feature"
sosDefaultColumnNameLat <- "lat"
sosDefaultColumnNameLon <- "lon"
sosDefaultColumnNameSRS <- "SRS"

# Created by library(RColorBrewer); brewer.pal(12, "Paired")
sosDefaultColorPalette <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C",
                            "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A",
                            "#FFFF99", "#B15928")

sosDefaultReferenceFrameSensorDescription <- "urn:ogc:def:crs:EPSG:4326"

#
# Defaults ----
#
SosDefaults <- function() {
  .defaults <- list(sosDefaultCharacterEncoding,
                    sosDefaultDescribeSensorOutputFormat,
                    sosDefaultGetCapSections,
                    sosDefaultGetCapAcceptFormats,
                    sosDefaultGetCapOwsVersion,
                    sosDefaultGetObsResponseFormat,
                    sosDefaultTimeFormat,
                    sosDefaultFilenameTimeFormat,
                    sosDefaultTempOpPropertyName,
                    sosDefaultTemporalOperator,
                    sosDefaultSpatialOpPropertyName,
                    sosDefaultColumnNameFeatureIdentifier,
                    sosDefaultColumnNameLat,
                    sosDefaultColumnNameLon,
                    sosDefaultColumnNameSRS,
                    sosDefaultReferenceFrameSensorDescription)
  names(.defaults) <- list("sosDefaultCharacterEncoding",
                           "sosDefaultDescribeSensorOutputFormat",
                           "sosDefaultGetCapSections",
                           "sosDefaultGetCapAcceptFormats",
                           "sosDefaultGetCapOwsVersion",
                           "sosDefaultGetObsResponseFormat",
                           "sosDefaultTimeFormat",
                           "sosDefaultFilenameTimeFormat",
                           "sosDefaultTempOpPropertyName",
                           "sosDefaultTemporalOperator",
                           "sosDefaultSpatialOpPropertyName",
                           "sosDefaultColumnNameFeatureIdentifier",
                           "sosDefaultColumnNameLat",
                           "sosDefaultColumnNameLon",
                           "sosDefaultColumnNameSRS",
                           "sosDefaultReferenceFrameSensorDescription")

  return(.defaults)
}

SosDefaults2 <- function (...) {
  .merge(els = list(...), defaults = SosDefaults())
}

#
#
#
SosDefaultDCPs <- function() {
  .defaults <- list()
  .names <- list()
  for (.x in SosSupportedBindings()) {
    .names <- c(.names, .x)
    .defaults <- c(.defaults, .getDcpDefaultPattern(.x))
  }
  names(.defaults) <- .names
  return(.defaults)
}

.sosDefaultParsingOptions <- c(
  #"NOERROR",
  "RECOVER"
)
SosDefaultParsingOptions <- function() {
  return(.sosDefaultParsingOptions)
}

.getDcpDefaultPattern <- function(dcp) {
  pattern <- switch(dcp,
                    "GET" = "application/x-kvp",
                    "POST" = "application/xml",
                    "KVP" = "application/x-kvp",
                    "POX" = "application/xml",
                    "*"
  )
}
