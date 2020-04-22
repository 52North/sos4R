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

############################################################################## #
# construction functions
#
OwsGetCapabilities <- function(
  service,
  acceptVersions,
  sections = sosDefaultGetCapSections,
  acceptFormats = sosDefaultGetCapAcceptFormats,
  updateSequence = c(as.character(NA)),
  owsVersion = sosDefaultGetCapOwsVersion,
  acceptLanguages = c(NA)) {
  if (owsVersion == "1.1.0") {
    if (!any(sapply(acceptLanguages, "is.na"), na.rm = TRUE))
      warning("Parameter 'acceptLanguages' is lost because it is not included in 1.1.0!")
    new("OwsGetCapabilities_1.1.0",
        request = sosGetCapabilitiesName,
        version = "1.1.0",
        service = service,
        acceptVersions = acceptVersions,
        sections = sections,
        acceptFormats = acceptFormats,
        updateSequence = updateSequence)
  }
  else if (owsVersion == "2.0.0") {
    new("OwsGetCapabilities_2.0.0",
        request = sosGetCapabilitiesName,
        version = "2.0.0",
        service = service,
        acceptVersions = acceptVersions,
        sections = sections,
        acceptFormats = acceptFormats,
        updateSequence = updateSequence,
        acceptLanguages = acceptLanguages)
  }
  else {
    new("OwsGetCapabilities",
        request = sosGetCapabilitiesName, version = "NONE",
        service = service, acceptVersions = acceptVersions,
        owsVersion = owsVersion)
  }
}

OwsCapabilities <- function(
  version,
  updateSequence = NA,
  owsVersion = sosDefaultGetCapOwsVersion,
  identification = NULL,
  provider = NULL,
  operations = NULL,
  contents = NULL,
  languages = NULL) {
  if (owsVersion == "1.1.0") {
    if (!is.na(languages))
      warning("Parameter 'languages' is lost because it is not included in 1.1.0!")
    new("OwsCapabilities_1.1.0",
        version = version, updateSequence = updateSequence,
        owsVersion = owsVersion, identification = identification,
        provider = provider, operations = operations,
        contents = contents)
  }
  else if (owsVersion == "2.0.0") {
    new("OwsCapabilities_2.0.0",
        version = version, updateSequence = updateSequence,
        owsVersion = owsVersion, identification = identification,
        provider = provider, operations = operations,
        contents = contents, languages = languages)
  }
  else {
    new("OwsCapabilities",
        version = version, updateSequence = updateSequence,
        owsVersion = owsVersion)
  }
}

OwsServiceIdentification <- function(serviceType, serviceTypeVersion,
                                     profile = c(NA), title, abstract = c(NA), keywords = c(NA),
                                     fees = as.character(NA), accessConstraints = c(NA)) {
  new("OwsServiceIdentification",
      serviceType = serviceType, serviceTypeVersion = serviceTypeVersion,
      profile = profile, title = title, abstract = abstract,
      keywords = keywords, fees = fees,
      accessConstraints = accessConstraints)
}

OwsServiceProvider <- function(providerName,
                               providerSite = as.character(NA),
                               serviceContact = xml2::xml_missing()) {
  new("OwsServiceProvider", providerName = providerName,
      providerSite = providerSite, serviceContact = serviceContact)
}

OwsOperationsMetadata <- function(operations,
                                  parameters = list(NA),
                                  constraints = list(NA),
                                  extendedCapabilities = xml2::xml_missing()) {
  new("OwsOperationsMetadata", operations = operations,
      parameters = parameters, constraints = constraints,
      extendedCapabilities = extendedCapabilities)
}

OwsOperation <- function(name, DCPs,
                         parameters = list(NA),
                         constraints = list(NA),
                         metadata = list(NA)) {
  new("OwsOperation", name = name, DCPs = DCPs, parameters = parameters,
      constraints = constraints, metadata = metadata)
}

OwsContents <- function(xml) {
  new("OwsContents", xml = xml)
}

OwsExceptionReport <- function(version, lang = as.character(NA),
                               exceptions = list(NA)) {
  new("OwsExceptionReport", version = version, lang = lang,
      exceptions = exceptions)
}

OwsException <- function(exceptionCode, exceptionText = c(),
                         locator = as.character(NA)) {
  new("OwsException", exceptionCode = exceptionCode,
      exceptionText = exceptionText,
      locator = locator)
}

OwsRange <- function(minimumValue = as.character(NA),
                     maximumValue = as.character(NA), rangeClosure = as.character(NA),
                     spacing = as.character(NA)) {
  new("OwsRange", minimumValue = minimumValue, maximumValue = maximumValue,
      rangeClosure = rangeClosure, spacing = spacing)
}

#
# KVP helper methods ----
#

# to add (possible) multiple values in kvp
.kvpKeyAndValues <- function(key, values) {
  if (is(values, "vector")) {
    .values <- sapply(values, .kvpEscapeSpecialCharacters)
    valueList <- paste(.values, collapse = ",")
    return(paste(key, valueList, sep = "="))
  }
  else {
    return(paste(key, .kvpEscapeSpecialCharacters(x = values), sep = "="))
  }
}

#
# Method to excape characters within values (!) of a parameter. This function
# cannot be called on the whole request string!
#
# See http://www.ietf.org/rfc/rfc2396.txt and
# http://www.oostethys.org/best-practices/best-practices-get and
# http://www.opengeospatial.org/standards/common (Section 11.3)
# and maybe also http://www.blooberry.com/indexdot/html/topics/urlencoding.htm
# and http://www.degraeve.com/reference/urlencoding.php
#
# Special character  	Escaped encoding
# :					 	        %3A
# / 					        %2F
# # 					        %23
# ? 					        %3F
# = 					        %3D
# (space)             %20
#
.kvpEscapeSpecialCharacters <- function(x) {
  if (is.list(x) && length(x) == 1 && isS4(x[[1]])) {
    x <- x[[1]]
  }
  if (isS4(x)) {
   stop("KVP escape not supported for S4 objects")
  }
  utils::URLencode(x, reserved = TRUE)
}

#
# KVP encoding ----
#
setMethod(f = "encodeRequestKVP", "OwsGetCapabilities",
          definition = function(obj, sos, verbose = FALSE) {
            .sosEncodeRequestKVPGetCapabilities(obj, verbose)
          })

.sosEncodeRequestKVPGetCapabilities <- function(obj, verbose = FALSE) {
  .service <- paste(
    "service",
    .kvpEscapeSpecialCharacters(x = obj@service),
    sep = "=")
  .request <- paste(
    "request",
    .kvpEscapeSpecialCharacters(x = obj@request),
    sep = "=")

  .kvpString <- paste(.service, .request, sep = "&")

  return(.kvpString)
}

setMethod(f = "encodeRequestKVP", "OwsGetCapabilities_1.1.0",
          function(obj, sos, verbose = FALSE) {
            .sosEncodeRequestKVPGetCapabilities_1.1.0(obj, verbose)
          })
.sosEncodeRequestKVPGetCapabilities_1.1.0 <- function(obj, verbose = FALSE) {
  .mandatory <- .sosEncodeRequestKVPGetCapabilities(obj, verbose)

  .optionals = ""
  if (!is.na(obj@acceptVersions)) {
    .optionals <- paste(.optionals, .kvpKeyAndValues("acceptVersions",
                                                     obj@acceptVersions), sep = "&")
  }

  if (!any(sapply(obj@sections, "is.na"), na.rm = TRUE)) {
    .optionals <- paste(.optionals, .kvpKeyAndValues("sections",
                                                     obj@sections), sep = "&")
  }

  if (!is.na(obj@updateSequence)) {
    .optionals <- paste(.optionals, .kvpKeyAndValues("updateSequence",
                                                     obj@updateSequence), sep = "&")
  }

  if (!any(sapply(obj@acceptFormats, "is.na"), na.rm = TRUE)) {
    .optionals <- paste(.optionals, .kvpKeyAndValues("acceptFormats",
                                                     obj@acceptFormats), sep = "&")
  }

  .kvpString <- paste(.mandatory, .optionals, sep = "")

  if (verbose) cat("[.sosEncodeRequestKVPGetCapabilities_1.1.0] done: ",
                  .kvpString, "\n")

  return(.kvpString)
}

setMethod(f = "encodeRequestKVP", "OwsGetCapabilities_2.0.0",
          function(obj, sos, verbose = FALSE) {
            .sosEncodeRequestKVPGetCapabilities_2.0.0(obj, verbose)
          })
.sosEncodeRequestKVPGetCapabilities_2.0.0 <- function(obj, verbose = FALSE) {
  .kvpString <- .sosEncodeRequestKVPGetCapabilities_1.1.0(obj)

  if (!any(sapply(obj@acceptLanguages, "is.na"), na.rm = TRUE)) {
    .kvpString <- paste(.kvpString, .kvpKeyAndValues("acceptLanguages",
                                                     obj@acceptLanguages), sep = "&")
  }

  if (verbose) cat("[.sosEncodeRequestKVPGetCapabilities_2.0.0] done: ",
                  .kvpString, "\n")

  return(.kvpString)
}

#
# XML encoding ----
#
setMethod("encodeRequestXML", "OwsGetCapabilities_1.1.0",
          function(obj, sos, verbose = FALSE) {
            if (verbose) {
              cat("[encodeRequestXML]", class(obj), "\n")
            }

            return(.sosEncodeRequestXMLOwsGetCapabilities_1.1.0(obj = obj, sos = sos))
          }
)

.sosEncodeRequestXMLOwsGetCapabilities_1.1.0 <- function(obj, sos) {
  xmlDoc <- xml2::xml_new_root(sosGetCapabilitiesName)
  xml2::xml_set_attrs(x = xmlDoc,
                      value = c(xmlns = sos100Namespace,
                                "xmlns:xsi" = xsiNamespace,
                                service = obj@service,
                                "xmlns:ows" = owsNamespace,
                                "xmlns:ogc" = ogcNamespace),
                      ns = sos@namespaces)

  # optional:
  if (!is.na(obj@acceptVersions)) {
    acceptVersions <- xml2::xml_add_child(xmlDoc, owsAcceptVersionsName)
    for (v in obj@acceptVersions) {
      xml2::xml_add_child(acceptVersions, owsVersionName, v)
    }
  }

  if (!any(sapply(obj@sections, "is.na"), na.rm = TRUE)) {
    sections <- xml2::xml_add_child(xmlDoc, owsSectionsName)
    for (section in obj@sections) {
      xml2::xml_add_child(sections, owsSectionName, section)
    }
  }

  if (!is.na(obj@updateSequence)) {
    xml2::xml_set_attr(x = xmlDoc, attr = "updateSequence", value = obj@updateSequence)
  }

  if (!any(sapply(obj@acceptFormats, "is.na"), na.rm = TRUE)) {
    acceptFormats <- xml2::xml_add_child(xmlDoc, owsAcceptFormatsName)
    for (format in obj@acceptFormats) {
      xml2::xml_add_child(acceptFormats, owsOutputFormatName, format)
    }
  }

  return(xmlDoc)
}

setMethod("encodeRequestXML", "OwsGetCapabilities_2.0.0",
          function(obj, sos, verbose = FALSE) {
            if (verbose) {
              cat("[encodeRequestXML]", class(obj), "\n")
            }

            return(.sosEncodeRequestXMLOwsGetCapabilities_2.0.0(obj = obj, sos = sos))
          }
)

.sosEncodeRequestXMLOwsGetCapabilities_2.0.0 <- function(obj, sos) {
  xmlDoc <- xml2::xml_new_root(sosGetCapabilitiesName)
  xml2::xml_set_attrs(x = xmlDoc,
                      value = c(xmlns = sos200Namespace,
                                "xmlns:xsi" = xsiNamespace,
                                service = obj@service,
                                "xmlns:ows" = owsNamespace,
                                "xmlns:ogc" = ogcNamespace),
                      ns = sos@namespaces)

  # optional:
  if (!is.na(obj@acceptVersions)) {
    acceptVersions <- xml2::xml_add_child(xmlDoc, owsAcceptVersionsName)
    for (v in obj@acceptVersions) {
      xml2::xml_add_child(acceptVersions, owsVersionName, v)
    }
  }

  if (!any(sapply(obj@sections, "is.na"), na.rm = TRUE)) {
    sections <- xml2::xml_add_child(xmlDoc, owsSectionsName)
    for (section in obj@sections) {
      xml2::xml_add_child(sections, owsSectionName, section)
    }
  }

  if (!is.na(obj@updateSequence)) {
    xml2::xml_set_attr(x = xmlDoc, attr = "updateSequence", value = obj@updateSequence)
  }

  if (!any(sapply(obj@acceptFormats, "is.na"), na.rm = TRUE)) {
    acceptFormats <- xml2::xml_add_child(xmlDoc, owsAcceptFormatsName)
    for (format in obj@acceptFormats) {
      xml2::xml_add_child(acceptFormats, owsOutputFormatName, format)
    }
  }

  if (!any(sapply(obj@acceptLanguages, "is.na"), na.rm = TRUE)) {
    acceptLangs <- xml2::xml_add_child(xmlDoc, owsAcceptLanguagesName)
    for (lang in obj@acceptLanguages) {
      xml2::xml_add_child(acceptLangs, owsLanguageName, lang)
    }
  }

  return(xmlDoc)
}

#
# SOAP encoding ----
#
setMethod("encodeRequestSOAP", "OwsGetCapabilities",
          function(obj, sos, verbose = FALSE) {
            if (verbose) {
              cat("ENCODE SOAP ", class(obj), "\n")
            }

            stop("[encodeRequestSOAP] SOAP functionality not implemented yet...")
          }
)
