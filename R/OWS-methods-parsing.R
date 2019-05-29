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
# parsing an ows:Operation ----
#
parseOwsOperation <- function(obj, sos) {
  name <- xml2::xml_attr(x = obj, attr = "name")

  dcpsXML <- xml2::xml_find_all(x = obj, xpath = owsDCPName, ns = sos@namespaces)
  dcps <- list()
  for (dcp in dcpsXML) {
    http <- xml2::xml_child(x = dcp, search = owsHTTPName, ns = sos@namespaces)
    endpoints <- xml2::xml_find_all(x = http, xpath = paste0(owsGetName, "|", owsPostName), ns = sos@namespaces)

    for (ep in endpoints) {
      httpMethod <- xml2::xml_name(x = ep, ns = sos@namespaces)
      href <- xml2::xml_attr(x = ep, attr = "xlink:href", ns = sos@namespaces)

      constraints <- xml2::xml_find_all(x = ep, xpath = owsConstraintName, ns = sos@namespaces)
      if (!length(constraints)) {
        contentTypes <- list(NA)
      }
      else {
        contentTypes <- list()

        for (constraint in constraints) {
          if (xml2::xml_attr(x = constraint, attr = "name") == owsContentTypeConstraintName) {

            allowedValuesXml <- xml2::xml_find_all(x = constraint,
                                                   xpath = paste0(owsAllowedValuesName, "/", owsValueName),
                                                   ns = sos@namespaces)
            if (length(allowedValuesXml) > 0) {
              contentTypes <- c(contentTypes, xml2::xml_text(allowedValuesXml))
            }
          }
        }

        if (!length(contentTypes)) contentTypes <- list(NA)
      }

      for (contentType in contentTypes) {
        dcp <- list()
        dcp[[owsDcpHttpMethodIndex]] <- httpMethod
        dcp[[owsDcpContentTypeIndex]] <- contentType
        dcp[[owsDcpUrlIndex]] <- href
        dcps[[length(dcps) + 1]] <- dcp
      }
    }
  }

  names(dcps) <- sapply(X = dcps, FUN = function(x) {x[[owsDcpHttpMethodIndex]]})

  .parametersXML <- xml2::xml_find_all(x = obj, xpath = owsParameterName, ns = sos@namespaces)
  .parameters = list()
  .names = list()

  if (length(.parametersXML) > 0) {
    for (.p in .parametersXML) {
      .allowedValuesAndRanges <- list()

      # check for ows:AnyValue
      if (!is.na(xml2::xml_child(x = .p, search = owsAnyValueName, ns = sos@namespaces))) {
        .allowedValuesAndRanges = list(owsAnyValueName)
      }
      else {
        # try list of allowed values
        .allowedValuesXml <- xml2::xml_find_all(x = .p,
                                                xpath = paste0(owsAllowedValuesName, "/", owsValueName),
                                                ns = sos@namespaces)
        if (length(.allowedValuesXml) > 0)
          .allowedValuesAndRanges <- c(.allowedValuesAndRanges, xml2::xml_text(.allowedValuesXml))

        # try list of ranges
        .rangesXml <- xml2::xml_find_all(x = .p,
                                         xpath = paste0(owsAllowedValuesName, "/", owsRangeName),
                                         ns = sos@namespaces)
        if (length(.rangesXml) > 0)
          .allowedValuesAndRanges <- c(.allowedValuesAndRanges,
                                       sapply(.rangesXml, parseOwsRange))
      }

      .names <- c(.names, xml2::xml_attr(x =  .p, attr = "name"))
      .parameters[[length(.parameters) + 1]] <- .allowedValuesAndRanges
    }

    names(.parameters) <- .names
  }

  if (length(xml2::xml_find_all(x = obj, xpath = owsConstraintName)))
    warning("operation level constraint elements are NOT processed!")
  constraints = list(NA)

  if (length(xml2::xml_find_all(x = obj, xpath = owsMetadataName)))
    warning("operation level metadata elements are NOT processed!")
  metadata = list(NA)

  .op <- OwsOperation(name = name,
                      DCPs = dcps,
                      parameters = .parameters,
                      constraints = constraints,
                      metadata = metadata)
  return(.op)
}

#
# parsing an ows:ExceptionReport ----
#
parseOwsExceptionReport <- function(obj, sos, verbose = FALSE) {
  if (verbose) cat("[parseOwsExceptionReport] Starting ...\n")
  .docRoot <- xml2::xml_root(x = obj)

  .version <- xml2::xml_attr(x = .docRoot, attr = "version")
  .lang <- xml2::xml_attr(x = .docRoot, attr = "lang", default = NA_character_)

  .exceptionsXML <- xml2::xml_find_all(x = .docRoot,
                                       xpath = paste0("//", owsExceptionName),
                                       ns = sos@namespaces)

  .exceptions = sapply(.exceptionsXML, parseOwsException, sos = sos)
  if (verbose) cat("[parseOwsExceptionReport]", length(.exceptions), "exceptions.")

  .report <- OwsExceptionReport(version = .version, lang = .lang, exceptions = .exceptions)

  return(.report)
}

#
# parsing an ows:Exception ----
#
parseOwsException <- function(obj, sos) {
  .code <- xml2::xml_attr(x = obj, attr = "exceptionCode")
  .locator <- xml2::xml_attr(x = obj, attr = "locator", default = NA_character_)

  .text <- xml2::xml_find_all(x = obj,
                              xpath = owsExceptionTextName,
                              ns = sos@namespaces)

  if (!is.na(.text))
    .text <- xml2::xml_text(x = .text)
  else .text <- as.character(NA)

  .exception <- OwsException(exceptionCode = .code,
                             exceptionText = .text,
                             locator = .locator)

  return(.exception)
}

#
# parsing ows:ServiceIdentification ----
#
parseOwsServiceIdentification <- function(obj, sos) {
  .serviceType <- xml2::xml_text(xml2::xml_child(x = obj,
                                                 search = owsServiceTypeName,
                                                 ns = sos@namespaces))
  .serviceTypeVersion <- xml2::xml_text(xml2::xml_child(x = obj,
                                                        search = owsServiceTypeVersionName,
                                                        ns = sos@namespaces))
  .title <- xml2::xml_text(xml2::xml_child(x = obj,
                                           search = owsTitleName,
                                           ns = sos@namespaces))

  # optional:
  .profile <- xml2::xml_text(xml2::xml_find_all(x = obj, xpath = owsProfileName, ns = sos@namespaces))

  .abstract <- xml2::xml_text(xml2::xml_find_all(x = obj, xpath = owsAbstractName, ns = sos@namespaces))

  .keywords <- c(NA)
  .keywordsList <- xml2::xml_child(x = obj, search = owsKeywordsName, ns = sos@namespaces)
  if (!is.na(.keywordsList))
    .keywords <- xml2::xml_text(xml2::xml_find_all(x = .keywordsList,
                                                   xpath = owsKeywordName,
                                                   ns = sos@namespaces))

  .fees <- xml2::xml_text(xml2::xml_find_all(x = obj, xpath = owsFeesName, ns = sos@namespaces))

  .accessConstraints <- xml2::xml_text(xml2::xml_find_all(x = obj, xpath = owsAccessConstraintsName, ns = sos@namespaces))

  .si <- OwsServiceIdentification(serviceType =  .serviceType,
                                  serviceTypeVersion = .serviceTypeVersion,
                                  profile = .profile,
                                  title = .title,
                                  abstract = .abstract,
                                  keywords = .keywords,
                                  fees = .fees,
                                  accessConstraints = .accessConstraints)
  return(.si)
}

#
# parsing ows:ServiceProvider ----
#
parseOwsServiceProvider <- function(obj, sos) {
  .name <- xml2::xml_text(x = xml2::xml_child(x = obj,
                                              search = owsProviderNameName,
                                              ns = sos@namespaces))

  # optional:
  .site <- xml2::xml_child(x = obj, search = owsProviderSiteName, ns = sos@namespaces)
  if (!is.na(.site))
    .site <- xml2::xml_attr(x = .site, attr = "href", default = NA_character_)
  else .site <- as.character(NA)

  .contact <- xml2::xml_child(x = obj, search = owsServiceContactName, ns = sos@namespaces)
  if (!is.na(.contact)) {
    .sp <- OwsServiceProvider(providerName = .name,
                              providerSite = .site,
                              serviceContact = .contact)
  }
  else .sp <- OwsServiceProvider(providerName = .name, providerSite = .site)

  return(.sp)
}

#
# parsing ows:Range ----
# all elements are optional
#
parseOwsRange <- function(obj) {
  .minimumXml <- xml2::xml_child(x = obj, search = owsMinimumValueName)
  if (is.na(.minimumXml)) {
    .minimum <- as.character(NA)
  } else {
    .minimum <- xml2::xml_text(x = .minimumXml)
  }

  .maximumXml <- xml2::xml_child(x = obj, search = owsMaximumValueName)
  if (is.na(.maximumXml)) {
    .maximum <- as.character(NA)
  } else {
    .maximum <- xml2::xml_text(x = .maximumXml)
  }

  .closure <- xml2::xml_attr(x = obj, attr = "rangeClosure")
  if (is.na(.closure)) {
    .closure <- as.character(NA)
  }

  .spacingXml <- xml2::xml_child(x = obj, search = owsSpacingName)
  if (is.na(.spacingXml)) {
    .spacing <- as.character(NA)
  } else {
    .spacing <- xml2::xml_text(x = .spacingXml)
  }

  .range <- OwsRange(minimumValue = .minimum, maximumValue = .maximum,
                     rangeClosure = .closure, spacing = .spacing)

  return(.range)
}

