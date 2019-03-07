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
# Project: sos4R - visit the project web page: https://github.com/52North/sos4R #
#                                                                              #
################################################################################

#
#
#
parseOwsOperation <- function(obj) {
  .name <- xml2::xml_attr(x = obj, attr = "name")

  .dcpsXML <- .filterXmlChildren(obj, owsDCPName)
  .dcps <- list()
  for (.dcp in .dcpsXML) {
    .http <- .dcp[[owsHTTPName]]
    .endpoints <- c(
      .filterXmlChildren(.http, owsGetName),
      .filterXmlChildren(.http, owsPostName))

    for (.ep in .endpoints) {
      .newEndpoint <- list(xml2::xml_attr(x = .ep, attr = "href"))
      names(.newEndpoint) <- xml2::xml_name(x = .ep)
      .dcps <- c(.dcps, .newEndpoint)
    }
  }

  .parametersXML <- .filterXmlChildren(obj, owsParameterName)
  .parameters = list()
  .names = list()

  if (length(.parametersXML) > 0) {
    for (.p in .parametersXML) {
      .allowedValues <- NULL
      .ranges <- NULL
      .allowedValuesAndRanges <- NULL

      # check for ows:AnyValue
      if (length(.p[owsAnyValueName]) > 0)
        .allowedValuesAndRanges = list(owsAnyValueName)
      else {
        # list of allowed values
        .xpathAllowedValues <- paste("./", owsNamespacePrefix, ":",
                                     owsAllowedValuesName, "/", owsNamespacePrefix, ":",
                                     owsValueName, sep = "")
        .allowedValues <- xml2::xml_text(
          xml2::xml_find_all(x = .p,
                             xpath = .xpathAllowedValues,
                             ns = SosAllNamespaces()))
        # list of ranges
        .xpathRanges <- paste("./", owsNamespacePrefix, ":",
                              owsAllowedValuesName, "/", owsNamespacePrefix, ":",
                              owsRangeName, sep = "")
        .ranges <-  sapply(xml2::xml_find_all(x = .p,
                                              xpath = .xpathRanges,
                                              ns = SosAllNamespaces()),
                           parseOwsRange)
        .allowedValuesAndRanges <- c(.allowedValues, .ranges)
      }

      .names <- c(.names, xml2::xml_attr(x =  .p, attr = "name"))
      .parameters[[length(.parameters) + 1]] <- .allowedValuesAndRanges
      # the following does NOT work as it recursively concatenates the
      # lists: .parameters <- c(.parameters, .allowedValuesAndRanges)
    }

    names(.parameters) <- .names
  }

  if(any(sapply(names(obj), "==", owsConstraintName)))
    warning("constraint elements are NOT processed!")
  .constraints = list(NA)

  if(any(sapply(names(obj), "==", owsMetadataName)))
    warning("metadata elements are NOT processed!")
  .metadata = list(NA)

  .op <- OwsOperation(name = .name, DCPs = .dcps,
                      parameters = .parameters, constraints = .constraints,
                      metadata = .metadata)
  return(.op)
}

#
# parsing an ows:ExceptionReport ---
#
parseOwsExceptionReport <- function(obj, verbose = FALSE) {
  if (verbose) cat("[parseOwsExceptionReport] Starting ...\n")
  .docRoot <- xml2::xml_root(x = obj)
  ## print(.docRoot)

  .version <- xml2::xml_attr(x = .docRoot, attr = "version")
  .lang <- xml2::xml_attr(x = .docRoot, attr = "lang", default = NA_character_)

  .exceptionsXML <- xml2::xml_find_all(x = .docRoot,
                                       xpath = paste0("//", owsExceptionName),
                                       ns = SosAllNamespaces())

  .exceptions = sapply(.exceptionsXML, parseOwsException)
  if (verbose) cat("[parseOwsExceptionReport]", length(.exceptions), "exceptions.")

  .report <- OwsExceptionReport(version = .version, lang = .lang, exceptions = .exceptions)

  return(.report)
}

#
# parsing an ows:Exception ----
#
parseOwsException <- function(obj) {
  .code <- xml2::xml_attr(x = obj, attr = "exceptionCode")
  .locator <- xml2::xml_attr(x = obj, attr = "locator", default = NA_character_)

  .text <- xml2::xml_find_all(x = obj,
                              xpath = owsExceptionTextName,
                              ns = SosAllNamespaces())

  if (!is.na(.text))
    .text <- xml2::xml_text(x = .text)
  else .text <- as.character(NA)

  .exception <- OwsException(exceptionCode = .code,
                             exceptionText = .text,
                             locator = .locator)

  return(.exception)
}

#
#
#
parseOwsServiceIdentification <- function(obj) {
  #	print("parsing ows service identification!")

  .children <- xml2::xml_children(x = obj)
  .serviceType <- sapply(.filterXmlChildren(obj, owsServiceTypeName),
                         xmlValue)
  .serviceTypeVersion <- sapply(.filterXmlChildren(obj,
                                                   owsServiceTypeVersionName),
                                xmlValue)
  .title <- sapply(.filterXmlChildren(obj, owsTitleName),
                   xmlValue)

  # optional:
  if(!is.na(xml2::xml_children(x = obj)[owsProfileName]))
    .profile <- lapply(.filterXmlChildren(obj, owsProfileName), xmlValue)
  else .profile <- c(NA)

  if(!is.na(xml2::xml_children(x = obj)[owsAbstractName]))
    .abstract <- lapply(.filterXmlChildren(obj, owsAbstractName), xmlValue)
  else .abstract <- c(NA)

  if(!is.na(xml2::xml_children(x = obj)[owsKeywordsName])) {
    .keywordLists <- .filterXmlChildren(obj, owsKeywordsName)
    .keywords <- c(lapply(.keywordLists, FUN = xmlToList), recursive = TRUE)
    .keywords <- lapply(.keywords, gsub, pattern = "^[[:space:]]+|[[:space:]]+$",
                        replacement = "") # http://finzi.psych.upenn.edu/R/Rhelp02a/archive/40714.html
  }
  else .keywords <- c(NA)

  if(!is.na(xml2::xml_children(x = obj)[owsFeesName]))
    .fees <- paste(sapply(.filterXmlChildren(obj, owsFeesName), xmlValue))
  else .fees <- as.character(NA)

  if(!is.na(xml2::xml_children(x = obj)[owsAccessConstraintsName]))
    .accessConstraints <- lapply(.filterXmlChildren(obj,
                                                    owsAccessConstraintsName),
                                 xmlValue)
  else .accessConstraints <- c(NA)

  .si <- OwsServiceIdentification(serviceType =  .serviceType,
                                  serviceTypeVersion = .serviceTypeVersion, profile = .profile,
                                  title = .title, abstract = .abstract, keywords = .keywords,
                                  fees = .fees, accessConstraints = .accessConstraints)
}

#
#
#
parseOwsServiceProvider <- function(obj) {
  #print("parsing ows service provider!")
  .name <- xml2::xml_text(x = obj[[owsProviderNameName]])

  # optional:
  if (!is.null(xml2::xml_children(x = obj)[[owsProviderSiteName]]))
    .site <- xml2::xml_attr(x = obj[[owsProviderSiteName]], attr = "href", default = NA_character_)
  else .site <- as.character(NA)

  if (!is.null(xml2::xml_children(x = obj)[[owsServiceContactName]])) {
    .contact <- obj[[owsServiceContactName]]
    .sp <- OwsServiceProvider(providerName = .name, providerSite = .site,
                              serviceContact = .contact)
  }
  else .sp <- OwsServiceProvider(providerName = .name, providerSite = .site)

  return(.sp)
}

#
# all elements are optional
#
parseOwsRange <- function(obj) {
  .children <- xml2::xml_children(x = obj)

  .minimumXml <- .children[[owsMinimumValueName]]
  if(is.null(.minimumXml)) {
    .minimum <- as.character(NA)
  } else {
    .minimum <- xml2::xml_text(x = .minimumXml)
  }


  .maximumXml <- .children[[owsMaximumValueName]]
  if(is.null(.maximumXml)) {
    .maximum <- as.character(NA)
  } else {
    .maximum <- xml2::xml_text(x = .maximumXml)
  }

  .closure <- xml2::xml_attr(x = obj, attr = "rangeClosure")
  if(is.null(.closure)) {
    .closure <- as.character(NA)
  }

  .spacingXml <- .children[[owsSpacingName]]
  if(is.null(.spacingXml)) {
    .spacing <- as.character(NA)
  } else {
    .spacing <- xml2::xml_text(x = .spacingXml)
  }

  .range <- OwsRange(minimumValue = .minimum, maximumValue = .maximum,
                     rangeClosure = .closure, spacing = .spacing)

  return(.range)
}


#
# If includeNamed is TRUE:
#     returns a list of all child nodes with xmlTagName of node.
# If includeNamed is FALSE:
#     return a list of all child nodes of node not having xmlTagName.
#
.filterXmlChildren <- function(node, xmlTagName, includeNamed = TRUE,
                               verbose = FALSE) {
  .temp <- xml2::xml_children(x = node)

  if(verbose) {
    cat("[.filterXmlChildren] Children:\n")
    print(.temp)
  }

  .filtered <- c()
  .names <- c()
  for (.x in .temp) {
    if(includeNamed && xml2::xml_name(x = .x) == xmlTagName) {
      .filtered <- c(.filtered, .x)
      if(verbose) cat("[.filterXmlChildren] Added", xml2::xml_name(x = .x), "\n")
      .names <- c(.names, xml2::xml_name(x = .x))
    }
    else if(!includeNamed && xml2::xml_name(x = .x) != xmlTagName) {
      .filtered <- c(.filtered, .x)
      if(verbose) cat("[.filterXmlChildren] Added", xml2::xml_name(x = .x), "\n")
      .names <- c(.names, xml2::xml_name(x = .x))
    }
  }
  names(.filtered) <- .names
  rm(.temp)
  rm(.names)
  return(.filtered)
}

.filterXmlOnlyNoneTexts <- function(node) {
  .filterXmlChildren(
    node = node,
    xmlTagName = xmlTextNodeName, includeNamed = FALSE)
}
