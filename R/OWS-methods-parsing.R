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
  .name <- XML::xmlGetAttr(node = obj, "name")

  .dcpsXML <- .filterXmlChildren(obj, owsDCPName)
  .dcps <- list()
  for(.dcp in .dcpsXML) {
    .http <- .dcp[[owsHTTPName]]
    .endpoints <- c(
      .filterXmlChildren(.http, owsGetName),
      .filterXmlChildren(.http, owsPostName))

    for(.ep in .endpoints) {
      .newEndpoint <- list(XML::xmlGetAttr(node = .ep, "href"))
      names(.newEndpoint) <- XML::xmlName(node =.ep)
      .dcps <- c(.dcps, .newEndpoint)
    }
  }

  .parametersXML <- .filterXmlChildren(obj, owsParameterName)
  .parameters = list()
  .names = list()

  if(length(.parametersXML) > 0) {
    for(.p in .parametersXML) {
      .allowedValues <- NULL
      .ranges <- NULL
      .allowedValuesAndRanges <- NULL

      # check for ows:AnyValue
      if(length(.p[owsAnyValueName]) > 0)
        .allowedValuesAndRanges = list(owsAnyValueName)
      else {
        # list of allowed values
        .xpathAllowedValues <- paste("./", owsNamespacePrefix, ":",
                                     owsAllowedValuesName, "/", owsNamespacePrefix, ":",
                                     owsValueName, sep = "")
        .allowedValues <- lapply(
          getNodeSet(doc = .p, path = .xpathAllowedValues,
                     namespaces = owsNamespaceContext),
          xmlValue)
        # list of ranges
        .xpathRanges <- paste("./", owsNamespacePrefix, ":",
                              owsAllowedValuesName, "/", owsNamespacePrefix, ":",
                              owsRangeName, sep = "")
        .ranges <-  sapply(
          getNodeSet(doc = .p,
            .xpathRanges,
            owsNamespaceContext
          ),
          parseOwsRange)
        .allowedValuesAndRanges <- c(.allowedValues, .ranges)
      }

      #			cat("[", .name, "] Adding to parameters list for",
      #					XML::xmlGetAttr(node = .p, "name"), ":",
      #					toString(.allowedValuesAndRanges), "\n")

      .names <- c(.names, XML::xmlGetAttr(node = .p, "name"))
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
# method for parsing an ows:ExceptionReport.
#
parseOwsExceptionReport <- function(obj, verbose = FALSE) {
  if(verbose) cat("[parseOwsExceptionReport] Starting ...")
  .docRoot <- XML::xmlRoot(x = obj)
  ## print(.docRoot)

  .version <- XML::xmlGetAttr(node = .docRoot, name = "version")
  .lang <- XML::xmlGetAttr(node = .docRoot, name = "lang", default = NA_character_)

  # remove all elements from docRoot that are not 'Exception'
  # could probably be done nicer with subsetting, but indexing with wildcards or similar (... xmlChildren()[[]] ...) did not work.
  .children <- XML::xmlChildren(x = .docRoot)
  .exceptionsXML <- list()
  for (x in .children) {
    if(XML::xmlName(node =x) == owsExceptionName)
      .exceptionsXML = c(.exceptionsXML, x)
    # else print(XML::xmlName(node =x))
  }

  .exceptions = sapply(.exceptionsXML, parseOwsException)
  if(verbose) cat("[parseOwsExceptionReport]", length(.exceptions),
                  "exceptions.")

  .report <- OwsExceptionReport(version = .version, lang = .lang, exceptions = .exceptions)

  return(.report)
}

#
# parsing a single xml node that is an ows:Exception
#
parseOwsException <- function(obj) {
  #	print("parsing e!")
  .code <- XML::xmlGetAttr(node = obj, name = "exceptionCode")
  .locator <- XML::xmlGetAttr(node = obj, name = "locator",
                         default = NA_character_)

  if(!is.na(XML::xmlChildren(x = obj)[owsExceptionTextName]))
    .text <- XML::xmlValue(x = XML::xmlChildren(x = obj)[[owsExceptionTextName]])
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

  .children <- XML::xmlChildren(x = obj)
  .serviceType <- sapply(.filterXmlChildren(obj, owsServiceTypeName),
                         xmlValue)
  .serviceTypeVersion <- sapply(.filterXmlChildren(obj,
                                                   owsServiceTypeVersionName),
                                xmlValue)
  .title <- sapply(.filterXmlChildren(obj, owsTitleName),
                   xmlValue)

  # optional:
  if(!is.na(XML::xmlChildren(x = obj)[owsProfileName]))
    .profile <- lapply(.filterXmlChildren(obj, owsProfileName), xmlValue)
  else .profile <- c(NA)

  if(!is.na(XML::xmlChildren(x = obj)[owsAbstractName]))
    .abstract <- lapply(.filterXmlChildren(obj, owsAbstractName), xmlValue)
  else .abstract <- c(NA)

  if(!is.na(XML::xmlChildren(x = obj)[owsKeywordsName])) {
    .keywordLists <- .filterXmlChildren(obj, owsKeywordsName)
    .keywords <- c(lapply(.keywordLists, FUN = xmlToList), recursive = TRUE)
    .keywords <- lapply(.keywords, gsub, pattern = "^[[:space:]]+|[[:space:]]+$",
                        replacement = "") # http://finzi.psych.upenn.edu/R/Rhelp02a/archive/40714.html
  }
  else .keywords <- c(NA)

  if(!is.na(XML::xmlChildren(x = obj)[owsFeesName]))
    .fees <- paste(sapply(.filterXmlChildren(obj, owsFeesName), xmlValue))
  else .fees <- as.character(NA)

  if(!is.na(XML::xmlChildren(x = obj)[owsAccessConstraintsName]))
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
  .name <- XML::xmlValue(x = obj[[owsProviderNameName]])

  # optional:
  if(!is.null(XML::xmlChildren(x = obj)[[owsProviderSiteName]]))
    .site <- XML::xmlGetAttr(node = obj[[owsProviderSiteName]],
                        name = "href", default = as.character(NA))
  else .site <- as.character(NA)

  if(!is.null(XML::xmlChildren(x = obj)[[owsServiceContactName]])) {
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
  .children <- XML::xmlChildren(x = obj)

  .minimumXml <- .children[[owsMinimumValueName]]
  if(is.null(.minimumXml)) {
    .minimum <- as.character(NA)
  } else {
    .minimum <- XML::xmlValue(x = .minimumXml)
  }


  .maximumXml <- .children[[owsMaximumValueName]]
  if(is.null(.maximumXml)) {
    .maximum <- as.character(NA)
  } else {
    .maximum <- XML::xmlValue(x = .maximumXml)
  }

  .closure <- XML::xmlGetAttr(node = obj, name = "rangeClosure")
  if(is.null(.closure)) {
    .closure <- as.character(NA)
  }

  .spacingXml <- .children[[owsSpacingName]]
  if(is.null(.spacingXml)) {
    .spacing <- as.character(NA)
  } else {
    .spacing <- XML::xmlValue(x = .spacingXml)
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
  .temp <- XML::xmlChildren(x = node)

  if(verbose) {
    cat("[.filterXmlChildren] Children:\n")
    print(.temp)
  }

  .filtered <- c()
  .names <- c()
  for (.x in .temp) {
    if(includeNamed && XML::xmlName(node =.x) == xmlTagName) {
      .filtered <- c(.filtered, .x)
      if(verbose) cat("[.filterXmlChildren] Added", XML::xmlName(node =.x), "\n")
      .names <- c(.names, XML::xmlName(node =.x))
    }
    else if(!includeNamed && XML::xmlName(node =.x) != xmlTagName) {
      .filtered <- c(.filtered, .x)
      if(verbose) cat("[.filterXmlChildren] Added", XML::xmlName(node =.x), "\n")
      .names <- c(.names, XML::xmlName(node =.x))
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
