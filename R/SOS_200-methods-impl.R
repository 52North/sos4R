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
# visit the Free Software Foundation web page, http://www.fsf.org.             #
#                                                                              #
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)                          #
# Created: 2013-08-28                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
################################################################################

#
# main request method ----
#
.sosRequest_2.0.0 <- function(sos, request, verbose = FALSE, inspect = FALSE) {
  if (inspect) cat("[.sosRequest_2.0.0] REQUEST:\n", toString(request), "\n")

  .response = ""

  # get encoding function for the respective method
  .encodingFunction <- sos@encoders[[sos@binding]]
  if (verbose) {
    .f <- functionBody(.encodingFunction)
    cat("[.sosRequest_2.0.0] Encoding Function (beginning of function body): ",
        substring(text = .f, first = 0, last = 60), " ... [",
        max((length(.f) - 60), 0), " more chrs].\n")
  }

  # encode!
  .encodedRequest = .encodingFunction(obj = request, sos = sos,
                                      verbose = verbose)

  if (sos@binding == .sosBindingKVP) {
    .dcp <- list("Get", "application/x-kvp", sos@url)

    if (sos@useDCPs) {
      if (verbose)
        cat("[.sosRequest_2.0.0] Using DCP from operation description.\n")

      .dcp <- sosGetDCP(sos, sosName(request), owsGetName)

      if (is.null(.dcp) || is.na(.dcp) || !length(.dcp)) {
        .dcp <- list("Get", "application/x-kvp", sos@url)
        if (verbose) cat("[.sosRequest_2.0.0] Could not get DCP from operation description. This is OK for first GetCapabilities request, using ", toString(.dcp), "\n")
      }

      if (is.list(.dcp) && length(.dcp) && is.list(.dcp[[1]])) {
        .dcp <- .sosFilterDCPs(dcp = .dcp,
                               pattern = sos@dcpFilter[[.sosBindingKVP]],
                               verbose = verbose)
        .dcp <- unlist(.dcp)
      }

      if (verbose) cat("[.sosRequest_2.0.0] Using DCP:", toString(.dcp), "\n")
    }
    else if (verbose) cat("[.sosRequest_2.0.0] Not using DCP from capabilities, but use ", toString(.dcp), "\n")

    if (isTRUE(grep(pattern = "[\\?]", x = .dcp[[owsDcpUrlIndex]]) > 0)) {
      if (verbose) cat("Given url already contains a '?', appending arguments!\n")
      .url = paste0(.dcp[[owsDcpUrlIndex]], .encodedRequest)
    }
    else .url = paste(.dcp[[owsDcpUrlIndex]], .encodedRequest, sep = "?")

    if (!is.na(sos@additionalKVPs) && length(sos@additionalKVPs) > 0) {
      .kvps <- sos@additionalKVPs

      if (verbose)
        cat("[.sosRequest_2.0.0] adding extra KVP parameters:\n\t",
            toString(.kvps))

      .kvpsString <- .encodeAdditionalKVPs(.kvps)
      .url <- paste(.url, .kvpsString, sep = "&")
    }

    if (inspect) cat("[.sosRequest_2.0.0] GET!\n[.sosRequest_2.0.0] REQUEST:\n\t", .url, "\n")

    if (verbose) cat("[.sosRequest_2.0.0] Do GET request...\n")

    .response = httr::GET(url = .url,
                          httr::accept_xml())
    .content <- .processResponse(.response, verbose)

    if (verbose) cat("[.sosRequest_2.0.0] ... done.\n")
  }
  else if (sos@binding == .sosBindingPOX) {
    if (inspect) {
      cat("[.sosRequest_2.0.0] POST!\n[.sosRequest_2.0.0] REQUEST:\n")
      print(.encodedRequest)
    }

    .dcp <- list("Post", "application/xml", sos@url)

    if (sos@useDCPs) {
      .dcp <- sosGetDCP(sos, sosName(request), owsPostName) #sos@url as fallback
      if (is.null(.dcp) || is.na(.dcp) || !length(.dcp)) {
        .dcp <- list("Post", "application/xml", sos@url)
        if (verbose) cat("[.sosRequest_2.0.0] Could not get DCP from operation description. This is OK for first GetCapabilities request. Using", toString(.dcp), "\n")
      }
      else if (verbose) cat("[.sosRequest_2.0.0] Got DCPs from capabilites:", toString(.dcp), "\n")

      if (is.list(.dcp) && length(.dcp) && is.list(.dcp[[1]])) {
        .dcp <- .sosFilterDCPs(dcp = .dcp,
                               pattern = sos@dcpFilter[[.sosBindingPOX]],
                               verbose = verbose)
        .dcp <- unlist(.dcp)
      }

      if (verbose) cat("[.sosRequest_2.0.0] Using DCP:", toString(.dcp), "\n")
    }
    else if (verbose)
      cat("[.sosRequest_2.0.0] *NOT* using DCP from capabilities:", .dcp, "\n")

    .requestString <- toString(.encodedRequest)

    # using 'POST' for application/xml encoded requests
    if (verbose) cat("[.sosRequest_2.0.0] Do request...\n")

    .response <- httr::POST(url = .dcp[[owsDcpUrlIndex]],
                            httr::content_type_xml(),
                            httr::accept_xml(),
                            body = .requestString )
    .content <- .processResponse(.response, verbose)

    if (verbose) cat("[.sosRequest_2.0.0] ... done.")
  }
  else if (sos@binding == .sosBindingSOAP) {
    if (inspect) {
      print("[.sosRequest_2.0.0] SOAP! REQUEST:\n")
      print(.encodedRequest)
    }

    stop("[sos4R] ERROR: SOAP is not implemented for SOS 2.0.0.\n")
  }
  else {
    stop(paste("Unsupported method, has to be one of",
               SosSupportedBindings(), "but is", sos@binding))
  }

  if (inspect) cat("[.sosRequest_2.0.0] RESPONSE:\n", toString(.content), "\n")
  return(.content)
}

#
# operation functions ----
#
.getCapabilities_2.0.0 <- function(sos, verbose, inspect, sections,
                                   acceptFormats, updateSequence, owsVersion,	acceptLanguages) {
  if (verbose) {
    cat("[.getCapabilities_2.0.0] of", sosUrl(sos), "\n")
  }

  .gc <- OwsGetCapabilities(service = sosService,
                            acceptVersions = c(sosVersion(sos)), sections = sections,
                            acceptFormats = acceptFormats, updateSequence = updateSequence,
                            owsVersion = owsVersion, acceptLanguages = acceptLanguages)
  if (verbose) cat("[.getCapabilities_2.0.0] REQUEST:\n", toString(.gc), "\n")

  .response = sosRequest(sos = sos,
                         request = .gc,
                         verbose = verbose,
                         inspect = inspect)
  if (inspect) {
    cat("[.getCapabilities_2.0.0] RESPONSE DOC:\n")
    print(.response)
  }

  if (.isExceptionReport(.response)) {
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

.describeSensor_2.0.0 <- function(sos, procedure,
                                  procedureDescriptionFormat,
                                  validTime,
                                  verbose, inspect, saveOriginal) {
  if (verbose) cat("[.describeSensor_2.0.0] ", procedure, "@", sos@url, "\n")

  # check inputs: procedure
  procedures = unique(unlist(sosProcedures(sos)))
  if (!any(procedure %in% procedures)) warning("Requested procedure(s) not listed in capablities, service might return error!")

  # check inputs: description format
  supportedFormats <- sosOperation(sos, sosDescribeSensorName)@parameters[["procedureDescriptionFormat"]]
  if (!(procedureDescriptionFormat %in% supportedFormats))
    warning(paste("outputFormat has to be one of", paste(supportedFormats, sep = ", ",
                                                         collapse = "', '"),
                  "'. Requested format is '", procedureDescriptionFormat, "'."))

  # check if multiple sensors
  if (length(procedure) > 1) {
    if (verbose) cat("[.describeSensor_2.0.0] multiple sensors: ", procedure, "\n")

    descriptions <- list()
    for (p in procedure) {
      description <- .describeSensor_2.0.0(sos = sos,
                                          procedure = p,
                                          procedureDescriptionFormat = procedureDescriptionFormat,
                                          validTime = validTime,
                                          verbose = verbose,
                                          inspect = inspect,
                                          saveOriginal = saveOriginal)
      descriptions <- c(descriptions, description)
    }

    return(descriptions)
  }

  # single procedure
  ds <- SosDescribeSensor(service = sosService,
                           version = sos@version,
                           procedure = procedure,
                           procedureDescriptionFormat = procedureDescriptionFormat,
                           validTime = validTime)
  if (verbose) cat("[.describeSensor_2.0.0] REQUEST:\n", toString(ds), "\n")

  response = sosRequest(sos = sos,
                        request = ds,
                        verbose = verbose,
                        inspect = inspect)

  filename <- NULL
  if (!is.null(saveOriginal)) {
    if (is.character(saveOriginal)) {
      filename <- saveOriginal
      if (verbose) cat("Using saveOriginal parameter for file name:", filename, "\n")
    }
    else if (is.logical(saveOriginal)) {
      if (saveOriginal) filename <- paste(cleanupFileName(procedure), ".xml", sep = "")
      if (verbose) cat("Generated file name:", filename, "\n")
    }

    if (verbose) cat("[.describeSensor_2.0.0] Saving original document...", filename, "in", getwd(), "\n")

    xml2::write_xml(x = response, file = filename)
    cat("[sos4R] Original document saved:", filename, "\n")
  }

  if (.isExceptionReport(response)) {
    return(.handleExceptionReport(sos, response))
  }
  else {
    parsingFunction <- sosParsers(sos)[[swesDescribeSensorResponseName]]
    sml <- parsingFunction(obj = response, sos = sos, verbose = verbose)

    return(sml)
  }
}

#
# encoding functions XML ----
#
.sosEncodeRequestXMLGetObservationById_2.0.0 <- function(obj, sos) {
  xmlDoc <- xml2::xml_new_root(sosGetObservationByIdName)
  xml2::xml_set_attrs(x = xmlDoc,
                      value = c(xmlns = sos200Namespace,
                                service = obj@service,
                                version = obj@version,
                                "xmlns:xsi" = xsiNamespace,
                                "xmlns:sos20" = sos200Namespace))

  for (i in 1:length(obj@observationId)) {
    xml2::xml_add_child(xmlDoc, sos200ObservationName, obj@observationId[[i]])
  }

  return(xmlDoc)
}

.sosEncodeRequestXMLDescribeSensor_2.0.0 <- function(obj, sos) {
  xmlDoc <- xml2::xml_new_root(swesDescribeSensorName)
  xml2::xml_set_attrs(x = xmlDoc,
                      value = c(xmlns = sos@namespaces[[swesNamespacePrefix]],
                                service = obj@service,
                                version = obj@version,
                                "xmlns:xsi" = xsiNamespace,
                                "xmlns:swes" = sos@namespaces[[swesNamespacePrefix]]))

  xml2::xml_add_child(xmlDoc, swesProcedureName, obj@procedure)
  xml2::xml_add_child(xmlDoc, swesProcedureDescriptionFormatName, obj@procedureDescriptionFormat)

  if (!is.null(obj@validTime)) {
    validTime <- xml2::xml_add_child(xmlDoc, swesValidTimeName)
    xml2::xml_add_child(validTime, encodeXML(obj = obj@validTime, sos = sos))
  }

  return(xmlDoc)
}

#
# encoding functions KVP ----
#
.sosEncodeRequestKVPGetObservationById_2.0.0 <- function(obj, sos, verbose = FALSE) {
  requestBase <- paste(
    paste("service", .kvpEscapeSpecialCharacters(x = obj@service), sep = "="),
    paste("version", .kvpEscapeSpecialCharacters(x = obj@version), sep = "="),
    paste("request", .kvpEscapeSpecialCharacters(x = sosGetObservationByIdName), sep = "="),
    sep = "&")

  escapedIds <-  sapply(X = obj@observationId, FUN = .kvpEscapeSpecialCharacters)
  observation <- paste("observation", paste(escapedIds, collapse = ","), sep = "=")
  kvpString <- paste(requestBase, observation, sep = "&")

  if (verbose) cat("[.sosEncodeRequestKVPGetObservationById_2.0.0] ", kvpString)
  return(kvpString)
}

# defined in SWES 2.0, OGC 09-001
.sosEncodeRequestKVPDescribeSensor_2.0.0 <- function(obj, sos, verbose = FALSE) {
  requestBase <- .kvpBuildRequestBase(sos, sosDescribeSensorName)

  # one mandatory:
  procedure <- paste("procedure", .kvpEscapeSpecialCharacters(x = obj@procedure), sep = "=")
  format <- paste("procedureDescriptionFormat",
                  .kvpEscapeSpecialCharacters(x = gsub(obj@procedureDescriptionFormat,
                                                       pattern = "&quot;",
                                                       replacement = '"')),
                  sep = "=")

  kvpString <- paste(requestBase,
                     procedure,
                     format,
                     sep = "&")

  # zero or one (optional)
  if (!is.null(obj@validTime)) {
    kvpString <- paste0(kvpString, "&", "validTime=",
                        .kvpEscapeSpecialCharacters(
                          encodeKVP(obj = obj@validTime, sos = sos, verbose = verbose))
                        )
  }

  if (verbose) cat("[.sosEncodeRequestKVPDescribeSensor_2.0.0] ", kvpString)

  return(kvpString)
}
