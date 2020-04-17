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
# Created: 2013-08-28                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

#
# main request method ----
#
.sosRequest_2.0.0 <- function(sos, request, verbose = FALSE, inspect = FALSE) {
  if (inspect) cat("[.sosRequest_2.0.0] REQUEST:\n", toString(request), "\n")

  response = ""

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
    dcp <- list("Get", "application/x-kvp", sos@url)

    if (sos@useDCPs) {
      if (verbose)
        cat("[.sosRequest_2.0.0] Using DCP from operation description.\n")

      dcp <- sosGetDCP(sos, sosName(request), owsGetName)

      if (is.list(dcp) && length(dcp) && is.list(dcp[[1]])) {
        dcp <- .sosFilterDCPs(dcp = dcp,
                               pattern = sos@dcpFilter[[.sosBindingKVP]],
                               verbose = verbose)
        dcp <- unlist(dcp)
      }

      if (any(is.null(dcp)) || any(is.na(dcp)) || !length(dcp)) {
        dcp <- list("Get", "application/x-kvp", sos@url)
        if (verbose) cat("[.sosRequest_2.0.0] Could not get DCP from operation description. This is OK for first GetCapabilities request, using ", toString(dcp), "\n")
      }

      if (verbose) cat("[.sosRequest_2.0.0] Using DCP:", toString(dcp), "\n")
    }
    else if (verbose) cat("[.sosRequest_2.0.0] Not using DCP from capabilities, but use ", toString(dcp), "\n")

    if (isTRUE(grep(pattern = "[\\?]", x = dcp[[owsDcpUrlIndex]]) > 0)) {
      if (verbose) cat("Given url already contains a '?', appending arguments!\n")
      url = paste0(dcp[[owsDcpUrlIndex]], .encodedRequest)
    }
    else url = paste(dcp[[owsDcpUrlIndex]], .encodedRequest, sep = "?")

    if (!is.na(sos@additionalKVPs) && length(sos@additionalKVPs) > 0) {
      .kvps <- sos@additionalKVPs

      if (verbose)
        cat("[.sosRequest_2.0.0] adding extra KVP parameters:\n\t",
            toString(.kvps))

      .kvpsString <- .encodeAdditionalKVPs(.kvps)
      url <- paste(url, .kvpsString, sep = "&")
    }

    if (inspect) cat("[.sosRequest_2.0.0] GET!\n[.sosRequest_2.0.0] REQUEST:\n\t", url, "\n")

    if (verbose) cat("[.sosRequest_2.0.0] Do GET request...\n")

    response = httr::GET(url = url,
                          httr::accept_xml())
    content <- .processResponse(response, verbose)

    if (verbose) cat("[.sosRequest_2.0.0] ... done.\n")
  }
  else if (sos@binding == .sosBindingPOX) {
    if (inspect) {
      cat("[.sosRequest_2.0.0] REQUEST:\n")
      print(.encodedRequest)
    }

    dcp <- list("Post", "application/xml", sos@url)

    if (sos@useDCPs) {
      dcp <- sosGetDCP(sos, sosName(request), owsPostName) #sos@url as fallback

      if (is.list(dcp) && length(dcp) && is.list(dcp[[1]])) {
        dcp <- .sosFilterDCPs(dcp = dcp,
                               pattern = sos@dcpFilter[[.sosBindingPOX]],
                               verbose = verbose)
        dcp <- unlist(dcp)
      }

      if (any(is.null(dcp)) || any(is.na(dcp)) || !length(dcp)) {
        dcp <- list("Post", "application/xml", sos@url)
        if (verbose) cat("[.sosRequest_2.0.0] Could not get DCP from operation description. This is OK for first GetCapabilities request. Using", toString(dcp), "\n")
      }
      else if (verbose) cat("[.sosRequest_2.0.0] Got DCPs from capabilites:", toString(dcp), "\n")

      if (verbose) cat("[.sosRequest_2.0.0] Using DCP:", toString(dcp), "\n")
    }
    else if (verbose)
      cat("[.sosRequest_2.0.0] *NOT* using DCP from capabilities, but ", toString(dcp), "\n")

    .requestString <- toString(.encodedRequest)

    # using 'POST' for application/xml encoded requests
    if (verbose) cat("[.sosRequest_2.0.0] Do request...\n")

    response <- httr::POST(url = dcp[[owsDcpUrlIndex]],
                            httr::content_type_xml(),
                            httr::accept_xml(),
                            body = .requestString )
    content <- .processResponse(response, verbose)

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

  if (inspect) cat("[.sosRequest_2.0.0] RESPONSE:\n", toString(content), "\n")
  return(content)
}

#
# operation functions ----
#
.getCapabilities_2.0.0 <- function(sos, verbose, inspect, sections,
                                   acceptFormats, updateSequence, owsVersion,	acceptLanguages) {
  if (verbose) {
    cat("[.getCapabilities_2.0.0] of", sosUrl(sos), "\n")
  }

  gc <- OwsGetCapabilities(service = sosService,
                            acceptVersions = c(sosVersion(sos)), sections = sections,
                            acceptFormats = acceptFormats, updateSequence = updateSequence,
                            owsVersion = owsVersion, acceptLanguages = acceptLanguages)
  if (verbose) cat("[.getCapabilities_2.0.0] REQUEST:\n", toString(gc), "\n")

  response = sosRequest(sos = sos,
                        request = gc,
                        verbose = verbose,
                        inspect = inspect)
  if (inspect) {
    cat("[.getCapabilities_2.0.0] RESPONSE DOC:\n")
    print(response)
  }

  if (.isExceptionReport(response)) {
    return(.handleExceptionReport(sos, response))
  }
  else {
    .parsingFunction <- sosParsers(sos)[[sosGetCapabilitiesName]]
    .caps <- .parsingFunction(obj = response, sos = sos)
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
      if (saveOriginal) filename <- paste(.cleanupFileName(procedure), ".xml", sep = "")
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

.getFeatureOfInterest_2.0.0 <- function(sos, featureOfInterest, verbose, inspect, saveOriginal){

  filename <- NULL
  #
  #   if (!is.null(saveOriginal)) {
  #     if (is.character(saveOriginal)) {
  #       filename <- paste(saveOriginal, ".xml", sep = "")
  #       if (verbose) cat("Using saveOriginal parameter for file name:",
  #                       filename, "\n")
  #     }
  #     else if (is.logical(saveOriginal)) {
  #       if (saveOriginal) filename <- paste(.cleanupFileName(featureOfInterest),
  #                                           ".xml", sep = "")
  #       if (verbose) cat("Generating file name:", filename, "\n")
  #     }
  #   }

  if (verbose)
    cat("[.getFeatureOfInterest_2.0.0] to ", sos@url, " with featureOfInterest ",
        featureOfInterest, "\n")

  gfoi <- SosGetFeatureOfInterest_2.0.0(sosService, sos@version, featureOfInterest)

  if (verbose)
    cat("[.getFeatureOfInterest_2.0.0] REQUEST:\n\n", toString(gfoi), "\n")

  response <- sosRequest(sos = sos,
                        request = gfoi,
                        verbose = verbose,
                        inspect = inspect)

  if (verbose) cat("[sos4R] Received response (size:", utils::object.size(response), "bytes), parsing ...\n")

  if (!is.null(filename)) {
    xml2::write_xml(x = response, file = filename)
    cat("[sos4R] Original document saved:", filename, "\n")
  }

  if (.isExceptionReport(response)) {
    return(.handleExceptionReport(sos, response))
  }

  parsingFunction <- sosParsers(sos)[[sosGetFeatureOfInterestResponseName]]

  if (verbose) {
    cat("[.getFeatureOfInterest_2.0.0] Parsing with function ")
    print(parsingFunction)
  }

  obs <- parsingFunction(obj = response,
                         sos = sos,
                         verbose = verbose)

  if (verbose) cat("[sos4R] Finished getFeatureOfInterest to", sos@url, "\n")

  return(obs)
}

.getObservation_2.0.0 <- function(sos,
                                  offerings,
                                  observedProperty,
                                  responseFormat,
                                  eventTime,
                                  procedure,
                                  featureOfInterest,
                                  BBOX,
                                  valueReferenceTemporalFilter = sosDefaultTemporalValueReference,
                                  verbose,
                                  inspect,
                                  saveOriginal,
                                  retrieveFOI) {
  filename <- NULL
  if (!is.null(saveOriginal)) {
    if (is.character(saveOriginal)) {
      filename <- saveOriginal
      if (verbose) cat("[.getObservation_2.0.0] Using saveOriginal parameter for file name:",
                       filename, "\n")
    }
    else if (is.logical(saveOriginal) && saveOriginal) {
      filename <- paste(.cleanupFileName(offerings),
                        format(Sys.time(), sosDefaultFilenameTimeFormat),
                        ".xml",
                        sep = "_")
      if (verbose) cat("[.getObservation_2.0.0] Generated file name:", filename, "\n")
    }
  }

  if (verbose) cat("[.getObservation_2.0.0] to ", sos@url, " with offerings ", offerings, "\n")

  # TODO Create filter shape from BBOX
  filterShape <- NULL

  go <- SosGetObservation_2.0.0(service = sosService,
                                version = sos@version,
                                offering = offerings,
                                observedProperty = observedProperty,
                                responseFormat =  responseFormat,
                                temporalFilter = eventTime,
                                procedure = procedure,
                                featureOfInterest = featureOfInterest,
                                spatialFilter = filterShape,
                                valueReferenceTemporalFilter = valueReferenceTemporalFilter)

  response <- sosRequest(sos = sos,
                        request = go,
                        verbose = verbose,
                        inspect = inspect)

  if (!is.null(filename)) {
    xml2::write_xml(x = response, file = filename)
  }

  if (.isExceptionReport(response)) {
    return(.handleExceptionReport(sos, response))
  }

  if (inherits(response, "xml_document")) {
    if (verbose) cat("[.getObservation_2.0.0] Got XML document as response.\n")
    if (!is.na(responseFormat) &&
        isTRUE(grep(pattern = "text/xml", x = responseFormat) != 1)) {
      warning("Got XML string, but request did not require text/xml (or subtype).")
    }

    parsingFunction <- sosParsers(sos)[[sosGetObservationResponseName]]

    if (verbose) {
      cat("[.getObservation_2.0.0] Parsing with function ")
      print(parsingFunction)
    }

    obs <- parsingFunction(obj = response,
                           sos = sos,
                           verbose = verbose,
                           retrieveFOI = retrieveFOI)

    # calculate result length vector
    if (inherits(obs, "OmObservationCollection")) {
      if (verbose) cat("[.getObservation_2.0.0] Got OmObservationCollection",
                       "... calculating length with sosResult()")

      result <- sosResult(obs, bind = FALSE, coordinates = FALSE)
      if (verbose) cat("[.getObservation_2.0.0] result: ", toString(result))

      .resultLength <- sapply(result, nrow)
      if (length(.resultLength) == 0){
        # nothing
        .resultLength <- 0
      }
    }
    else if (is.list(obs) && length(obs) > 0 && all(sapply(obs, function(o) { class(o) == "OmOM_Observation"}))) {
      .resultLength <- sum(sapply(obs, function(o){ !is.null(o@result)}))
    }
    else .resultLength <- NA

    if (verbose) {
      cat("[.getObservation_2.0.0] PARSED RESPONSE:",
          class(obs), "\n")
      cat("[.getObservation_2.0.0] Result length(s): ",
          toString(.resultLength), "\n")
    }

    if (is.list(obs) && any(sapply(obs, is.null))) {
      .countInfo <- paste("NO DATA, turn on 'verbose' for more information.")
    }
    else {
      .countInfo <- paste(sum(.resultLength), "result values")
      if (verbose) .countInfo <- paste0(.countInfo,  " [",
                                        toString(.resultLength), "].")
    }

    if (verbose) cat("[sos4R] Finished getObservation to", sos@url,
                     "\n\t--> received", length(obs), "observation(s) having", .countInfo , "\n")
    if (!is.null(filename)) cat("[sos4R] Original document saved:", filename, "\n")

    return(obs)
  }
  else {# response is NOT an XML document:
    if (verbose)
      cat("[.getObservation_2.0.0] Did NOT get XML document as response, trying to parse with",
          responseFormat, "\n")

    if (mimeTypeCSV == responseFormat) {
      if (inspect) {
        cat("[.getObservation_2.0.0] CSV RESPONSE:\n")
        print(response)
      }

      .parsingFunction <- sosParsers(sos)[[mimeTypeCSV]]
      .csv <- .parsingFunction(obj = response, verbose = verbose)

      if (!is.null(filename)) {
        filename <- paste(file = filename, ".csv", sep = "")
        utils::write.csv(.csv, filename)
      }

      if (verbose) cat("[sos4R] Finished getObservation to", sos@url, "\n\t",
                       "--> received observations with dimensions", toString(dim(.csv)), "\n")
      if (!is.null(filename)) cat("[sos4R] Original document saved:", filename, "\n")

      return(.csv)
    }
  } # else

  # not xml nor csv nore otherwise handled
  if (inspect) {
    cat("[.getObservation_2.0.0] UNKNOWN RESPONSE FORMAT; Response string: \n'")
    print(response)
    warning("Unknown response format!")
  }

  if (!is.null(filename)) {
    save(response, file = filename)
    cat("[sos4R] Saved original document:", filename)
  }

  return(response)
}

#
# encoding functions XML ----
#
.sosEncodeRequestXMLGetObservation_2.0.0 <- function(obj, sos, verbose = FALSE) {
  xmlDoc <- xml2::xml_new_root(sosGetObservationName)
  xml2::xml_set_attrs(x = xmlDoc,
                      value = c(xmlns = sos200Namespace,
                                service = obj@service,
                                version = obj@version,
                                "xmlns:xsi" = xsiNamespace,
                                "xmlns:sos" = sos200Namespace,
                                "xmlns:sos20" = sos200Namespace,
                                "xmlns:om20" = om20Namespace,
                                "xmlns:fes" = fesNamespace,
                                "xmlns:gml" = gml32Namespace))

  for (op in obj@observedProperty) {
    xml2::xml_add_child(xmlDoc, sosObservedPropertyName, op)
  }

  for (off in obj@offering) {
    xml2::xml_add_child(xmlDoc, sosOfferingName, off)
  }

  for (p in obj@procedure) {
    xml2::xml_add_child(xmlDoc, sosProcedureName, p)
  }

  if (length(obj@temporalFilter) > 0) {
    temporalFilterList <- lapply(X = obj@temporalFilter,
                                 FUN = encodeXML,
                                 sos = sos,
                                 verbose = verbose)
    for (filter in temporalFilterList) {
      tf <- xml2::xml_add_child(xmlDoc, sos200TemporalFilterName)
      operator <- xml2::xml_add_child(tf, fesDuringName)
      xml2::xml_add_child(operator, fesValueReferenceName, obj@valueReferenceTemporalFilter)
      xml2::xml_add_child(operator, filter)
    }
  }

  for (foi in obj@featureOfInterest) {
    xml2::xml_add_child(xmlDoc, sosFeatureOfInterestName, foi)
  }

  if (!is.null(obj@spatialFilter)) {
    warning("GetObservation contains spatialFilter, but that is not supported for 'POST' and ignored.")
  }

  if (!is.na(obj@responseFormat)) {
    rF <- gsub(obj@responseFormat, pattern = "&quot;", replacement = "\"")
    xml2::xml_add_child(xmlDoc, sosResponseFormatName, rF)
  }

  return(xmlDoc)
}

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
# See Table 50: GetObservation request KVP encoding
#
.sosEncodeRequestKVPGetObservation_2.0.0 <- function(obj, sos, verbose = FALSE) {
  if (verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0] encoding", toString(obj), "\n")

  mandatory <- .kvpBuildRequestBase(sos, sosGetObservationName)
  if (verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0]", "mandatory elements: ", mandatory, "\n")

  optionals <- c()
  namespaces <- c()

  if (length(obj@offering) > 0) {
    offering <- .kvpKeyAndValues(sosKVPParamNameOffering, obj@offering)
    optionals <- c(optionals, offering)
  }

  if (length(obj@observedProperty) > 0) {
    observedProperty <- .kvpKeyAndValues(sosKVPParamNameObsProp, obj@observedProperty)
    optionals <- c(optionals, observedProperty)
  }

  if (length(obj@procedure) > 0) {
    procedure <- .kvpKeyAndValues(sosKVPParamNameProcedure, obj@procedure)
    optionals <- c(optionals, procedure)
  }

  if (length(obj@featureOfInterest) > 0) {
    featureOfInterest <- .kvpKeyAndValues(sosKVPParamNameFoi, obj@featureOfInterest)
    optionals <- c(optionals, featureOfInterest)
  }

  if (length(obj@temporalFilter) > 0) {
    if (verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0] Adding temporalFilter", toString(obj@temporalFilter),
                     " with valueReference ", toString(obj@valueReferenceTemporalFilter), "\n")
    if (length(obj@temporalFilter) > 1)
      warning("Discarding additional temporal filters, using only first one in KVP encoding.")

    namespaces <- c(namespaces, paste0("xmlns(", om20NamespacePrefix, ",", om20Namespace, ")"))

    timeString <- encodeKVP(obj = obj@temporalFilter[[1]], sos = sos, verbose = verbose)

    if (!is.na(obj@valueReferenceTemporalFilter))
      timeString <- paste0(obj@valueReferenceTemporalFilter, ",", timeString)

      optionals <- c(optionals, paste("temporalFilter",
                                      .kvpEscapeSpecialCharacters(x = timeString),
                                      sep = "="))
  }

  if (!is.null(obj@spatialFilter)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0] Adding spatialFilter ", obj@spatialFilter, "\n")
    namespaces <- c(namespaces, paste0("xmlns(", samsNamespacePrefix, ",", samsNamespace, ")"))
    # TODO: implement SamsShape encoding

    # use sosCreateBBOX to create Envelope!
  }

  if (!is.na(obj@responseFormat)) {
    responseFormat <- .kvpKeyAndValues(sosKVPParamNameResponseFormat, obj@responseFormat)
    optionals <- c(optionals, responseFormat)
  }

  if (length(namespaces) > 0) {
    optionals <- c(optionals, .kvpKeyAndValues(sosKVPParamNameNamespaces, namespaces))
  }

  if (verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0]", "optional elements: ", toString(optionals), "\n")

  kvpString <- paste(mandatory, paste(optionals, collapse = "&"), sep = "&")

  if (verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0]",
                   "Finished KVP string creation:\n", kvpString, "\n")

  return(kvpString)
}

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
