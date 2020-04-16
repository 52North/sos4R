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
# Created: 2013-03-06                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
################################################################################

#
# sosRequest ----
#
setMethod(f = "sosRequest",
          signature = signature(sos = "SOS_2.0.0", request = "OwsServiceOperation",
                                verbose = "logical", inspect = "logical"),
          def = function(sos, request, verbose, inspect) {
            .sosRequest_2.0.0(sos = sos, request = request, verbose = verbose,
                              inspect = inspect)
          }
)

#
# getCapabilities ----
#
setMethod(f = "getCapabilities", signature = signature(sos = "SOS_2.0.0"),
          def = function(sos, verbose, inspect, sections, acceptFormats,
                         updateSequence, owsVersion,	acceptLanguages) {
            return(.getCapabilities_2.0.0(sos = sos, verbose = verbose,
                                          inspect = inspect, sections = sections,
                                          acceptFormats = acceptFormats,
                                          updateSequence = updateSequence,
                                          owsVersion = owsVersion,
                                          acceptLanguages = acceptLanguages))
          }
)

#
# describeSensor ----
#
setMethod(f = "describeSensor",
          signature = signature(sos = "SOS_2.0.0", procedure  = "character"),
          def = function(sos, procedure, outputFormat, verbose, inspect,
                         saveOriginal) {
            stop("describeSensor for SOS_2.0.0 not implemented yet!")
          }
)


#
# getObservationById ----
#
#
setMethod(f = "getObservationById",
          signature = signature(sos = "SOS_2.0.0", observationId = "character"),
          def = function(sos, observationId, responseFormat, srsName,
                         resultModel, responseMode, verbose, inspect, saveOriginal) {
            stop("getObservationById for SOS_2.0.0 not implemented yet!")
          }
)

#
# getObservation: offering object ----
#
setMethod(f = "getObservation",
          signature = signature(sos = "SOS_2.0.0",
                                offering = "SosObservationOffering_2.0.0"),
          def = function(sos, offering, observedProperty, responseFormat, srsName,
                         eventTime,	procedure, featureOfInterest, result, resultModel,
                         responseMode, BBOX, verbose, inspect, saveOriginal) {
            .offeringId <- offering@id
            if (verbose)	cat("[getObservation] Requesting offering", .offeringId,
                            "by SosObservationOffering.\n")
            return(.getObservation_2.0.0(sos = sos, offeringId = .offeringId,
                                         observedProperty = observedProperty,
                                         responseFormat = responseFormat,
                                         srsName = srsName, eventTime = eventTime,
                                         procedure = procedure,
                                         featureOfInterest = featureOfInterest,
                                         result = result, resultModel = resultModel,
                                         responseMode = responseMode, BBOX = BBOX,
                                         verbose = verbose,
                                         inspect = inspect, saveOriginal = saveOriginal))
          }
)

#
# getObservation: offering string ----
#
setMethod(f = "getObservation",
          signature = signature(sos = "SOS_2.0.0",
                                offering = "character"),
          def = function(sos, offering, observedProperty = list(), responseFormat,
                         srsName, eventTime,	procedure, featureOfInterest, result,
                         resultModel, responseMode, BBOX, verbose, inspect,
                         saveOriginal) {
            if (verbose)	cat("[getObservation] Requesting offering", offering,
                            "by name.\n")

            .off <- sosOfferings(sos)[[offering]]

            if (length(observedProperty) == 0) {
              .obsProps <- sosObservableProperties(.off)
              if (verbose) cat("[getObservation] Got observation(s) from offering because none given:",
                              toString(.obsProps), "\n")
            }
            else {
              .obsProps <- observedProperty
            }

            return(.getObservation_2.0.0(sos = sos, offeringId = offering,
                                         observedProperty = .obsProps,
                                         responseFormat = responseFormat,
                                         srsName = srsName, eventTime = eventTime,
                                         procedure = procedure,
                                         featureOfInterest = featureOfInterest,
                                         result = result, resultModel = resultModel,
                                         responseMode = responseMode, BBOX = BBOX,
                                         verbose = verbose,
                                         inspect = inspect, saveOriginal = saveOriginal))
          }
)

#
# getFeatureOfInterest ----
#
setMethod(f = "getFeatureOfInterest", signature = signature(sos = "SOS_2.0.0", featureOfInterest = "character"),
          def = function(sos, featureOfInterest, verbose, inspect, saveOriginal) {
            return(.getFeatureOfInterest_2.0.0(sos = sos, featureOfInterest = featureOfInterest,
                                               verbose = verbose, inspect = inspect, saveOriginal=saveOriginal))
          }
)

#
# getFeatureOfInterest - without filter ----
#
setMethod(f = "getFeatureOfInterest", signature = signature(sos = "SOS_2.0.0"),
          def = function(sos, featureOfInterest, verbose, inspect, saveOriginal) {
            return(.getFeatureOfInterest_2.0.0(sos = sos,
                                               featureOfInterest = as.character(NA), # TODO better way possible?
                                               verbose = verbose, inspect = inspect, saveOriginal=saveOriginal))
          }
)

#
#
#
.getFeatureOfInterest_2.0.0 <- function(sos, featureOfInterest, verbose, inspect,
                                        saveOriginal){

  .filename <- NULL
  #
  #   if (!is.null(saveOriginal)) {
  #     if (is.character(saveOriginal)) {
  #       .filename <- paste(saveOriginal, ".xml", sep = "")
  #       if (verbose) cat("Using saveOriginal parameter for file name:",
  #                       .filename, "\n")
  #     }
  #     else if (is.logical(saveOriginal)) {
  #       if (saveOriginal) .filename <- paste(.cleanupFileName(featureOfInterest),
  #                                           ".xml", sep = "")
  #       if (verbose) cat("Generating file name:", .filename, "\n")
  #     }
  #   }

  if (verbose)
    cat("[.getFeatureOfInterest_2.0.0] to ", sos@url, " with featureOfInterest ",
        featureOfInterest, "\n")

  .gfoi <- SosGetFeatureOfInterest_2.0.0(sosService, sos@version, featureOfInterest)

  if (verbose)
    cat("[.getFeatureOfInterest_2.0.0] REQUEST:\n\n", toString(.gfoi), "\n")

  .response = sosRequest(sos = sos,
                         request = .gfoi,
                         verbose = verbose,
                         inspect = inspect)

  if (verbose) cat("[sos4R] Received response (size:", object.size(.response), "bytes), parsing ...\n")

  if (!is.null(.filename)) {
    xml2::write_xml(x = .response, file = .filename)
    if (verbose) cat("[.getFeatureOfInterest_2.0.0] Saved original document:", .filename, "\n")
  }

  if (.isExceptionReport(.response)) {
    return(.handleExceptionReport(sos, .response))
  }

  .parsingFunction <- sosParsers(sos)[[sosGetFeatureOfInterestName]]

  if (verbose) {
    cat("[.getFeatureOfInterest_2.0.0] Parsing with function ")
    print(.parsingFunction)
  }

  .obs <- .parsingFunction(obj = .response, sos = sos,
                           verbose = verbose)

  .msg <- paste("[sos4R] Finished getFeatureOfInterest to", sos@url, "\n")

  if (!is.null(.filename)) {
    .msg <- paste(.msg,
                  "[sos4R] Original document saved:", .filename, "\n")

    .oldAttrs <- attributes(.obs)
    .newAttrs <- list(.filename)
    names(.newAttrs) <- list(sosAttributeFileName)
    if (verbose) cat("[.getObservationById_1.0.0] Appending new attributes",
                    toString(.newAttrs), "(names",
                    toString(names(.newAttrs)), ")\n")

    attributes(.obs) <- c(.oldAttrs, .newAttrs)
  }
  cat(.msg)

  return(.obs)
}

#
# getObservation function ----
#
# TODO: Remove/handle obsolete parameters: resultModel, responseMode, eventTime -> temporalFilter
#
.getObservation_2.0.0 <- function(sos, offeringId, observedProperty,
                                  responseFormat, srsName, eventTime,	procedure, featureOfInterest,
                                  result, resultModel, responseMode, BBOX,
                                  valueReferenceTemporalFilter = sosDefaultTemporalValueReference,
                                  verbose,
                                  inspect,
                                  saveOriginal) {

  .filename <- NULL
  if (!is.null(saveOriginal)) {
    if (is.character(saveOriginal)) {
      .filename <- saveOriginal
      if (verbose) cat("[.getObservation_2.0.0] Using saveOriginal parameter for file name:",
                      .filename, "\n")
    }
    else if (is.logical(saveOriginal) && saveOriginal) {
      .filename <- paste(.cleanupFileName(offeringId),
                         format(Sys.time(), sosDefaultFilenameTimeFormat),
                         ".xml",
                         sep = "_")
      if (verbose) cat("[.getObservation_2.0.0] Generated file name:", .filename, "\n")
    }
  }

  if (verbose) cat("[.getObservation_2.0.0] to ", sos@url, " with offering ", offeringId, "\n")

  .go <- .createGetObservation_2.0.0(sos, offeringId, observedProperty,
                                     responseFormat, srsName, eventTime,	procedure, featureOfInterest,
                                     result, resultModel, responseMode, BBOX,
                                     valueReferenceTemporalFilter,
                                     verbose, inspect,
                                     saveOriginal)

  if (verbose)cat("[.getObservation_2.0.0] REQUEST:\n\n", toString(.go), "\n")

  .response = sosRequest(sos = sos,
                         request = .go,
                         verbose = verbose,
                         inspect = inspect)

  if (verbose) cat("[.getObservation_2.0.0] Received response (size:", object.size(.response), "bytes), parsing ...\n")

  if (!is.null(.filename)) {
    xml2::write_xml(x = .response, file = .filename)
    if (verbose) cat("[.getObservation_2.0.0] Saved original document:", .filename, "\n")
  }

  if (.isExceptionReport(.response)) {
    return(.handleExceptionReport(sos, .response))
  }

  if (inherits(.response, "xml_document")) {
    if (verbose) cat("[.getObservation_1.0.0] Got XML document as response.\n")
    if ( !is.na(responseFormat) &&
        isTRUE(grep(pattern = "text/xml", x = responseFormat) != 1)) {
      warning("Got XML string, but request did not require text/xml (or subtype).")
    }

    .parsingFunction <- sosParsers(sos)[[sosGetObservationName]]

    if (verbose) {
      cat("[.getObservation_2.0.0] Parsing with function ")
      print(.parsingFunction)
    }

    .obs <- .parsingFunction(obj = .response, sos = sos,
                             verbose = verbose)

    # calculate result length vector
    if (inherits(.obs, "OmObservationCollection")) {
      if (verbose) cat("[.getObservation_2.0.0] Got OmObservationCollection",
                      "... calculating length with sosResult()")

      .result <- sosResult(.obs, bind = FALSE, coordinates = FALSE)
      if (verbose) cat("[.getObservation_2.0.0] result: ", toString(.result))

      .resultLength <- sapply(.result, nrow)
      if (length(.resultLength) == 0){
        # nothing
        .resultLength <- 0
      }
    }
    else if (is.list(.obs) && all(sapply(.obs, function(o) { class(o) == "OmOM_Observation"}))) {
      .resultLength <- sum(sapply(.obs, function(o){ !is.null(o@result)}))
    }
    else .resultLength <- NA

    if (verbose) {
      cat("[.getObservation_2.0.0] PARSED RESPONSE:",
          class(.obs), "\n")
      cat("[.getObservation_2.0.0] Result length(s): ",
          toString(.resultLength), "\n")
    }

    if (is.list(.obs) && any(sapply(.obs, is.null))) {
      .countInfo <- paste("NO DATA, turn on 'verbose' for more information.")
    }
    else {
      .countInfo <- paste(sum(.resultLength), "result values")
      if (verbose) .countInfo <- paste0(.countInfo,  " [",
                          toString(.resultLength), "].")
    }

    .msg <- paste("[sos4R] Finished getObservation to", sos@url,
                  "\n\t--> received", length(.obs), "observation(s) having",
                  .countInfo , "\n")
    if (!is.null(.filename)) {
      .msg <- paste(.msg,
                    "[sos4R] Original document saved:", .filename, "\n")

      .oldAttrs <- attributes(.obs)
      .newAttrs <- list(.filename)
      names(.newAttrs) <- list(sosAttributeFileName)
      if (verbose) cat("[.getObservation_2.0.0] Appending new attributes",
                      toString(.newAttrs), "(names",
                      toString(names(.newAttrs)), ")\n")

      attributes(.obs) <- c(.oldAttrs, .newAttrs)
    }
    cat(.msg)

    return(.obs)
  }
  else {# response is NOT an XML document:
    if (verbose)
      cat("[.getObservation_2.0.0] Did NOT get XML document as response, trying to parse with",
          responseFormat, "\n")

    if (mimeTypeCSV == responseFormat) {
      if (inspect) {
        cat("[.getObservation_2.0.0] CSV RESPONSE:\n")
        print(.response)
      }

      .parsingFunction <- sosParsers(sos)[[mimeTypeCSV]]
      .csv <- .parsingFunction(obj = .response, verbose = verbose)

      if (!is.null(.filename)) {
        .filename <- paste(file = .filename, ".csv", sep = "")
        write.csv(.csv, .filename)
      }

      .msg <- paste("[sos4R] Finished getObservation to", sos@url, "\n\t",
                    "--> received observations with dimensions",
                    toString(dim(.csv)), "\n")
      if (!is.null(.filename)) {
        .msg <- paste(.msg,
                      "[sos4R] Original document saved:", .filename, "\n")

        .oldAttrs <- attributes(.csv)
        .newAttrs <- list(.filename)
        names(.newAttrs) <- list(sosAttributeFileName)
        if (verbose) cat("[.getObservation_2.0.0] Appending new attributes",
                        toString(.newAttrs), "(names",
                        toString(names(.newAttrs)), ")\n")

        attributes(.csv) <- c(.oldAttrs, .newAttrs)
      }
      cat(.msg)

      # RETURN ###
      return(.csv)
    } # grep(pattern = mimeTypeCSV...

    # TODO Add other non-XML encodings here.
  } # else

  # not xml nor csv nore otherwise handled
  if (inspect) {
    cat("[.getObservation_2.0.0] UNKNOWN RESPONSE FORMAT; Response string: \n'")
    print(.response)
    warning("Unknown response format!")
  }

  if (!is.null(.filename)) {
    save(.response, file = .filename)
    cat("[sos4R] Saved original document:", .filename)
  }

  return(.response)
}

#
#
#
.createGetObservation_2.0.0 <- function(sos, offeringId, observedProperty,
                                        responseFormat, srsName, eventTime,	procedure, featureOfInterest,
                                        result, resultModel, responseMode, BBOX,
                                        valueReferenceTemporalFilter,
                                        verbose, inspect,
                                        saveOriginal) {
  .go <- SosGetObservation_2.0.0(service = sosService,
                                 version = sos@version,
                                 offering = offeringId,
                                 observedProperty = observedProperty,
                                 responseFormat =  responseFormat,
                                 srsName = srsName,
                                 eventTime = eventTime,
                                 procedure = procedure,
                                 featureOfInterest = featureOfInterest,
                                 result = result,
                                 resultModel = resultModel,
                                 responseMode = responseMode,
                                 BBOX = BBOX,
                                 valueReferenceTemporalFilter = valueReferenceTemporalFilter)
  if (verbose)
    cat("[.createGetObservation_2.0.0] Done:\n", toString(.go), "\n")

  return(.go)
}
#
# checkRequest - GetObservation ----
#
setMethod(f = "checkRequest",
          signature = signature(service = "SOS_2.0.0",
                                operation = "SosGetObservation",
                                verbose = "logical"),
          definition = function(service, operation, verbose) {
            # check if operation is for SOS and operation is GetObservation
            if (!(operation@service == sosService &&
                 operation@request == sosGetObservationName)) {
              stop("Wrong input! Require classes 'SOS_2.0.0' as service and 'GetObservation' as operation.")
              return(FALSE)
            }

            # TODO implement checkRequest for GetObservation

            # check if given responseFormat is supported by the service

            # check if temporal operator and operand are a valid combination according to filter capabilities

            return(TRUE)
          }
)
#
# checkRequest - GetFeatureOfInterest ----
#
setMethod(f = "checkRequest",
          signature = signature(service = "SOS_2.0.0",
                                operation = "SosGetFeatureOfInterest_2.0.0",
                                verbose = "logical"),
          definition = function(service, operation, verbose) {
            # check if operation is for SOS and operation is DescribeSensor
            if (!(operation@service == sosService &&
                 operation@request == sosGetFeatureOfInterestName)) {
              stop("Wrong input! Require classes 'SOS_2.0.0' as service and 'GetFeatureOfInterest' as operation.")
              return(FALSE)
            }

            # TODO implement checkRequest for GetObservation

            # check if given responseFormat is supported by the service

            # check if temporal operator and operand are a valid combination according to filter capabilities

            return(TRUE)
          }
)
#
# encodeRequest - KVP - GetObservation ----
#
setMethod("encodeRequestKVP", "SosGetObservation_2.0.0",
          function(obj, sos, verbose = FALSE) {
            if (obj@version == sos200_version) {
              return(.sosEncodeRequestKVPGetObservation_2.0.0(obj, sos,
                                                              verbose))
            }
            else {
              stop("Version not supported!")
            }
          }
)
#
# encodeRequest - KVP - GetFeatureOfInterest ----
#
setMethod("encodeRequestKVP", "SosGetFeatureOfInterest_2.0.0",
          function(obj, sos, verbose = FALSE) {
            if (obj@version == sos200_version) {
              return(.sosEncodeRequestKVPGetFeatureOfInterest_2.0.0(obj, sos,
                                                                    verbose))
            }
            else {
              stop("Version not supported!")
            }
          }
)

.sosEncodeRequestKVPGetObservation_2.0.0 <- function(obj, sos, verbose = FALSE) {
  if (verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0] encoding", toString(obj), "\n")

  # required:
  .requestBase <- .kvpBuildRequestBase(sos, sosGetObservationName)
  .offering <- paste(sosKVPParamNameOffering,
                     .kvpEscapeSpecialCharacters(x = obj@offering), sep = "=")
  .observedProperty <- .kvpKeyAndValues(sosKVPParamNameObsProp,
                                        obj@observedProperty)

  .mandatory <- paste(.requestBase, .offering,
                      .observedProperty, sep = "&")

  if (verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0]",
                  "mandatory elements: ", .mandatory, "\n")

  # optional:
  .optionals = ""
  # is optional for GET
  if (!is.na(obj@srsName)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0] Adding SRS name ",
                    obj@srsName, "\n")
    .optionals <- paste(.optionals, paste(sosKVPParamNameSrsName,
                                          .kvpEscapeSpecialCharacters(x = obj@srsName),
                                          sep = "="),
                        sep = "&")
  }

  if ( !length(obj@eventTime) == 0) {
    if (verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0] Adding event time",
                    toString(obj@eventTime), " with valueReference ", toString(obj@valueReferenceTemporalFilter), "\n")
    if (length(obj@eventTime) > 1)
      warning("Only first event time in the list is used for KVP!")

    .timeString <- encodeKVP(obj = obj@eventTime[[1]],
                             sos = sos, verbose = verbose)

    # if the eventTime is a latest request, it returns NA, the GET binding
    # says for the latest observation eventTime is omitted
    if (!is.na(.timeString)) {
      # http://www.opengis.net/spec/SOS/2.0/req/kvp-core/go-temporalFilter-encoding
      if (!is.na(obj@valueReferenceTemporalFilter))
        .timeString <- paste0(obj@valueReferenceTemporalFilter, ",", .timeString)

      .optionals <- paste(.optionals, paste("temporalFilter",
                                            .kvpEscapeSpecialCharacters(x = .timeString),
                                            sep = "="),
                          sep = "&")
      .optionals <- paste(.optionals,
                          paste0(
                            "namespaces=",
                            # TODO make namespace configurable (use this as default value)
                            .kvpEscapeSpecialCharacters("xmlns(om,http://www.opengis.net/om/2.0)")
                            ),
                          sep = "&")
    }
    else {
      if (verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0] ",
                      "encodeKVP returned NA for eventTime, omitting",
                      "parameter for request for latest observation.")
    }
  }

  if ( !any(sapply(obj@procedure, "is.na"), na.rm = TRUE)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0] Adding procedures ",
                    obj@procedure, "\n")
    .optionals <- paste(.optionals,
                        .kvpKeyAndValues(sosKVPParamNameProcedure, obj@procedure),
                        sep = "&")
  }

  if ( !is.null(obj@featureOfInterest)) {
    .foiIDs <- obj@featureOfInterest@objectIDs

    if (verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0] Adding features ",
                    toString(obj@featureOfInterest), "by IDs ", toString(.foiIDs), "\n")

    .optionals <- paste(.optionals,
                        .kvpKeyAndValues(sosKVPParamNameFoi, .foiIDs),
                        sep = "&")
  }

  if ( !is.null(obj@result)) {
    warning("'result' is not supported for 'GET' - parameter is discarded, use another method to include it!")
  }

  if ( !is.na(obj@resultModel)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0] Adding result model ",
                    obj@resultModel, "\n")
    .optionals <- paste(.optionals, paste(sosKVPParamNameResultModel,
                                          .kvpEscapeSpecialCharacters(x = obj@resultModel),
                                          sep = "="),
                        sep = "&")
  }

  if ( !is.na(obj@responseMode)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0] Adding response mode ",
                    obj@responseMode, "\n")
    .optionals <- paste(.optionals, paste(sosKVPParamNameResponseMode,
                                          .kvpEscapeSpecialCharacters(x = obj@responseMode),
                                          sep = "="),
                        sep = "&")
  }

  if ( !is.na(obj@BBOX)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0] Adding BBOX ",
                    obj@BBOX, "\n")
    .optionals <- paste(.optionals, paste(sosKVPParamNameBBOX,
                                          .kvpEscapeSpecialCharacters(x = obj@BBOX), sep = "="),
                        sep = "&")
  }

  if (verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0]",
                  "optional elements: ", .optionals, "\n")

  .kvpString <- paste(.mandatory, .optionals, sep = "")

  if (verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0]",
                  "Finished KVP string creation:\n", .kvpString, "\n")

  return(.kvpString)
}

.sosEncodeRequestKVPGetFeatureOfInterest_2.0.0 <- function(obj, sos,
                                                           verbose = FALSE) {
  if (verbose) cat("[.sosEncodeRequestKVPGetFeatureOfInterest_2.0.0] encoding",
                  toString(obj), "\n")

  # required:
  .requestBase <- .kvpBuildRequestBase(sos, sosGetFeatureOfInterestName)

  # optionals
  .optionals <- ""
  if (!is.na(obj@featureOfInterest)) {
    .optionals <- paste(sosKVPParamNameFoi,
                                .kvpEscapeSpecialCharacters(x = obj@featureOfInterest), sep = "=")
  }

  #TODO Implement procedure or a spatial filter
  if (is.character(.optionals)  && stringr::str_length(.optionals) > 0) {
    .kvpString <- paste(.requestBase, .optionals, sep = "&")
  } else {
    .kvpString <- .requestBase
  }
  if (verbose) cat("[.sosEncodeRequestKVPGetFeatureOfInterest_2.0.0]",
                  "with request: ", .kvpString, "\n")

  return(.kvpString)
}


#
#
#
SosGetObservation_2.0.0 <- function(
  service,
  version,
  offering,
  observedProperty,
  responseFormat,
  srsName = as.character(NA),
  eventTime = list(NA),
  procedure = as.character(NA),
  featureOfInterest = NULL,
  result = NULL,
  resultModel = as.character(NA),
  responseMode = as.character(NA),
  BBOX = as.character(NA),
  valueReferenceTemporalFilter = NA) {
  new("SosGetObservation_2.0.0",
      request = sosGetObservationName,
      service = service,
      version = version,
      offering = offering,
      observedProperty = observedProperty,
      responseFormat = responseFormat,
      srsName = srsName,
      eventTime = eventTime,
      procedure = procedure,
      featureOfInterest = featureOfInterest,
      result = result,
      resultModel = resultModel,
      responseMode = responseMode,
      BBOX = BBOX,
      valueReferenceTemporalFilter = valueReferenceTemporalFilter)
}


SosObservationOffering_2.0.0 <- function(id, name = as.character(NA),
                                         resultTime, phenomenonTime, procedure, observableProperty,
                                         featureOfInterestType, responseFormat,
                                         procedureDescriptionFormat, observationType, observedArea = list()) {
  new("SosObservationOffering_2.0.0", id = id, name = name,
      resultTime = resultTime, phenomenonTime = phenomenonTime, procedure = procedure,
      observableProperty = observableProperty,
      featureOfInterestType = featureOfInterestType,
      observationType = observationType,
      responseFormat = responseFormat,
      procedureDescriptionFormat = procedureDescriptionFormat,
      observedArea = observedArea)
}

#
#
#
SosGetFeatureOfInterest_2.0.0 <- function(
  service,
  version,
  featureOfInterest) {
  new("SosGetFeatureOfInterest_2.0.0",
      request = sosGetFeatureOfInterestName,
      service = service,
      version = version,
      featureOfInterest = featureOfInterest)
}
