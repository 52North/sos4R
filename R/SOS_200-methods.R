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
          definition = function(sos, request, verbose, inspect) {
            .sosRequest_2.0.0(sos = sos, request = request, verbose = verbose,
                              inspect = inspect)
          }
)

#
# getCapabilities ----
#
setMethod(f = "getCapabilities", signature = signature(sos = "SOS_2.0.0"),
          definition = function(sos, verbose, inspect, sections, acceptFormats,
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
setMethod(f = "getObservationById",
          signature = signature(sos = "SOS_2.0.0", observationId = "character"),
          definition = function(sos,
                                observationId,
                                responseFormat,
                                srsName,
                                resultModel,
                                responseMode,
                                verbose,
                                inspect,
                                saveOriginal) {
            if (verbose) cat("[getObservationById 2.0.0] ID", observationId, "\n")

            filename <- NULL
            if (!is.null(saveOriginal)) {
              if (is.character(saveOriginal)) {
                filename <- saveOriginal
              }
              else if (is.logical(saveOriginal) && saveOriginal) {
                filename <- paste(observationId,
                                  format(Sys.time(), sosDefaultFilenameTimeFormat),
                                  ".xml",
                                  sep = "_")

              }
              if (verbose) cat("[getObservationById 2.0.0] Saving original to file", filename, "\n")
            }

            go <- SosGetObservationById(service = sosService,
                                        version = sos@version,
                                        observationId = observationId,
                                        responseFormat =  responseFormat,
                                        srsName = srsName,
                                        resultModel = resultModel,
                                        responseMode = responseMode)

            response = sosRequest(sos = sos,
                                  request = go,
                                  verbose = verbose,
                                  inspect = inspect)

            if (!is.null(filename)) {
              xml2::write_xml(x = response, file = filename)
              cat("[sos4R] Original document saved:", filename, "\n")
            }

            if (.isExceptionReport(response)) {
              return(.handleExceptionReport(sos, response))
            }
            else {
              parsingFunction <- sosParsers(sos)[[sosGetObservationByIdResponseName]]
              obs <- parsingFunction(obj = response,
                                     sos = sos,
                                     verbose = verbose)

              # remove list if only one element
              if (is.list(obs) && length(obs) == 1)
                obs <- obs[[1]]

              if (verbose) {
                cat("[getObservationById 2.0.0] PARSED RESPONSE:\n")
                print(obs)
              }

              return(obs)
            }
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
            return(.getObservation_2.0.0(sos = sos,
                                         offeringId = .offeringId,
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
          definition = function(sos,
                                offering,
                                observedProperty = list(),
                                responseFormat,
                                srsName,
                                eventTime,
                                procedure,
                                featureOfInterest,
                                result,
                                resultModel,
                                responseMode,
                                BBOX,
                                verbose,
                                inspect,
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
                                         srsName = srsName,
                                         eventTime = eventTime,
                                         procedure = procedure,
                                         featureOfInterest = featureOfInterest,
                                         result = result,
                                         resultModel = resultModel,
                                         responseMode = responseMode,
                                         BBOX = BBOX,
                                         verbose = verbose,
                                         inspect = inspect,
                                         saveOriginal = saveOriginal))
          }
)

#
# getFeatureOfInterest ----
#
setMethod(f = "getFeatureOfInterest", signature = signature(sos = "SOS_2.0.0"),
          definition = function(sos, featureOfInterest, verbose, inspect, saveOriginal) {
            return(.getFeatureOfInterest_2.0.0(sos = sos,
                                               featureOfInterest = NA_character_,
                                               verbose = verbose, inspect = inspect, saveOriginal = saveOriginal))
          }
)

#
# getFeatureOfInterest - without filter ----
#
setMethod(f = "getFeatureOfInterest", signature = signature(sos = "SOS_2.0.0", featureOfInterest = "character"),
          definition = function(sos, featureOfInterest, verbose, inspect, saveOriginal) {
            return(.getFeatureOfInterest_2.0.0(sos = sos,
                                               featureOfInterest = featureOfInterest,
                                               verbose = verbose, inspect = inspect, saveOriginal = saveOriginal))
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

  if (verbose) cat("[sos4R] Received response (size:", utils::object.size(.response), "bytes), parsing ...\n")

  if (!is.null(.filename)) {
    xml2::write_xml(x = .response, file = .filename)
    cat("[sos4R] Original document saved:", .filename, "\n")
  }

  if (.isExceptionReport(.response)) {
    return(.handleExceptionReport(sos, .response))
  }

  parsingFunction <- sosParsers(sos)[[sosGetFeatureOfInterestResponseName]]

  if (verbose) {
    cat("[.getFeatureOfInterest_2.0.0] Parsing with function ")
    print(parsingFunction)
  }

  obs <- parsingFunction(obj = .response,
                         sos = sos,
                         verbose = verbose)

  cat("[sos4R] Finished getFeatureOfInterest to", sos@url, "\n")

  return(obs)
}

#
# getObservation function ----
#
# TODO: Remove/handle obsolete parameters: resultModel, responseMode, eventTime -> temporalFilter
#
.getObservation_2.0.0 <- function(sos, offeringId, observedProperty,
                                  responseFormat,
                                  srsName,
                                  eventTime,
                                  procedure,
                                  featureOfInterest,
                                  result,
                                  resultModel,
                                  responseMode,
                                  BBOX,
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

  go <- SosGetObservation_2.0.0(service = sosService,
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

  response = sosRequest(sos = sos,
                         request = go,
                         verbose = verbose,
                         inspect = inspect)

  if (!is.null(.filename)) {
    xml2::write_xml(x = response, file = .filename)
    if (verbose) cat("[.getObservation_2.0.0] Saved original document:", .filename, "\n")
  }

  if (.isExceptionReport(response)) {
    return(.handleExceptionReport(sos, response))
  }

  if (inherits(response, "xml_document")) {
    if (verbose) cat("[.getObservation_2.0.0] Got XML document as response.\n")
    if ( !is.na(responseFormat) &&
        isTRUE(grep(pattern = "text/xml", x = responseFormat) != 1)) {
      warning("Got XML string, but request did not require text/xml (or subtype).")
    }

    parsingFunction <- sosParsers(sos)[[sosGetObservationName]]

    if (verbose) {
      cat("[.getObservation_2.0.0] Parsing with function ")
      print(parsingFunction)
    }

    obs <- parsingFunction(obj = response,
                           sos = sos,
                           verbose = verbose)

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
    else if (is.list(obs) && all(sapply(obs, function(o) { class(o) == "OmOM_Observation"}))) {
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

    cat("[sos4R] Finished getObservation to", sos@url,
        "\n\t--> received", length(obs), "observation(s) having", .countInfo , "\n")
    if (!is.null(.filename)) cat("[sos4R] Original document saved:", .filename, "\n")

    return(obs)
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
        utils::write.csv(.csv, .filename)
      }

      cat("[sos4R] Finished getObservation to", sos@url, "\n\t",
          "--> received observations with dimensions", toString(dim(.csv)), "\n")
      if (!is.null(.filename)) cat("[sos4R] Original document saved:", .filename, "\n")

      return(.csv)
    }
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
              if (verbose) cat("[.sosEncodeRequestKVPGetFeatureOfInterest_2.0.0] encoding",
                               toString(obj), "\n")

              # required:
              .requestBase <- .kvpBuildRequestBase(sos, sosGetFeatureOfInterestName)

              # optionals
              .optionals <- ""
              if (!all(is.na(obj@featureOfInterest))) {
                escapedIds <-  sapply(X = obj@featureOfInterest, FUN = .kvpEscapeSpecialCharacters)
                .optionals <- paste(sosKVPParamNameFoi, paste(escapedIds, collapse = .kvpEscapeSpecialCharacters(",")), sep = "=")
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
            else {
              stop("Version not supported!")
            }
          }
)

#
# encodeRequest - XML - GetFeatureOfInterest ----
#
#
setMethod("encodeRequestXML", "SosGetFeatureOfInterest_2.0.0",
          function(obj, sos, verbose = FALSE) {
            if (obj@version == sos200_version) {
              if (verbose) cat("[encodeRequestXML GetFeatureOfInterest] encoding",
                               toString(obj), "\n")
              xmlDoc <- xml2::xml_new_root(sosGetFeatureOfInterestName)
              xml2::xml_set_attrs(x = xmlDoc,
                                  value = c(xmlns = sos200Namespace,
                                            service = obj@service,
                                            version = obj@version,
                                            "xmlns:xsi" = xsiNamespace,
                                            "xmlns:sos20" = sos200Namespace))

              for (i in 1:length(obj@featureOfInterest)) {
                xml2::xml_add_child(xmlDoc, sos200FeatureOfInterestName, obj@featureOfInterest[[i]])
              }

              return(xmlDoc)
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
  valueReferenceTemporalFilter = as.character(NA)) {
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
