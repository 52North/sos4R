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
# Created: 2013-03-06                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
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
                         responseMode, BBOX, latest, verbose, inspect, saveOriginal) {
            .offeringId <- offering@id
            if(verbose)	cat("[getObservation] Requesting offering", .offeringId,
                            "by SosObservationOffering.\n")
            return(.getObservation_2.0.0(sos = sos, offeringId = .offeringId,
                                         observedProperty = observedProperty,
                                         responseFormat = responseFormat,
                                         srsName = srsName, eventTime = eventTime,
                                         procedure = procedure,
                                         featureOfInterest = featureOfInterest,
                                         result = result, resultModel = resultModel,
                                         responseMode = responseMode, BBOX = BBOX,
                                         latest = latest, verbose = verbose,
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
                         resultModel, responseMode, BBOX, latest, verbose, inspect,
                         saveOriginal) {
            if(verbose)	cat("[getObservation] Requesting offering", offering,
                            "by name.\n")

            .off <- sosOfferings(sos)[[offering]]

            if(length(observedProperty) == 0) {
              .obsProps <- sosObservableProperties(.off)
              if(verbose) cat("[getObservation] Got observation(s) from offering because none given:",
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
                                         latest = latest, verbose = verbose,
                                         inspect = inspect, saveOriginal = saveOriginal))
          }
)

#
# getFeatureOfInterest ----
#
setMethod(f = "getFeatureOfInterest", signature = signature(sos = "SOS_2.0.0", featureOfInterest = "character"),
          def = function(sos, featureOfInterest, verbose, inspect, saveOriginal) {
            return(.getFeatureOfInterest_2.0.0(sos = sos, featureOfInterest = featureOfInterest, verbose = verbose, inspect = inspect, saveOriginal=saveOriginal))
          }
)

#
#
#
.getFeatureOfInterest_2.0.0 <- function(sos, featureOfInterest, verbose, inspect,
                                        saveOriginal,
                                        xmlParseOptions = c(XML::NOERROR, XML::RECOVER)){

  .filename <- NULL
  #
  #   if(!is.null(saveOriginal)) {
  #     if(is.character(saveOriginal)) {
  #       .filename <- paste(saveOriginal, ".xml", sep = "")
  #       if(verbose) cat("Using saveOriginal parameter for file name:",
  #                       .filename, "\n")
  #     }
  #     else if(is.logical(saveOriginal)) {
  #       if(saveOriginal) .filename <- paste(.cleanupFileName(featureOfInterest),
  #                                           ".xml", sep = "")
  #       if(verbose) cat("Generating file name:", .filename, "\n")
  #     }
  #   }

  if(verbose)
    cat("[.getFeatureOfInterest_2.0.0] to ", sos@url, " with featureOfInterest ",
        featureOfInterest, "\n")

  .gfoi <- .createFeatureOfInterest_2.0.0(sos, featureOfInterest)

  if(verbose)
    cat("[.getFeatureOfInterest_2.0.0] REQUEST:\n\n", toString(.gfoi), "\n")

  .responseString = sosRequest(sos = sos, request = .gfoi,
                               verbose = verbose, inspect = inspect)

  cat("[sos4R] Received response (size:", object.size(.responseString),
      "bytes), parsing ...\n")

  # responseFormat starts with text/xml OR the response string is XML content,
  # for example an exeption (which is xml even if request wants something else)
  .contentType <- NA_character_
  .contentType <- attributes(.responseString)[["Content-Type"]]

  if(verbose) cat("[.getFeatureOfInterest_2.0.0] Content-Type:", .contentType, "\n")

  if(nchar(.responseString) < 1) {
    warning(paste("Response string has length ", nchar(.responseString),
                  ". Please re-check query parameters."))
  }

  if(isXMLString(.responseString)) {
    if(verbose) {
      cat("[.getFeatureOfInterest_2.0.0] Got XML string as response",
          "(based on isXMLString()).\n")
      cat("[.getFeatureOfInterest_2.0.0] Content type: '", toString(.contentType), "'.\n")
    }

    .hasSubtype <- FALSE
    .contentSubtype <- NA
    if(length(.contentType) < 1) {
      if(verbose) cat("[.getFeatureOfInterest_2.0.0] No content type!",
                      "Falling back to '", mimeTypeXML, "'\n")
      .contentType <- mimeTypeXML
    }
    else if(length(.contentType) > 1) {
      # check if subtype is present or take just the first
      .subtypeIdx <- which(names(.contentType) == "subtype")
      if(length(.subtypeIdx) > 0 && .subtypeIdx > 0) {
        .hasSubtype <- TRUE
        .contentSubtype <- .contentType[[.subtypeIdx]]
        if(verbose) cat("[.getFeatureOfInterest_2.0.0] Found mime subtype: ",
                        toString(.contentSubtype), "'\n")
      }
      else if(verbose) cat(
        "[.getFeatureOfInterest_2.0.0] More than one content type, ",
        "no subtype detected : '",
        toString(.contentType),
        "'\n\tUsing the first one: '",
        .contentType[[1]], "'\n")
      .contentType <- .contentType[[1]]
    }

    .response <- xmlParseDoc(.responseString, asText = TRUE,
                             options = xmlParseOptions)
    if(verbose || inspect) {
      cat("[.getFeatureOfInterest_2.0.0] RESPONSE DOC:\n")
      print(.response)
    }
    # select the parser and file ending based on the mime type FIRST
    .fileEnding <- ".xml"
    if(.contentType == mimeTypeXML) {
      if(verbose)
        cat("[.getFeatureOfInterest_2.0.0] Got pure XML according to mime type.",
            "Trying to parse with default parser, see SosParsingFunctions().\n")
      .parserName <- mimeTypeXML

    }
    else {
      # fall back, or more of a default: the function name
      .parserName <- sosGetFeatureOfInterestName
    }

    if(!is.null(.filename)) {
      .filename <- paste(.filename, .fileEnding, sep = "")
      saveXML(.response, file = .filename)

      if(verbose) {
        cat("[.getFeatureOfInterest_2.0.0] Saved original document:",
            .filename, "\n")
      }
    }

    if(.isExceptionReport(.response)) {
      return(.handleExceptionReport(sos, .response))
    }

    .parsingFunction <- sosParsers(sos)[[.parserName]]

    if(verbose) {
      cat("[.getFeatureOfInterest_2.0.0] Parsing with function ")
      print(.parsingFunction)
    }

    .obs <- .parsingFunction(obj = .response, sos = sos,
                             verbose = verbose)

    .msg <- paste("[sos4R] Finished getFeatureOfInterest to", sos@url, "\n")

    if(!is.null(.filename)) {
      .msg <- paste(.msg,
                    "[sos4R] Original document saved:", .filename, "\n")

      .oldAttrs <- attributes(.obs)
      .newAttrs <- list(.filename)
      names(.newAttrs) <- list(sosAttributeFileName)
      if(verbose) cat("[.getObservationById_1.0.0] Appending new attributes",
                      toString(.newAttrs), "(names",
                      toString(names(.newAttrs)), ")\n")

      attributes(.obs) <- c(.oldAttrs, .newAttrs)
    }
    cat(.msg)

    # RETURN ###
    return(.obs)
  }
}

.createFeatureOfInterest_2.0.0 <- function(sos, featureOfInterest){
  SosGetFeatureOfInterest_2.0.0("SOS", "2.0.0", featureOfInterest)
}


.isEmptyResponse <- function(response = "") {
  gsub(pattern = "\t|\r|\n", x = response, replacement = "") == sos200_emptyGetObservationResponseString
}

#
# getObservation function ----
#
# TODO: Remove/handle obsolete parameters: resultModel, responseMode, eventTime -> temporalFilter
#
.getObservation_2.0.0 <- function(sos, offeringId, observedProperty,
                                  responseFormat, srsName, eventTime,	procedure, featureOfInterest,
                                  result, resultModel, responseMode, BBOX, latest, verbose, inspect,
                                  saveOriginal, xmlParseOptions = c(XML::NOERROR, XML::RECOVER)) {

  .filename <- NULL
  if(!is.null(saveOriginal)) {
    if(is.character(saveOriginal)) {
      .filename <- saveOriginal
      if(verbose) cat("[.getObservation_2.0.0] Using saveOriginal parameter for file name:",
                      .filename, "\n")
    }
    else if(is.logical(saveOriginal) && saveOriginal) {
      .filename <- paste(.cleanupFileName(offeringId),
                         format(Sys.time(), sosDefaultFilenameTimeFormat),
                         sep = "_")
      if(verbose) cat("[.getObservation_2.0.0] Generating file name:",
                      .filename, "\n")
    }
  }

  if(verbose)
    cat("[.getObservation_2.0.0] to ", sos@url, " with offering ",
        offeringId, "\n")

  .go <- .createGetObservation_2.0.0(sos, offeringId, observedProperty,
                                     responseFormat, srsName, eventTime,	procedure, featureOfInterest,
                                     result, resultModel, responseMode, BBOX, latest, verbose, inspect,
                                     saveOriginal)

  if(verbose)
    cat("[.getObservation_2.0.0] REQUEST:\n\n", toString(.go), "\n")

  .responseString = sosRequest(sos = sos, request = .go,
                               verbose = verbose, inspect = inspect)

  cat("[.getObservation_2.0.0] Received response (size:", object.size(.responseString),
      "bytes), parsing ...\n")

  # responseFormat starts with text/xml OR the response string is XML content,
  # for example an exception (which is xml even if request wants something else
  .contentType <- NA_character_
  .contentType <- attributes(.responseString)[["Content-Type"]]

  if(verbose) cat("[.getObservation_2.0.0] Content-Type:", .contentType, "\n")

  if(nchar(.responseString) < 1) {
    warning(paste("Response string has length ", nchar(.responseString),
                  ". Please re-check query parameters."))
  }

  if(isXMLString(.responseString)) {
    if(verbose) {
      cat("[.getObservation_2.0.0] Got XML string as response",
          "(based on isXMLString()).\n")
      cat("[.getObservation_2.0.0] Content type: '", toString(.contentType), "'.\n")
    }

    .hasSubtype <- FALSE
    .contentSubtype <- NA
    if(length(.contentType) < 1) {
      if(verbose) cat("[.getObservation_2.0.0] No content type!",
                      "Falling back to '", mimeTypeXML, "'\n")
      .contentType <- mimeTypeXML
    }
    else if(length(.contentType) > 1) {
      # check if subtype is present or take just the first
      .subtypeIdx <- which(names(.contentType) == "subtype")
      if(length(.subtypeIdx) > 0 && .subtypeIdx > 0) {
        .hasSubtype <- TRUE
        .contentSubtype <- .contentType[[.subtypeIdx]]
        if(verbose) cat("[.getObservation_2.0.0] Found mime subtype: ",
                        toString(.contentSubtype), "'\n")
      }
      else if(verbose) cat(
        "[.getObservation_2.0.0] More than one content type, ",
        "no subtype detected : '",
        toString(.contentType),
        "'\n\tUsing the first one: '",
        .contentType[[1]], "'\n")
      .contentType <- .contentType[[1]]
    }

    .response <- xmlParseDoc(.responseString, asText = TRUE,
                             options = xmlParseOptions)
    if(verbose || inspect) {
      cat("[.getObservation_2.0.0] RESPONSE DOC:\n")
      print(.response)
    }
    # select the parser and file ending based on the mime type FIRST
    .fileEnding <- ".xml"
    if(.contentType == mimeTypeXML) {
      if(.hasSubtype && .contentSubtype == mimeSubtypeOM) {
        if(verbose)
          cat("[.getObservation_2.0.0] Got OM according to mime type.\n")
        .parserName <- mimeTypeOM
      }
      else {
        if(verbose)
          cat("[.getObservation_2.0.0] Got pure XML according to mime type.",
              "Trying to parse with default parser, see SosParsingFunctions().\n")
        .parserName <- mimeTypeXML
      }
    }
    else if (.contentType == mimeTypeKML) {
      if(verbose) cat("[.getObservation_2.0.0] Got KML according to mime type.\n")

      .fileEnding <- ".kml"
      .parserName <- mimeTypeKML
    }
    else {
      # fall back, or more of a default: the function name
      .parserName <- sosGetObservationName
    }

    if(!is.null(.filename)) {
      .filename <- paste(.filename, .fileEnding, sep = "")
      saveXML(.response, file = .filename)

      if(verbose) {
        cat("[.getObservation_2.0.0] Saved original document:",
            .filename, "\n")
      }
    }

    if(.isExceptionReport(.response)) {
      return(.handleExceptionReport(sos, .response))
    }

    # TODO is this still required?
    if( !is.na(responseFormat) &&
        isTRUE(grep(pattern = "text/xml", x = responseFormat) != 1)) {
      warning("Got XML string, but request did not require text/xml (or subtype).")
    }

    .parsingFunction <- sosParsers(sos)[[.parserName]]

    if(verbose) {
      cat("[.getObservation_2.0.0] Parsing with function ")
      print(.parsingFunction)
    }

    .obs <- .parsingFunction(obj = .response, sos = sos,
                             verbose = verbose)

    # calculate result length vector
    if(inherits(.obs, "OmObservationCollection")) {
      if(verbose) cat("[.getObservation_2.0.0] Got OmObservationCollection",
                      "... calculating length with sosResult()")

      .result <- sosResult(.obs, bind = FALSE, coordinates = FALSE)
      if(verbose) cat("[.getObservation_2.0.0] result: ", toString(.result))

      .resultLength <- sapply(.result, nrow)
      if(length(.resultLength) == 0){
        # nothing
        .resultLength <- 0
      }
    }
    else if (is.list(.obs) && all(sapply(.obs, function(o) { class(o) == "OmOM_Observation"}))) {
      .resultLength <- sum(sapply(.obs, function(o){ !is.null(o@result)}))
    }
    else .resultLength <- NA

    if(verbose) {
      cat("[.getObservation_2.0.0] PARSED RESPONSE:",
          class(.obs), "\n")
      cat("[.getObservation_2.0.0] Result length(s): ",
          toString(.resultLength), "\n")
    }

    if(is.list(.obs) && any(sapply(.obs, is.null))) {
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
    if(!is.null(.filename)) {
      .msg <- paste(.msg,
                    "[sos4R] Original document saved:", .filename, "\n")

      .oldAttrs <- attributes(.obs)
      .newAttrs <- list(.filename)
      names(.newAttrs) <- list(sosAttributeFileName)
      if(verbose) cat("[.getObservation_2.0.0] Appending new attributes",
                      toString(.newAttrs), "(names",
                      toString(names(.newAttrs)), ")\n")

      attributes(.obs) <- c(.oldAttrs, .newAttrs)
    }
    cat(.msg)

    # RETURN ###
    return(.obs)
  }
  else if (.isEmptyResponse(.responseString)) {
    if(verbose)
      cat("[.getObservation_2.0.0] Received empty observation response.", "\n")
    return(list())
  }
  else { # response is NOT an XML string:
    if(verbose)
      cat("[.getObservation_2.0.0] Did NOT get XML string as response, trying to parse with",
          responseFormat, "\n")

    if(is.na(responseFormat) || is.null(responseFormat)) {
      if(verbose)
        cat("[.getObservation_2.0.0] responseFormat is ",
            responseFormat, " >>> returning response string...\n")
      return(.responseString)
    }

    if(mimeTypeCSV == responseFormat) {
      if(verbose || inspect) {
        cat("[.getObservation_2.0.0] CSV RESPONSE:\n")
        print(.responseString)
      }

      .parsingFunction <- sosParsers(sos)[[mimeTypeCSV]]
      .csv <- .parsingFunction(obj = .responseString, verbose = verbose)

      if(!is.null(.filename)) {
        .filename <- paste(file = .filename, ".csv", sep = "")
        write.csv(.csv, .filename)
      }

      .msg <- paste("[sos4R] Finished getObservation to", sos@url, "\n\t",
                    "--> received observations with dimensions",
                    toString(dim(.csv)), "\n")
      if(!is.null(.filename)) {
        .msg <- paste(.msg,
                      "[sos4R] Original document saved:", .filename, "\n")

        .oldAttrs <- attributes(.csv)
        .newAttrs <- list(.filename)
        names(.newAttrs) <- list(sosAttributeFileName)
        if(verbose) cat("[.getObservation_2.0.0] Appending new attributes",
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
  if(verbose || inspect) {
    cat("[.getObservation_2.0.0] UNKNOWN RESPONSE FORMAT; Response string: \n'")
    cat(.responseString, "'\n")
    cat("[.getObservation_2.0.0] Content-Type: ", .contentType)
    warning("Unknown response format!")
  }

  if(!is.null(.filename)) {
    save(.responseString, file = .filename)
    cat("[sos4R] Saved original document:", .filename)
  }
  else {
    message("File name is NULL, could not save document!")
  }

  # RETURN ##############
  return(.responseString)
}

#
#
#
.createGetObservation_2.0.0 <- function(sos, offeringId, observedProperty,
                                        responseFormat, srsName, eventTime,	procedure, featureOfInterest,
                                        result, resultModel, responseMode, BBOX, latest, verbose, inspect,
                                        saveOriginal) {

  if(latest) .eventTime <- list(.createLatestEventTime(verbose))
  else .eventTime <- eventTime

  if(latest && !is.na(eventTime))
    warning("'Latest' is set to TRUE > given eventTime is ignored!")

  .go <- SosGetObservation_2.0.0(service = sosService, version = sos@version,
                                 offering = offeringId, observedProperty = observedProperty,
                                 responseFormat =  responseFormat, srsName = srsName,
                                 eventTime = .eventTime, procedure = procedure,
                                 featureOfInterest = featureOfInterest, result = result,
                                 resultModel = resultModel, responseMode = responseMode,
                                 BBOX = BBOX)

  if(verbose)
    cat("[.createGetObservation_2.0.0] Done:\n", toString(.go), "\n")

  return(.go)
}
#
# checkRequest - GetObservation ----
#
setMethod(f = "checkRequest",
          signature = signature(service = "SOS_2.0.0", operation = "SosGetObservation_2.0.0",
                                verbose = "logical"),
          def = function(service, operation, verbose) {
            # check if operation is for SOS and operation is GetObservation
            if(!(operation@service == sosService &&
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
          signature = signature(service = "SOS_2.0.0", operation = "SosGetFeatureOfInterest_2.0.0",
                                verbose = "logical"),
          def = function(service, operation, verbose) {
            # check if operation is for SOS and operation is DescribeSensor
            if(!(operation@service == sosService &&
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
            if(obj@version == sos200_version) {
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
            if(obj@version == sos200_version) {
              return(.sosEncodeRequestKVPGetFeatureOfInterest_2.0.0(obj, sos,
                                                                    verbose))
            }
            else {
              stop("Version not supported!")
            }
          }
)

.sosEncodeRequestKVPGetObservation_2.0.0 <- function(obj, sos, verbose = FALSE) {
  if(verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0] encoding", toString(obj), "\n")

  # required:
  .requestBase <- .kvpBuildRequestBase(sos, sosGetObservationName)
  .offering <- paste(sosKVPParamNameOffering,
                     .kvpEscapeSpecialCharacters(x = obj@offering), sep = "=")
  .observedProperty <- .kvpKeyAndValues(sosKVPParamNameObsProp,
                                        obj@observedProperty)

  .mandatory <- paste(.service, .request, .version, .offering,
                      .observedProperty, sep = "&")

  if(verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0]",
                  "mandatory elements: ", .mandatory, "\n")

  # optional:
  .optionals = ""
  # is optional for GET
  if(!is.na(obj@srsName)) {
    if(verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0] Adding SRS name ",
                    obj@srsName, "\n")
    .optionals <- paste(.optionals, paste(sosKVPParamNameSrsName,
                                          .kvpEscapeSpecialCharacters(x = obj@srsName),
                                          sep = "="),
                        sep = "&")
  }

  if(!is.na(obj@eventTime)) {
    if(verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0] Adding event time",
                    toString(obj@eventTime), "\n")
    if(length(obj@eventTime) > 1)
      warning("Only first event time in the list is used for KVP!")

    .timeString <- obj@eventTime[[1]]

    # if the eventTime is a latest request, it returns NA, the GET binding
    # says for the latest observation eventTime is omitted
    if(isS4(.timeString) || !is.na(.timeString)) {
      #  Add namespace and filter reference for SOS v2.0 and O&M v2.0
      .optionals <- paste(.optionals,
                          paste0("temporalFilter=",
                                 .kvpEscapeSpecialCharacters(
                                   paste0(
                                     # TODO make value reference configurable (use this as default value)
                                     "om:phenomenonTime,",
                                     encodeKVP(sos = sos, obj = .timeString, verbose = verbose)
                                   )
                                 )
                          ),
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
      if(verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0] ",
                      "encodeKVP returned NA for eventTime, omitting",
                      "parameter for request for latest observation.")
    }
  }

  if(!any(sapply(obj@procedure, "is.na"), na.rm = TRUE)) {
    if(verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0] Adding procedures ",
                    obj@procedure, "\n")
    .optionals <- paste(.optionals,
                        .kvpKeyAndValues(sosKVPParamNameProcedure, obj@procedure),
                        sep = "&")
  }

  if(!is.null(obj@featureOfInterest)) {
    .foiIDs <- obj@featureOfInterest@objectIDs

    if(verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0] Adding features ",
                    toString(obj@featureOfInterest), "by IDs ", toString(.foiIDs), "\n")

    .optionals <- paste(.optionals,
                        .kvpKeyAndValues(sosKVPParamNameFoi, .foiIDs),
                        sep = "&")
  }

  if(!is.na(obj@BBOX)) {
    if(verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0] Adding BBOX ",
                    obj@BBOX, "\n")
    .optionals <- paste(.optionals, paste(sosKVPParamNameBBOX,
                                          .kvpEscapeSpecialCharacters(x = obj@BBOX), sep = "="),
                        sep = "&")
  }

  if(verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0]",
                  "optional elements: ", .optionals, "\n")

  .kvpString <- paste(.mandatory, .optionals, sep = "")

  if(verbose) cat("[.sosEncodeRequestKVPGetObservation_2.0.0]",
                  "Finished KVP string creation:\n", .kvpString, "\n")

  return(.kvpString)
}

.sosEncodeRequestKVPGetFeatureOfInterest_2.0.0 <- function(obj, sos,
                                                           verbose = FALSE) {
  if(verbose) cat("[.sosEncodeRequestKVPGetFeatureOfInterest_2.0.0] encoding",
                  toString(obj), "\n")

  # required:
  .request <- paste(sosKVPParamNameRequest, sosGetFeatureOfInterestName, sep = "=")
  .service <- paste(sosKVPParamNameService,
                    .kvpEscapeSpecialCharacters(x = obj@service), sep = "=")
  .version <- paste(sosKVPParamNameVersion,
                    .kvpEscapeSpecialCharacters(x = obj@version), sep = "=")
  .featureOfInterest <- paste(sosKVPParamNameFoi,
                              .kvpEscapeSpecialCharacters(x = obj@featureOfInterest), sep = "=")

  #TODO featureOfInterest is not really mandatory, could also be procedure or a spatial filter
  .kvpString <- paste(.service, .request, .version, .featureOfInterest, sep = "&")

  if(verbose) cat("[.sosEncodeRequestKVPGetFeatureOfInterest_2.0.0]",
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
  BBOX = as.character(NA)) {
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
      BBOX = BBOX)
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
