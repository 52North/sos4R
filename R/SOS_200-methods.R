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
# Created: 2013-03-06                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

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
          definition = function(sos,
                                procedure,
                                outputFormat,
                                validTime,
                                verbose,
                                inspect,
                                saveOriginal) {
            return(.describeSensor_2.0.0(sos = sos,
                                         procedure = procedure,
                                         procedureDescriptionFormat = outputFormat,
                                         validTime = validTime,
                                         verbose = verbose,
                                         inspect = inspect,
                                         saveOriginal = saveOriginal))
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
# getObservation: no parameters ----
#
setMethod(f = "getObservation",
          signature = signature(sos = "SOS_2.0.0"),
          definition = function(sos,
                                offering,
                                observedProperty,
                                responseFormat,
                                eventTime,
                                procedure,
                                featureOfInterest,
                                BBOX,
                                verbose,
                                inspect,
                                saveOriginal,
                                retrieveFOI = TRUE) {
            if (verbose) cat("[getObservation] Requesting observations without offering\n")
            return(.getObservation_2.0.0(sos = sos,
                                         offerings = offering,
                                         observedProperty = observedProperty,
                                         responseFormat = responseFormat,
                                         eventTime = eventTime,
                                         procedure = procedure,
                                         featureOfInterest = featureOfInterest,
                                         BBOX = BBOX,
                                         verbose = verbose,
                                         inspect = inspect,
                                         saveOriginal = saveOriginal,
                                         retrieveFOI = retrieveFOI))
          }
)

#
# getObservation: offering object ----
#
setMethod(f = "getObservation",
          signature = signature(sos = "SOS_2.0.0",
                                offering = "SosObservationOffering_2.0.0"),
          definition = function(sos,
                                offering,
                                observedProperty,
                                responseFormat,
                                eventTime,
                                procedure = list(),
                                featureOfInterest,
                                BBOX,
                                verbose,
                                inspect,
                                saveOriginal,
                                retrieveFOI = TRUE) {
            if (verbose) cat("[getObservation] Requesting offering", offering@id, "with object\n")
            return(.getObservation_2.0.0(sos = sos,
                                         offerings = list(offering),
                                         observedProperty = observedProperty,
                                         responseFormat = responseFormat,
                                         eventTime = eventTime,
                                         procedure = procedure,
                                         featureOfInterest = featureOfInterest,
                                         BBOX = BBOX,
                                         verbose = verbose,
                                         inspect = inspect,
                                         saveOriginal = saveOriginal,
                                         retrieveFOI = retrieveFOI))
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
                                eventTime,
                                procedure = list(),
                                featureOfInterest,
                                BBOX,
                                verbose,
                                inspect,
                                saveOriginal,
                                retrieveFOI = TRUE) {
            if (verbose)	cat("[getObservation] Requesting offering", offering, "\n")

            return(.getObservation_2.0.0(sos = sos,
                                         offerings = list(offering),
                                         observedProperty = observedProperty,
                                         responseFormat = responseFormat,
                                         eventTime = eventTime,
                                         procedure = procedure,
                                         featureOfInterest = featureOfInterest,
                                         BBOX = BBOX,
                                         verbose = verbose,
                                         inspect = inspect,
                                         saveOriginal = saveOriginal,
                                         retrieveFOI = retrieveFOI))
          }
)

#
# getObservation: offering list ----
#
setMethod(f = "getObservation",
          signature = signature(sos = "SOS_2.0.0",
                                offering = "list"),
          definition = function(sos,
                                offering,
                                observedProperty,
                                responseFormat,
                                eventTime,
                                procedure,
                                featureOfInterest,
                                BBOX,
                                verbose,
                                inspect,
                                saveOriginal,
                                retrieveFOI = TRUE) {
            if (verbose)	cat("[getObservation] Requesting list of offerings:", toString(offering), "\n")

            return(.getObservation_2.0.0(sos = sos,
                                         offerings = offering,
                                         observedProperty = observedProperty,
                                         responseFormat = responseFormat,
                                         eventTime = eventTime,
                                         procedure = procedure,
                                         featureOfInterest = featureOfInterest,
                                         BBOX = BBOX,
                                         verbose = verbose,
                                         inspect = inspect,
                                         saveOriginal = saveOriginal,
                                         retrieveFOI = retrieveFOI))
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
# encodeRequest - KVP - GetObservation ----
#
setMethod("encodeRequestKVP", "SosGetObservation_2.0.0",
          function(obj, sos, verbose = FALSE) {
            if (obj@version == sos200_version) {
              return(.sosEncodeRequestKVPGetObservation_2.0.0(obj, sos, verbose))
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
              requestBase <- .kvpBuildRequestBase(sos, sosGetFeatureOfInterestName)

              # optionals
              optionals <- ""
              if (!all(is.na(obj@featureOfInterest))) {
                escapedIds <-  sapply(X = obj@featureOfInterest, FUN = .kvpEscapeSpecialCharacters)
                optionals <- paste(sosKVPParamNameFoi, paste(escapedIds, collapse = .kvpEscapeSpecialCharacters(",")), sep = "=")
              }

              #TODO Implement procedure or a spatial filter
              if (is.character(optionals)  && stringr::str_length(optionals) > 0) {
                kvpString <- paste(requestBase, optionals, sep = "&")
              } else {
                kvpString <- requestBase
              }
              if (verbose) cat("[.sosEncodeRequestKVPGetFeatureOfInterest_2.0.0]",
                               "with request: ", kvpString, "\n")

              return(kvpString)
            }
            else {
              stop("Version not supported!")
            }
          }
)

#
# encodeRequest - XML - GetObservation ----
#
setMethod("encodeRequestXML", "SosGetObservation_2.0.0",
          function(obj, sos, verbose = FALSE) {
            if (obj@version == sos200_version) {
              return(.sosEncodeRequestXMLGetObservation_2.0.0(obj, sos,
                                                              verbose))
            }
            else {
              stop("Version not supported!")
            }
          }
)

#
# encodeRequest - XML - GetFeatureOfInterest ----
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

#
# sosFeaturesOfInterest("SOS_2.0.0") ----
#
setMethod(f = "sosFeaturesOfInterest", signature = signature(obj = "SOS_2.0.0"),
          definition = function(obj, offerings = sosOfferingIds(obj)) {
            return(sosOperations(obj)$GetFeatureOfInterest@parameters$featureOfInterest)
          }
)

#
# construction functions ----
#
SosGetObservation_2.0.0 <- function(
  service,
  version,
  offering = list(),
  observedProperty = list(),
  responseFormat = as.character(NA),
  temporalFilter = list(),
  procedure = list(),
  featureOfInterest = list(),
  spatialFilter = NULL,
  valueReferenceTemporalFilter = as.character(NA)) {

  offeringIDs <- lapply(X = offering, FUN = function(o) {
    if (is.character(o))
      return(o)
    else if (inherits(o, "SosObservationOffering_2.0.0"))
      return(o@id)
    else {
      stop("Unsupported object in offering list (only character or SosObservationOffering_2.0.0): ", toString(o))
    }
  })

  featureIDs <- lapply(X = featureOfInterest, FUN = function(o) {
    if (is.character(o))
      return(o)
    else if (inherits(o, "GmlFeatureProperty"))
      return(sosFeatureIds(o))
    else {
      stop("Unsupported object in offering list (only character or SosObservationOffering_2.0.0): ", toString(o))
    }
  })

  new("SosGetObservation_2.0.0",
      request = sosGetObservationName,
      service = service,
      version = version,
      offering = offeringIDs,
      observedProperty = observedProperty,
      responseFormat = responseFormat,
      temporalFilter = temporalFilter,
      procedure = procedure,
      featureOfInterest = featureIDs,
      spatialFilter = spatialFilter,
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
