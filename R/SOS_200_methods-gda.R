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
# visit the Free Software Foundation web page, http://www.fsf.org              #
#                                                                              #
# Author: Eike Hinderk Jürrens (e.h.juerrens@52north.org)                      #
# Created: 2018-11-23                                                          #
# Project: sos4R - visit project web page https://52north.org/geostatistics    #
#                                                                              #
############################################################################## #

#
# helper functions ----
#
.getDataAvailability_1.0.0 <- function(sos,
                                       procedures,
                                       observedProperties,
                                       featuresOfInterest,
                                       offerings,
                                       verbose,
                                       inspect,
                                       saveOriginal,
                                       ...) {
  filename <- NULL
  if (!is.null(saveOriginal)) {
    if (is.character(saveOriginal)) {
      filename <- saveOriginal
      if (verbose) cat("[.getDataAvailability_1.0.0] Using saveOriginal parameter for file name: '", filename, "'\n", sep = "")
    }
    else if (is.logical(saveOriginal)) {
      if (saveOriginal) {
        filename <- tempfile(pattern = format(Sys.time(), sosDefaultFilenameTimeFormat), fileext = ".xml")
      }
      if (verbose) cat("[.getDataAvailability_1.0.0] Generated file name:", filename, "\n")
    }
  }
  gda <- SosGetDataAvailability_1.0.0(service = sosService,
                                      version = sos@version,
                                      observedProperties = observedProperties,
                                      procedures = procedures,
                                      featuresOfInterest = featuresOfInterest,
                                      offerings = offerings)

  if (verbose) cat("[.getDataAvailability_1.0.0] REQUEST:\n", toString(gda), "\n")

  response <- sosRequest(sos = sos,
                         request = gda,
                         verbose = verbose,
                         inspect = inspect)

  if (verbose) cat("[.getDataAvailability_1.0.0] Received response (size:", utils::object.size(response),
                   "bytes), parsing ...\n")

  if (inspect) {
    cat("[.getDataAvailability_1.0.0] Response XML document:\n")
    print(response)
  }

  if (!is.null(filename)) {
    xml2::write_xml(x = response, file = filename)
    if (verbose) cat("[.getDataAvailability_1.0.0] Saved original document:", filename, "\n")
  }

  if (.isExceptionReport(response)) {
    return(.handleExceptionReport(sos, response))
  }

  parsingFunction <- sosParsers(sos)[[gdaGetDataAvailabilityResponseName]]

  if (verbose) {
    cat("[.getDataAvailability_1.0.0] Parsing with function ")
    print(parsingFunction)
  }

  dataAvailability <- parsingFunction(obj = response, sos = sos, verbose = verbose)
  if (verbose) cat("[.getDataAvailability_1.0.0] Received and parsed", length(dataAvailability),
                    "data availability members.\n")
  return (dataAvailability)
}


#
# getDataAvailability - Impl. ----
#
# Implementation of abstract method from Generic-Methods.R
#
setMethod(f = "getDataAvailability",
          signature = signature(sos = "SOS_2.0.0"),
          definition = function(sos,
                         procedures,
                         observedProperties,
                         featuresOfInterest,
                         offerings,
                         verbose,
                         inspect = FALSE,
                         saveOriginal = NULL,
                         ...) {
            if (verbose) {
              cat("[getDataAvailability] Requesting metadata via procedures: '",
                  paste0(procedures, collapse = ", "),
                  "'; observedProperties: '",
                  paste0(observedProperties, collapse = ", "),
                  "'; featuresOfInterest: '",
                  paste0(featuresOfInterest, collapse = ", "),
                  "'; offerings: '",
                  paste0(offerings, collapse = ", "),
                  "'\n")
            }
            return(.getDataAvailability_1.0.0(sos = sos,
                                              procedures = procedures,
                                              observedProperties = observedProperties,
                                              featuresOfInterest = featuresOfInterest,
                                              offerings = offerings,
                                              verbose = verbose,
                                              inspect = inspect,
                                              saveOriginal = saveOriginal,
                                              ...))
          }
)

#
# encodeRequest - KVP - GetDataAvailability ----
#
setMethod("encodeRequestKVP", "SosGetDataAvailability_1.0.0",
          function(obj, sos, verbose = FALSE) {
            if (obj@version == sos200_version) {
              return(.sosEncodeRequestKVPGetDataAvailability_1.0.0(obj, sos, verbose))
            }
            else {
              stop("Version not supported!")
            }
          }
)
#
# Encoding helper function for GDA 1.0
#
# Result must be a valid and url encoded query string without leading "?"
#
.sosEncodeRequestKVPGetDataAvailability_1.0.0 <- function(obj, sos, verbose = FALSE) {
  if (verbose) cat("[.sosEncodeRequestKVPGetDataAvailability_1.0.0] encoding", toString(obj), "\n")

  # mandatory
  .mandatory <- .kvpBuildRequestBase(sos, sosGetDataAvailabilityName)
  if (verbose) cat("[.sosEncodeRequestKVPGetDataAvailability_1.0.0] mandatory elements: ",
                   .mandatory, "\n")

  # optional
  .optionals <- ""
  if (sosIsListFieldAvailable(obj@procedures)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetDataAvailability_1.0.0] Adding procedures ",
                     paste0("'", unlist(obj@procedures), "'", collapse = ", "), "\n")
    .optionals <- paste(.optionals,
                        .kvpKeyAndValues(key = sosKVPParamNameProcedure, obj@procedures),
                        sep = "&")
  }
  if (sosIsListFieldAvailable(obj@observedProperties)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetDataAvailability_1.0.0] Adding observed properties: ",
                     paste0("'", unlist(obj@observedProperties), "'", collapse = ", "), "\n")
    .optionals <- paste(.optionals,
                        .kvpKeyAndValues(key = sosKVPParamNameObsProp, obj@observedProperties),
                        sep = "&")
  }
  if (sosIsListFieldAvailable(obj@featuresOfInterest)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetDataAvailability_1.0.0] Adding features of interest ",
                     paste0("'", unlist(obj@featuresOfInterest), "'", collapse = ", "), "\n")
    .optionals <- paste(.optionals,
                        .kvpKeyAndValues(key = sosKVPParamNameFoi, obj@featuresOfInterest),
                        sep = "&")
  }
  if (sosIsListFieldAvailable(obj@offerings)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetDataAvailability_1.0.0] Adding offerings ",
                     paste0("'", unlist(obj@offerings), "'", collapse = ", "), "\n")
    .optionals <- paste(.optionals,
                        .kvpKeyAndValues(key = sosKVPParamNameOffering, obj@offerings),
                        sep = "&")
  }

  # build final querystring
  .kvpString <- paste(.mandatory, .optionals, sep = "")
}

#
# sosName(SosGetDataAvailability_1.0.0)
#
setMethod(f = "sosName", signature = signature(obj = "SosGetDataAvailability_1.0.0"),
          definition = function(obj) {
            return(sosGetDataAvailabilityName)
          })

parseGetDataAvailabilityResponse <- function(obj, sos, verbose = FALSE) {
  if (verbose) cat("[parseGetDataAvailabilityResponse]")

  if (sos@version != sos200_version) {
    stop(paste0("[parseGetDataAvailabilityResponse] SOS version 2.0 required! Received '",
                sos@version, "'"))
  }
  ns <- sos@namespaces
  ns["gda"] <- xml2::xml_ns(obj)["gda"][[1]]

  gdaMembers <- xml2::xml_find_all(x = obj, xpath = gdaDataAvailabilityMemberName, ns = ns)

  if (verbose) cat("[parseGetDataAvailabilityResponse] with", length(gdaMembers), "element(s).\n")

  assign("phenTimeCache", list(), envir = get("sos4R_caches"))

  parsedGDAMembers <- lapply(gdaMembers, .parseGDAMember, sos, verbose)
  if (verbose) cat("[parseGetDataAvailabilityResponse] Done. Processed", length(parsedGDAMembers),
                   "elements")

  assign("phenTimeCache", list(), envir = get("sos4R_caches"))

  return(parsedGDAMembers)
}

.parseGDAMember <- function(gdaMember, sos, verbose = FALSE) {
  ns <- sos@namespaces
  ns["gda"] <- xml2::xml_ns(gdaMember)["gda"][[1]]
  id <- xml2::xml_attr(x = gdaMember, attr = "gml:id", ns = ns)
  if (verbose) cat("[parseGDAMember]", id, "\n")

  procedure <- .parseGDAReferencedElement(gdaMember, sos, gdaProcedureName, verbose)
  observedProperty <- .parseGDAReferencedElement(gdaMember, sos, gdaObservedPropertyName, verbose)
  featureOfInterest <- .parseGDAReferencedElement(gdaMember, sos, gdaFeatureOfInterestName, verbose)

  phenTime <- NULL
  ptNode <- xml2::xml_find_first(x = gdaMember, xpath = gdaPhenomenonTimeName, ns = ns)
  if (is.na(ptNode)) {
    stop(paste0("[parseGDAMember] ", gdaPhenomenonTimeName, " not found!"))
  }

  phenTimeCache <- get(x = "phenTimeCache", envir = get("sos4R_caches"))

  # if href is in <gda:phenomenonTime xlink:href="#tp_2"/> then in-document reference starting with "#" and than the GML:id of the referenced element
  if (gmlIsNodeReferenced(sos, ptNode)) {
    phenTime <- phenTimeCache[[substring(text = xml2::xml_attr(ptNode, "xlink:href", ns = ns), first = 2)]]

    if (is.null(phenTime)) {
      ptNode <- gmlGetReferencedNode(sos, gdaMember, ptNode, verbose = verbose)
      phenTime <- parseTimePeriod(xml2::xml_find_first(x = ptNode,
                                                       xpath = gmlTimePeriodName,
                                                       ns = ns),
                                  sos = sos)

      phenTimeCache[[phenTime@id]] <- phenTime
    }
  } else {
    phenTime <- parseTimePeriod(xml2::xml_find_first(x = ptNode,
                                                     xpath = gmlTimePeriodName,
                                                     ns = ns),
                                sos = sos)
    if (!is.na(phenTime@id)) {
      phenTimeCache[[phenTime@id]] <- phenTime
    }
  }

  assign(x = "phenTimeCache", value = phenTimeCache, envir = get("sos4R_caches"))

  if (is.null(phenTime)) {
    stop("[parseGDAMember] could NOT parse phenomenon time.")
  }

  DataAvailabilityMember(procedure = procedure,
                         observedProperty = observedProperty,
                         featureOfInterest = featureOfInterest,
                         phenomenonTime = phenTime)
}

.parseGDAReferencedElement <- function(gdaMember, sos, elementName, verbose = FALSE) {
  ns <- sos@namespaces
  ns["gda"] <- xml2::xml_ns(gdaMember)["gda"][[1]]
  node <- xml2::xml_find_first(x = gdaMember, xpath = elementName, ns = ns)
  if (is.na(node)) {
    stop(paste0("[parseGDAMember] no element found for '", elementName, "'."))
  }
  .element <- xml2::xml_attr(x = node, attr = "href")
  if (is.na(.element) || stringr::str_length(.element) < 1) {
    stop(paste0("[parseGDAMember] element found for '", elementName, "' misses href attribute. Found '",
                .element, "'."))
  }
  return(.element)
}
