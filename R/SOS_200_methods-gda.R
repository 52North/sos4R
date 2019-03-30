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
# visit the Free Software Foundation web page, http://www.fsf.org              #
#                                                                              #
# Author: Eike Hinderk Jürrens (e.h.juerrens@52north.org)                      #
# Created: 2018-11-23                                                          #
# Project: sos4R - visit project web page https://52north.org/geostatistics    #
#                                                                              #
################################################################################

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
                                       saveOriginal) {
  .filename <- NULL
  if (!is.null(saveOriginal)) {
    if (is.character(saveOriginal)) {
      .filename <- saveOriginal
      if (verbose) cat("[.getDataAvailability_1.0.0] Using saveOriginal parameter for file name: '", .filename, "'.\n", sep = "")
    }
    else if (is.logical(saveOriginal)) {
      if (saveOriginal) {
        .filename <- tempfile(pattern = format(Sys.time(), sosDefaultFilenameTimeFormat), fileext = ".xml")
      }
      if (verbose) cat("[.getDataAvailability_1.0.0] Generated file name:", .filename, "\n")
    }
  }
  .gda <- SosGetDataAvailability_1.0.0(service = sosService,
                                       version = sos@version,
                                       observedProperties = observedProperties,
                                       procedures = procedures,
                                       featuresOfInterest = featuresOfInterest,
                                       offerings = offerings)

  if (verbose) cat("[.getDataAvailability_1.0.0] REQUEST:\n", toString(.gda), "\n")

  .response = sosRequest(sos = sos,
                         request = .gda,
                         verbose = verbose,
                         inspect = inspect)

  if (verbose) cat("[.getDataAvailability_1.0.0] Received response (size:", object.size(.response),
                   "bytes), parsing ...\n")

  if (inspect) {
    cat("[.getDataAvailability_1.0.0] Response XML document:\n")
    print(.response)
  }

  if (!is.null(.filename)) {
    xml2::write_xml(x = .response, file = .filename)
    if (verbose) cat("[.getDataAvailability_1.0.0] Saved original document:", .filename, "\n")
  }

  if (.isExceptionReport(.response)) {
    return(.handleExceptionReport(sos, .response))
  }

  .parsingFunction <- sosParsers(sos)[[mimeTypeXML]]

  if (verbose) {
    cat("[.getDataAvailability_1.0.0] Parsing with function ")
    print(.parsingFunction)
  }

  .dams <- .parsingFunction(obj = .response, sos = sos, verbose = verbose)
  if (verbose) cat("[.getDataAvailability_1.0.0] Received and parsed", length(.dams),
                    "data availability members.\n")
  return (.dams)
}


#
# getDataAvailability - Impl. ----
#
# Implementation of abstract method from Generic-Methods.R
#
setMethod(f = "getDataAvailability",
          signature = signature(sos = "SOS_2.0.0"),
          def = function(sos,
                         procedures,
                         observedProperties,
                         featuresOfInterest,
                         offerings,
                         verbose,
                         inspect = FALSE,
                         saveOriginal = NULL) {
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
                                              saveOriginal = saveOriginal))
          }
)

#
# checkRequest - GetDataAvailability ----
#
setMethod(f = "checkRequest",
          signature = signature(service = "SOS_2.0.0",
                                operation = "SosGetDataAvailability_1.0.0",
                                verbose = "logical"),
          def = function(service, operation, verbose) {
            # check if operation is for SOS and operation is GetDataAvailability
            if (!(operation@service == sosService &&
                  operation@request == sosGetDataAvailabilityName)) {
              stop("Wrong input! Require classes 'SOS_2.0.0' as service and 'GetDataAvailability' as operation.")
              return(FALSE)
            }

            # TODO implement checkRequest for GetDataAvailability
            # all elements are optional
            # TODO check feature identifier
            # TODO check observed properties
            # TODO check offerings
            # TODO check procedures

            return(TRUE)
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
  if (.isListFieldAvailable(obj@procedures)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetDataAvailability_1.0.0] Adding procedures ",
                     obj@procedures, "\n")
    .optionals <- paste(.optionals,
                        .kvpKeyAndValues(key = sosProcedureName, obj@procedures),
                        sep = "&")
  }
  if (.isListFieldAvailable(obj@observedProperties)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetDataAvailability_1.0.0] Adding observed properties ",
                     obj@observedProperties, "\n")
    .optionals <- paste(.optionals,
                        .kvpKeyAndValues(key = sosObservedPropertyName, obj@observedProperties),
                        sep = "&")
  }
  if (.isListFieldAvailable(obj@featuresOfInterest)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetDataAvailability_1.0.0] Adding features of interest ",
                     obj@featuresOfInterest, "\n")
    .optionals <- paste(.optionals,
                        .kvpKeyAndValues(key = sosFeatureOfInterestName, obj@featuresOfInterest),
                        sep = "&")
  }
  if (.isListFieldAvailable(obj@offerings)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetDataAvailability_1.0.0] Adding offerings ",
                     obj@offerings, "\n")
    .optionals <- paste(.optionals,
                        .kvpKeyAndValues(key = sosOfferingName, obj@offerings),
                        sep = "&")
  }

  # build final querystring
  .kvpString <- paste(.mandatory, .optionals, sep = "")
}
#
# sosName(SosGetDataAvailability_1.0.0)
#
setMethod(f = "sosName", signature = signature(obj = "SosGetDataAvailability_1.0.0"),
          def = function(obj) {
            return(sosGetDataAvailabilityName)
          })

parseGetDataAvailabilityResponse <- function(obj, sos, verbose = FALSE) {
  if (verbose) cat("[parseGetDataAvailabilityResponse]")

  if (sos@version != sos200_version) {
    stop(paste0("[parseGetDataAvailabilityResponse] SOS version 2.0 required! Received '",
                sos@version, "'"))
  }

  .gdaMembers <- xml2::xml_find_all(x = obj, xpath = sosGDAMemberName, ns = SosAllNamespaces())

  if (verbose) cat("[parseGetDataAvailabilityResponse] with", length(.gdaMembers), "element(s).\n")
  phenTimeCache <- list()
  .parsedGDAMembers <- lapply(.gdaMembers, .parseGDAMember, sos, phenTimeCache, verbose)
  if (verbose) cat("[parseGetDataAvailabilityResponse] Done. Processed", length(.parsedGDAMembers),
                   "elements")
  return(.parsedGDAMembers)
}

.parseGDAMember <- function(gdaMember, sos, phenTimeCache, verbose = FALSE) {
  if (verbose) cat("[parseGDAMember]")
  #
  # procedure
  .procedure <- .parseGDAReferencedElement(gdaMember, sos, sosProcedureName, verbose)
  #
  # observed property
  .observedProperty <- .parseGDAReferencedElement(gdaMember, sos, sosObservedPropertyName, verbose)
  #
  # feature of interest
  .featureOfInterest <- .parseGDAReferencedElement(gdaMember, sos, sosFeatureOfInterestName, verbose)
  #
  # phenomenon time
  .phenTime <- NULL
  .ptNode <- xml2::xml_find_all(x = gdaMember, xpath = sosPhenomenonTimeName, ns = SosAllNamespaces())
  # Case A: Encoded
  if (!.nodeFound(.ptNode)) {
    stop(paste0("[parseGDAMember] ", sosPhenomenonTimeName, " not found!"))
  }
  .ptNode <- .ptNode[[1]]
  # href is an in-document reference starting with "#" and than the GML:id of the referenced element
  if (.isHrefAttributeAvailable(.ptNode)) {
    .ptNodeHref <- xml2::xml_attr(x = .ptNode, attr = "href")
    if(verbose) cat(paste0("[parseGDAMember] trying to get referenced phenomenon time via '", .ptNodeHref, "'."))
    .phenTime <- phenTimeCache[[stringr::str_sub(.ptNodeHref, start = 2)]]
    if (is.null(.phenTime)) {
      stop(paste0("[parseGDAMember] XML document invalid. Time reference '", .ptNodeHref ,"' not in document."))
    }
  }
  # Case B: in-document reference -> cache
  else {
    .phenTime <- parseTimePeriod(xml2::xml_find_first(x = gdaMember,
                                                      xpath = gmlTimePeriodName,
                                                      ns = SosAllNamespaces()),
                                 format = sosDefaultTimeFormat)
    phenTimeCache[[.phenTime@id]] <- .phenTime
  }
  if (is.null(.phenTime)) {
    stop("[parseGDAMember] could NOT parse phenomenon time.")
  }
  DataAvailabilityMember(.procedure, .observedProperty, .featureOfInterest, .phenTime)
}

.nodeFound <- function(.node = NULL) {
  return(!is.null(.node) && !is.na(.node) && is.list(.node) && length(.node) == 1)
}

.isHrefAttributeAvailable <- function(.node = NULL) {
  if (is.null(.node)) return(FALSE)
  .nodeAttributes <- xml2::xml_attrs(x = .node)

  if (is.null(.nodeAttributes)) return(FALSE)
  .href <- .nodeAttributes[["href"]]
  return(!is.null(.href) && is.character(.href) && length(.href) > 0)
}

.parseGDAReferencedElement <- function(gdaMember, sos, elementName, verbose = FALSE) {
  .nodes <- xml2::xml_find_all(x = gdaMember, xpath = elementName, ns = SosAllNamespaces())
  if (is.na(.nodes) || length(.nodes) != 1) {
    stop(paste0("[parseGDAMember] no element found for '", elementName, "'."))
  }
  .element <- xml2::xml_attr(x = .nodes[[1]], attr = "href")
  if (is.na(.element) || stringr::str_length(.element) < 1) {
    stop(paste0("[parseGDAMember] element found for '", elementName, "' misses href attribute. Found '",
                .element, "'."))
  }
  return(.element)
}
