################################################################################
# Copyright (C) 2018 by 52 North                                               #
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
                                       saveOriginal,
                                       xmlParseOptions = c(XML::NOERROR, XML::RECOVER)) {
  .filename <- NULL
  if(!is.null(saveOriginal)) {
    if(is.character(saveOriginal)) {
      .filename <- paste0(saveOriginal, ".xml")
      if(verbose){
        cat("[.getDataAvailability_1.0.0] Using saveOriginal parameter for file name: '", .filename, "'.\n", sep = "")
      }
    }
    else if(is.logical(saveOriginal)) {
      if(saveOriginal) {
        .filename <- tempfile(pattern=format(Sys.time(), sosDefaultFilenameTimeFormat), fileext = ".xml")
      }
      if(verbose) {
        cat("[.getDataAvailability_1.0.0] Generating file name:", .filename, "\n")
      }
    }
  }
  # FIXME encoding is not working
  .gda <- .createGetDataAvailability_1.0.0(sos = sos,
                                           procedures = procedures,
                                           observedProperties = observedProperties,
                                           featuresOfInterest = featuresOfInterest,
                                           offerings = offerings,
                                           verbose = verbose,
                                           inspect = inspect,
                                           saveOriginal = saveOriginal)

  if(verbose) {
    cat("[.getDataAvailability_1.0.0] REQUEST:\n", toString(.gda), "\n")
  }

  .responseString = sosRequest(sos = sos, request = .gda,
                               verbose = verbose, inspect = inspect)

  cat("[.getDataAvailability_1.0.0] Received response (size:", object.size(.responseString), "bytes), parsing ...\n")
  stop("FIXME continue implementation here: Parse Returned XML: add filename and line")
  return(TRUE)
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
            if(verbose) {
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
            if(!(operation@service == sosService &&
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
            if(obj@version == sos200_version) {
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
  if(verbose) cat("[.sosEncodeRequestKVPGetDataAvailability_1.0.0] encoding", toString(obj), "\n")

  # mandatory
  .mandatory <- .kvpBuildRequestBase(sos, sosGetDataAvailabilityName)
  if(verbose) cat("[.sosEncodeRequestKVPGetDataAvailability_1.0.0] mandatory elements: ",
                  .mandatory, "\n")

  # optional
  .optionals <- ""
  if (.isListFieldAvailable(obj@procedures)) {
    if(verbose) cat("[.sosEncodeRequestKVPGetDataAvailability_1.0.0] Adding procedures ",
                    obj@procedures, "\n")
    .optionals <- paste(.optionals,
                        .kvpKeyAndValues(key = sosProcedureName, obj@procedures),
                        sep = "&")
  }
  if (.isListFieldAvailable(obj@observedProperties)) {
    if(verbose) cat("[.sosEncodeRequestKVPGetDataAvailability_1.0.0] Adding observed properties ",
                    obj@observedProperties, "\n")
    .optionals <- paste(.optionals,
                        .kvpKeyAndValues(key = sosObservedPropertyName, obj@observedProperties),
                        sep = "&")
  }
  if (.isListFieldAvailable(obj@featuresOfInterest)) {
    if(verbose) cat("[.sosEncodeRequestKVPGetDataAvailability_1.0.0] Adding features of interest ",
                    obj@featuresOfInterest, "\n")
    .optionals <- paste(.optionals,
                        .kvpKeyAndValues(key = sosFeatureOfInterestName, obj@featuresOfInterest),
                        sep = "&")
  }
  if (.isListFieldAvailable(obj@offerings)) {
    if(verbose) cat("[.sosEncodeRequestKVPGetDataAvailability_1.0.0] Adding offerings ",
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
