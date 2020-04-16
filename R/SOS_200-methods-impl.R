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
  # check the request for consistency with service description
  .checkResult <- checkRequest(service = sos, operation = request,
                               verbose = verbose)
  if (!.checkResult) {
    warning("Check returned FALSE! Turn on verbose option for possible details.",
            immediate. = TRUE)
  }

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
    .dcp <- sos@url

    if (sos@useDCPs) {
      if (verbose)
        cat("[.sosRequest_2.0.0] Using DCP from operation description.\n")

      .dcp <- sosGetDCP(sos, sosName(request), owsGetName)

      if (all(is.null(.dcp)) || all(is.na(.dcp))) {
        .dcp <- sos@url
        if (verbose) cat("[.sosRequest_2.0.0] Could not get DCP from operation description. This is OK for first GetCapabilities request, using",
                         .dcp, "\n")
      }

      .dcp <- .sosFilterDCPs(dcp = .dcp,
                             pattern = sos@dcpFilter[[.sosBindingKVP]],
                             verbose = verbose)
    }
    else if (verbose)
      cat("[.sosRequest_2.0.0] Not using DCP from capabilities, but use ", .dcp, "\n")

    if (isTRUE(grep(pattern = "[\\?]", x = .dcp) > 0)) {
      if (verbose) cat("Given url already contains a '?', appending arguments!\n")
      .url = paste0(.dcp, .encodedRequest)
    }
    else .url = paste(.dcp, .encodedRequest, sep = "?")

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

    .dcp <- sos@url

    if (sos@useDCPs) {
      .dcp <- sosGetDCP(sos, sosName(request), owsPostName) #sos@url as fallback

      if (all(is.null(.dcp)) || all(is.na(.dcp))) {
        .dcp <- sos@url
        if (verbose) cat("[.sosRequest_2.0.0] Could not get DCP from operation description. This is OK for first GetCapabilities request. Using", .dcp, "\n")
      }
      else {
        if (verbose) cat("[.sosRequest_2.0.0] Got DCPs from capabilites:",
                        toString(.dcp), "\n")
      }

      .dcp <- .sosFilterDCPs(dcp = .dcp,
                             pattern = sos@dcpFilter[[.sosBindingPOX]],
                             verbose = verbose)
      .dcp <- unlist(.dcp)
      if (verbose)
        cat("[.sosRequest_2.0.0] Using DCP:", toString(.dcp), "\n")
    }
    else if (verbose)
      cat("[.sosRequest_2.0.0] *NOT* using DCP from capabilities:",
          .dcp, "\n")

    .requestString <- toString(.encodedRequest)

    # using 'POST' for application/xml encoded requests
    if (verbose) cat("[.sosRequest_2.0.0] Do request...\n")

    .response <- httr::POST(url = .dcp,
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

  return(.content)
}

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
