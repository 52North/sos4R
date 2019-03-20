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
# Created: 2013-08-28                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

#
# main request method ----
#
.sosRequest_2.0.0 <- function(sos, request, verbose = FALSE, inspect = FALSE) {
  # check the request for consistency with service description
  .checkResult <- checkRequest(service = sos, operation = request,
                               verbose = verbose)
  if(!.checkResult) {
    warning("Check returned FALSE! Turn on verbose option for possible details.",
            immediate. = TRUE)
  }
  
  .response = ""
  
  # get encoding function for the respective method
  .encodingFunction <- sos@encoders[[sos@binding]]
  if(verbose) {
    .f <- functionBody(.encodingFunction)
    cat("[.sosRequest_2.0.0] Encoding Function (beginning of function body): ",
        substring(text = .f, first = 0, last = 60), " ... [",
        max((length(.f) - 60), 0), " more chrs].\n")
  }
  
  # encode!
  .encodedRequest = .encodingFunction(obj = request, sos = sos,
                                      verbose = verbose)
  
  if(sos@binding == .sosBindingKVP) {
    .dcp <- list("Get", "application/x-kvp", sos@url)
    
    if(sos@useDCPs) {
      if(verbose)
        cat("[.sosRequest_2.0.0] Using DCP from operation description.\n")
      
      .dcp <- sosGetDCP(sos, sosName(request), owsGetName)
      
      if(is.null(.dcp) || is.na(.dcp) || !length(.dcp)) {
        .dcp <- list("Get", "application/x-kvp", sos@url)
        if(verbose) cat("[.sosRequest_2.0.0] Could not get DCP from operation description. This is OK for first GetCapabilities request.\n")
      }
      
      if (is.list(.dcp) && length(.dcp) && is.list(.dcp[[1]])) {
        .dcp <- .sosFilterDCPs(dcp = .dcp,
                               pattern = sos@dcpFilter[[.sosBindingKVP]],
                               verbose = verbose)
        .dcp <- unlist(.dcp)
      }
    }
    else if(verbose)
      cat("[.sosRequest_2.0.0] Not using DCP from capabilities.\n",
          .dcp, "\n")
    
    if(isTRUE(grep(pattern = "[\\?]", x = .dcp) > 0)) {
      if (verbose) cat("Given url already contains a '?', appending arguments!\n")
      .url = paste0(.dcp[[owsDcpUrlIndex]], .encodedRequest)
    }
    else .url = paste(.dcp[[owsDcpUrlIndex]], .encodedRequest, sep = "?")
    
    if(!is.na(sos@additionalKVPs) && length(sos@additionalKVPs) > 0) {
      .kvps <- sos@additionalKVPs
      
      if(verbose)
        cat("[.sosRequest_2.0.0] adding extra KVP parameters:\n\t",
            toString(.kvps))
      
      .kvpsString <- .encodeAdditionalKVPs(.kvps)
      .url <- paste(.url, .kvpsString, sep = "&")
    }
    
    if(verbose || inspect) {
      cat("[.sosRequest_2.0.0] GET!\n[.sosRequest_2.0.0] REQUEST:\n\t",
          .url, "\n")
    }
    
    if(verbose) cat("[.sosRequest_2.0.0] Do request...")
    
    .response = getURL(url = .url, .opts = sos@curlOptions,
                       curl = sos@curlHandle,
                       .encoding = sosDefaultCharacterEncoding, httpauth = 1L)
    
    pos = regexpr('<\\?xml', .response)
    
    if(pos > 1){
      .response = substr(.response, pos, nchar(.response))
    }
    
    if(verbose) cat("[.sosRequest_2.0.0] ... done.")
  }
  else if(sos@binding == .sosBindingPOX) {
    if(verbose || inspect) {
      cat("[.sosRequest_2.0.0] POST!\n[.sosRequest_2.0.0] REQUEST:\n")
      print(.encodedRequest)
    }
    
    .dcp <- list("Post", "application/xml", sos@url)
    
    if(sos@useDCPs) {		
      .dcp <- sosGetDCP(sos, sosName(request), owsPostName) #sos@url as fallback
      if(is.null(.dcp) || is.na(.dcp) || !length(.dcp)) {
        .dcp <- list("Post", "application/xml", sos@url)
        if(verbose) cat("[.sosRequest_2.0.0] Could not get DCP from operation description. This is OK for first GetCapabilities request. Using", .dcp, "\n")
      }
      else {
        if(verbose) cat("[.sosRequest_2.0.0] Got DCPs from capabilites:",
                        toString(.dcp), "\n")
      }
      if (is.list(.dcp) && length(.dcp) && is.list(.dcp[[1]])) {
        .dcp <- .sosFilterDCPs(dcp = .dcp,
                               pattern = sos@dcpFilter[[.sosBindingPOX]],
                               verbose = verbose)
        .dcp <- unlist(.dcp)
      }
      if(verbose)
        cat("[.sosRequest_2.0.0] Using DCP:", toString(.dcp), "\n")
    }
    else if(verbose)
      cat("[.sosRequest_2.0.0] *NOT* using DCP from capabilities:",
          .dcp, "\n")
    
    .request <- toString(.encodedRequest)
    
    # using 'POST' for application/xml encoded requests
    if(verbose) cat("[.sosRequest_2.0.0] Do request...")
    
    .response <- httr::POST(url = .dcp[[owsDcpUrlIndex]],
                            content_type(.dcp[[owsDcpContentTypeIndex]]),
                            accept(.dcp[[owsDcpContentTypeIndex]]),
                            body = .request)
    
    stop_for_status(.response, "sending POST request")
    
    .response <- content(x = .response, as = "text", encoding = sosDefaultCharacterEncoding)

    if(verbose) cat("[.sosRequest_2.0.0] ... done.")
  }
  else if(sos@binding == .sosBindingSOAP) {
    if(verbose || inspect) {
      print("[.sosRequest_2.0.0] SOAP! REQUEST:\n")
      print(.encodedRequest)
    }
    
    # TODO add SOAP request method
    stop("[sos4R] ERROR: SOAP is not implemented for SOS 2.0.0.\n")
  }
  else {
    stop(paste("Unsupported method, has to be one of",
               SosSupportedBindings(), "but is", sos@binding))
  }
  
  if(verbose) {
    cat("[.sosRequest_2.0.0] RESPONSE:\n")
    print(.response)
    if(is.raw(.response)) cat("raw as char: ", rawToChar(.response), "\n")
  }
  
  if(length(.response) > 0 & 
     regexpr("(<html>|<HTML>|<!DOCTYPE HTML|<!DOCTYPE html)", .response) > 0) {
    if(verbose) cat("[.sosRequest_2.0.0] Got HTML, probably an error.\n")
    
    # might still be KML with embedded HTML!
    if(regexpr("(http://www.opengis.net/kml/)", .response) > 0) {
      if(verbose) cat("[.sosRequest_2.0.0] Got KML! Can continue...\n")
    }
    else stop(paste("[sos4R] ERROR: Got HTML response!:\n", .response,
                    "\n\n"))
  }
  
  return(.response)
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
  if(verbose) cat("[.getCapabilities_2.0.0] REQUEST:\n", toString(.gc), "\n")
  
  .responseString = sosRequest(sos = sos, request = .gc,
                               verbose = verbose, inspect = inspect)
  if(verbose){
    cat("[.getCapabilities_2.0.0] RESPONSE:\n", .responseString , "\n")
  }
  
  tmpStoredXMLCaps = tempfile()
  
  fileConn<-file(tmpStoredXMLCaps)
  writeLines(.responseString, fileConn)
  close(fileConn)
  
  #dump(xmlCaps, file = tmpStoredXMLCaps)
  
  .response <- xmlParseDoc(file = tmpStoredXMLCaps)
  if(verbose || inspect) {
    cat("[.getCapabilities_2.0.0] RESPONSE DOC:\n")
    print(.response)
  }
  
  if(.isExceptionReport(.response)) {
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
