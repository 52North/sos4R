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
# Created: 2010-09-17                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

#
# samplingTime is the only time that's really used, so set it as default
#
TM_After <- function(propertyName = sosDefaultTempOpPropertyName, time) {
  new("TM_After", propertyName = propertyName, time = time)
}
TM_Before <- function(propertyName = sosDefaultTempOpPropertyName, time) {
  new("TM_Before", propertyName = propertyName, time = time)
}
TM_During <- function(propertyName = sosDefaultTempOpPropertyName, time) {
  new("TM_During", propertyName = propertyName, time = time)
}
TM_Equals <- function(propertyName = sosDefaultTempOpPropertyName, time) {
  new("TM_Equals", propertyName = propertyName, time = time)
}

#
# constructor functins ----
#
OgcBBOX <- function(propertyName = sosDefaultSpatialOpPropertyName,
                    envelope) {
  new("OgcBBOX", propertyName = propertyName, envelope = envelope)
}
OgcContains <- function(propertyName = sosDefaultSpatialOpPropertyName,
                        geometry = NULL, envelope = NULL) {
  new("OgcContains", propertyName = propertyName, geometry = geometry,
      envelope = envelope)
}
OgcIntersects <- function(propertyName = sosDefaultSpatialOpPropertyName,
                          geometry = NULL, envelope = NULL) {
  new("OgcIntersects", propertyName = propertyName, geometry = geometry,
      envelope = envelope)
}
OgcOverlaps <- function(propertyName = sosDefaultSpatialOpPropertyName,
                        geometry = NULL, envelope = NULL) {
  new("OgcOverlaps", propertyName = propertyName, geometry = geometry,
      envelope = envelope)
}


#
# encoding functions ----
#
setMethod(f = "encodeXML",
          signature = signature(obj = "TM_After", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] TM_After with", toString(obj@time))

            .encoded <- .encodeTM(nodeName = ogcTempOpTMAfterName,
                                  propertyName = obj@propertyName, time = obj@time,
                                  sos = sos, verbose = verbose)
            return(.encoded)
          }
)
setMethod(f = "encodeXML",
          signature = signature(obj = "TM_Before", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] TM_After with", toString(obj@time),
                            "\n")

            .encoded <- .encodeTM(nodeName = ogcTempOpTMBeforeName,
                                  propertyName = obj@propertyName, time = obj@time,
                                  sos = sos, verbose = verbose)
            return(.encoded)
          }
)
setMethod(f = "encodeXML",
          signature = signature(obj = "TM_During", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] TM_During with", toString(obj@time),
                            "\n")

            .encoded <- .encodeTM(nodeName = ogcTempOpTMDuringName,
                                  propertyName = obj@propertyName, time = obj@time,
                                  sos = sos, verbose = verbose)
            return(.encoded)
          }
)
setMethod(f = "encodeXML",
          signature = signature(obj = "TM_Equals", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] TM_Equals with", toString(obj@time),
                            "\n")

            .encoded <- .encodeTM(nodeName = ogcTempOpTMEqualsName,
                                  propertyName = obj@propertyName, time = obj@time,
                                  sos = sos, verbose = verbose)
            return(.encoded)
          }
)

.encodeTM <- function(nodeName, propertyName, time, sos, verbose = FALSE) {
  if (verbose) cat("[.encodeTM] ", nodeName, "\n")

  # FIXME: https://github.com/r-lib/xml2/issues/239
  #operation <- xml2::xml_new_root(nodeName,
  #                          xmlns = ogcNamespace)
  tm <- xml2::read_xml(paste0("<", ogcNamespacePrefix, ":", nodeName,
                              " xmlns:", ogcNamespacePrefix, "=\"", ogcNamespace, "\" />"))

  pn <- xml2::xml_add_child(tm, ogcPropertyNameName)
  xml2::xml_text(x = pn) <- propertyName

  time <- encodeXML(obj = time, sos = sos, verbose = verbose)
  xml2::xml_add_child(tm, time)

  return(tm)
}

setMethod(f = "encodeXML",
          signature = signature(obj = "OgcBBOX", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] OgcBBOX with", toString(obj), "\n")

            # FIXME: https://github.com/r-lib/xml2/issues/239
            #operation <- xml2::xml_new_root(opName,
            #                          xmlns = ogcNamespace)
            bbox <- xml2::read_xml(paste0("<", ogcBBOXName, " xmlns:", ogcNamespacePrefix, "=\"", ogcNamespace, "\" />"))
            pN <- .createPropertyName(propertyName = obj@propertyName)
            env <- encodeXML(obj = obj@envelope, sos = sos, verbose = verbose)
            xml2::xml_add_child(bbox, pN)
            xml2::xml_add_child(bbox, env)

            return(bbox)
          }
)

setMethod(f = "encodeXML",
          signature = signature(obj = "OgcContains", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] OgcContains with", toString(obj), "\n")

            .contains <- .encodeBinarySpatialOp(opName = ogcContainsName,
                                                propertyName = obj@propertyName,
                                                geometry = obj@geometry,
                                                envelope = obj@envelope,
                                                sos = sos)

            return(.contains)
          }
)

setMethod(f = "encodeXML",
          signature = signature(obj = "OgcIntersects", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] OgcIntersects with", toString(obj), "\n")

            .intersects <- .encodeBinarySpatialOp(opName = ogcIntersectsName,
                                                  propertyName = obj@propertyName,
                                                  geometry = obj@geometry,
                                                  envelope = obj@envelope,
                                                  sos = sos)

            return(.intersects)
          }
)

setMethod(f = "encodeXML",
          signature = signature(obj = "OgcOverlaps", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] OgcOverlaps with", toString(obj), "\n")

            .overlaps <- .encodeBinarySpatialOp(opName = ogcOverlapsName,
                                                propertyName = obj@propertyName, geometry = obj@geometry,
                                                envelope = obj@envelope, sos = sos)

            return(.overlaps)
          }
)

.encodeBinarySpatialOp <- function(opName,
                                   propertyName,
                                   geometry,
                                   envelope,
                                   sos) {
  # switch between geometry and envelope
  if (!is.null(geometry)) {
    geomOrEnv <- encodeXML(obj = geometry, sos = sos)
  }
  else if (!is.null(envelope)) {
    geomOrEnv <- encodeXML(obj = envelope, sos = sos)
  }
  else {
    stop("At least one of 'geometry' or 'envelope' has to be set.")
  }

  # FIXME: https://github.com/r-lib/xml2/issues/239
  #operation <- xml2::xml_new_root(opName,
  #                          xmlns = ogcNamespace)
  spOp <- xml2::read_xml(paste0("<", opName, " xmlns=\"", ogcNamespace, "\" />"))
  pN <- .createPropertyName(propertyName = propertyName)
  xml2::xml_add_child(spOp, pN)
  xml2::xml_add_child(spOp, geomOrEnv)

  return(spOp)
}

.createPropertyName <- function(propertyName) {
  # FIXME: https://github.com/r-lib/xml2/issues/239
  #pN <- xml2::xml_new_root(ogcPropertyNameName, xmlns = ogcNamespace)
  pN <- xml2::read_xml(paste0("<", ogcPropertyNameName, " xmlns:ogc=\"", ogcNamespace, "\" />"))
  xml2::xml_text(x = pN) <- propertyName
  return(pN)
}

setMethod(f = "encodeXML",
          signature = signature(obj = "OgcComparisonOps", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeXML] OgcComparisonOps with", toString(obj), "\n")
            warning("Encoding of OgcComparisonOps not implemented yet! Returning obj as is...")
            return(obj)
          }
)

#
# see: http://www.oostethys.org/best-practices/best-practices-get
#
setMethod(f = "encodeKVP",
          signature = signature(obj = "OgcBinaryTemporalOp", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeKVP] temporalOps: ", toString(obj), "\n")
            .time <- NULL
            .tempOpTime <- obj@time

            if (class(.tempOpTime) == "GmlTimeInstant") {
              if (verbose)
                cat("[encodeKVP] Encoding instant.\n")
              .time <- encodeKVP(.tempOpTime@timePosition@time, sos = sos,
                                 verbose = verbose)
            }
            # ignore type, because temporal operators are not supportded by the
            # GET binding
            else if (class(.tempOpTime) == "GmlTimePeriod") {
              if (!is.null(.tempOpTime@begin) && !is.null(.tempOpTime@end)) {
                if (verbose)
                  cat("[encodeKVP] Encoding period with begin and end.\n")
                .begin <- encodeKVP(.tempOpTime@begin@time@timePosition, sos = sos,
                                    verbose = verbose)
                .end <- encodeKVP(.tempOpTime@end@time@timePosition, sos = sos,
                                  verbose = verbose)
                .time <- paste(.begin, "/", .end, sep = "")
              }
              else if (!is.null(.tempOpTime@beginPosition)
                      && !is.null(.tempOpTime@endPosition)) {
                if (verbose)
                  cat("[encodeKVP] Encoding period with beginPosition and endPosition.\n")
                .begin <- encodeKVP(.tempOpTime@beginPosition@time, sos = sos,
                                    verbose = verbose)
                .end <- encodeKVP(.tempOpTime@endPosition@time, sos = sos,
                                  verbose = verbose)
                .time <- paste(.begin, "/", .end, sep = "")
              }
              else {
                stop(paste("Incomplete gml:TimePeriod:",
                           toString(.tempOpTime)))
              }
            }
            else {
              stop(paste("Cannot encode given object as KVP",
                         toString(.tempOpTime)))
            }

            return(.time)
          }
)
