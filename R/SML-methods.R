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
# Created: 2010-06-18                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
################################################################################

#
#
#
SensorML <- function(xml, coords = data.frame(), id = NA_character_,
                     name = NA_character_, description = NA_character_,
                     boundedBy = matrix()) {
  new("SensorML", xml = xml, coords = coords, id = id, name = name,
      description = description, boundedBy = boundedBy)
}

.xPathToken <- "@@@"
.smlXPathIdentifier <- paste(
  "//sml:System/sml:identification//sml:identifier/sml:Term[@definition='urn:ogc:def:identifier:OGC::",
  .xPathToken,
  "' or @definition='urn:ogc:def:identifier:OGC:1.0:",
  .xPathToken,
  "' or @definition='urn:ogc:def:identifier:OGC:", # technically this is invalid, but common
  .xPathToken,
  "']/sml:value/text()", sep = "")
.smlXPathDescription <- "//sml:System/gml:description/text()"
.smlXPathPosition <- "//sml:System/sml:position/swe:Position"
.smlXPathObservedBBox <- "//swe:field[@name='observedBBOX']/swe:Envelope"


#
# parseSensorML(mySensor@xml, sos = mySOS, verbose = TRUE)
#
parseSensorML <- function(obj, sos, verbose = FALSE) {
  if (verbose) cat("[parseSensorML] Starting... \n")

  .id <- .smlIdentifier(obj, "uniqueID", verbose = verbose)
  .shortName <- .smlIdentifier(obj, "shortName", verbose = verbose)
  .descrNode <- xml2::xml_find_first(x = obj,
                                     xpath = .smlXPathDescription,
                                     ns = SosAllNamespaces())
  .description <- xml2::xml_text(x = .descrNode)

  if (verbose) cat("[parseSensorML] Got ID", .id,
                   "and shortName", .shortName,
                   "and description", .description, "\n")

  # bounded by
  if (verbose) cat("[parseSensorML] Parsing boundedBy from", .smlXPathObservedBBox, "\n")
  .observedBBox <- xml2::xml_find_first(x = obj,
                                        xpath = .smlXPathObservedBBox,
                                        ns = SosAllNamespaces())
  if (!is.na(.observedBBox)) {
    .referenceFrame <- xml2::xml_attr(x = .observedBBox, attr = "referenceFrame", ns = SosAllNamespaces())
    .llVector <- parseSweVector(xml2::xml_child(x = .observedBBox,
                                             search = paste0(sweUpperCornerName, "/", sweVectorName),
                                             ns = SosAllNamespaces()),
                             sos = sos, verbose = verbose)
    .uuVector <- parseSweVector(xml2::xml_child(x = .observedBBox,
                                             search = paste0(sweLowerCornerName, "/", sweVectorName),
                                             ns = SosAllNamespaces()),
                             sos = sos, verbose = verbose)
    .bb <- matrix(c(.llVector[["x"]][["value"]],
                    .llVector[["y"]][["value"]],
                    .uuVector[["x"]][["value"]],
                    .uuVector[["y"]][["value"]]),
                  ncol = 2,
                  dimnames = list(c("coords.lon", "coords.lat"),
                                  c("min", "max")))
    .oldAttrs <- attributes(.bb)
    attributes(.bb) <- c(.oldAttrs,
                         list(referenceFrame = .referenceFrame))

    if (verbose) cat("[parseSensorML] Parsed bounding box: ", toString(.bb), "\n")
  }
  else {
    .bb <- matrix()
    if (verbose) cat("[parseSensorML] No boundedBy element found, bbox is ", .bb, "\n")
  }

  # coordinates
  if (verbose) cat("[parseSensorML] Parsing coordinates from", .smlXPathPosition, "\n")
  .xmlPosition <- xml2::xml_find_first(x = obj,
                                       xpath = .smlXPathPosition,
                                       ns = SosAllNamespaces())
  if (!is.na(.xmlPosition)) {
    .position <- parseSwePosition(.xmlPosition,
                                  sos = sos,
                                  verbose = verbose)
    .referenceFrame = attributes(.position)[["referenceFrame"]]
    .uom <- lapply(.position, "[[", "uomCode")
    names(.uom) <- lapply(.position, "[[", "axisID")
    .name <- lapply(.position, "[[", "name")
    names(.name) <- lapply(.position, "[[", "axisID")

    .values <- lapply(.position, "[[", "value")
    names(.values) <- lapply(.position, "[[", "axisID")
    if (any(is.na(names(.values)))) {
      warning("[parseSensorML] No axisID given, cannot name data.frame with them, trying 'name'.")
      names(.values) <- lapply(.position, "[[", "name")
    }

    if (verbose) {
      cat("[parseSensorML] names: ", names(.values), "\n")
      cat("[parseSensorML] values: ", toString(.values),	"\n")
    }

    .coords <- data.frame(.values)
    .oldAttrs <- attributes(.coords)
    attributes(.coords) <- c(as.list(.oldAttrs),
                             list(referenceFrame = .referenceFrame,
                                  uom = .uom, name = .name))

    if (!is.na(.id))
      row.names(.coords) <- .id
    if (verbose) cat("[parseSensorML]  row names: ", row.names(.coords),
                    "\n")
  }
  else {
    .coords <- data.frame()
  }

  # create instance
  .sml = SensorML(xml = obj,
                  coords = .coords,
                  id = .id,
                  name = .shortName,
                  description = .description,
                  boundedBy = .bb)

  if (verbose) cat("[parseSensorML]  Done: ", toString(.sml), "\n")

  return(.sml)
}


#
#
#
.smlIdentifier <- function(doc, identifierName, verbose = FALSE) {
  .xpath <- gsub(pattern = .xPathToken,
                 replacement = identifierName,
                 x = .smlXPathIdentifier)

  if (verbose) cat("[.smlIdentifier] Accessing path ", .xpath, "\n")
  .identifierText <- xml2::xml_text(xml2::xml_find_first(x = doc,
                                                         xpath = .xpath,
                                                         ns = SosAllNamespaces()))
  if (verbose) cat("[.smlIdentifier] Parsed", identifierName, ":", .identifierText, "\n")

  return(.identifierText)
}


#
#
#
plot.SensorML <- function(x, y, ...) {
  .sp <- as(x, "SpatialPointsDataFrame")
  plot(.sp, ...)
}
setMethod("plot", signature(x = "SensorML", y = "missing"), plot.SensorML)

