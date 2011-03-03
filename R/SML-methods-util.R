################################################################################
# Copyright (C) 2010 by 52 North                                               #
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
# Created: 2011-02-11                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################


.xPathToken <- "@@@"
.smlXPathIdentifier <- paste(
		"//sml:System/sml:identification//sml:identifier/sml:Term[@definition='urn:ogc:def:identifier:OGC::",
		.xPathToken, 
		"' or @definition='urn:ogc:def:identifier:OGC:1.0:",
		.xPathToken,
		"']/sml:value/text()", sep = "")
.smlXPathDescription <- "//sml:System/gml:description/text()"
.smlXPathPosition <- "//sml:System/sml:position/swe:Position"
.smlXPathObservedBBox <- "//swe:field[@name='observedBBOX']/swe:Envelope"

#
#
#
setMethod(f = "sosId", signature = signature(obj = "SensorML"),
		def = function(obj) {
			.root <- xmlRoot(obj@xml)
			.id <- .smlIdentifier(.root, "uniqueID")
			return(.id)
		})

#
#
#
setMethod(f = "sosName", signature = signature(obj = "SensorML"),
		def = function(obj) {
			.root <- xmlRoot(obj@xml)
			.name <- .smlIdentifier(.root, "shortName")
			return(.name)
		})

#
#
#
setMethod(f = "sosAbstract", signature = signature(obj = "SensorML"),
		def = function(obj) {
			.root <- xmlRoot(obj@xml)
			.abstract <- getNodeSet(doc = .root, path = .smlXPathDescription,
					namespaces = .sosNamespaceDefinitionsSML)[[1]]
			return(xmlValue(.abstract))
		})

#
#
#
setMethod(f = "sosGetCRS",
		signature = c(obj = "SensorML"),
		def = function(obj) {
			.coords <- sosCoordinates(obj)
			.crs <- sosGetCRS(attributes(.coords)[["referenceFrame"]])
			return(.crs)
		}
)

#
# extract the coordinates from the SensorML document and return as a data.frame
#
setMethod(f = "sosCoordinates", signature = signature(obj = "SensorML"),
		def = function(obj, sos, verbose = FALSE, forceReparse = FALSE) {
			if(all(dim(obj@coords) > 0) && !forceReparse) {
				if(verbose) cat("Coordinates already in SensorML, returning.\n")
				return(obj@coords)
			}
			
			if(verbose) cat("Parsing coordinates from SensorML...\n")
					
			.root <- xmlRoot(obj@xml)
			
			.xmlPosition <- getNodeSet(doc = .root, path = .smlXPathPosition,
					namespaces = .sosNamespaceDefinitionsSML)[[1]]
			.position <- parseSwePosition(.xmlPosition, sos = sos,
					verbose = verbose)
			.referenceFrame = attributes(.position)[["referenceFrame"]]			
			.uom <- lapply(.position, "[[", "uomCode")
			names(.uom) <- lapply(.position, "[[", "axisID")
			.name <- lapply(.position, "[[", "name")
			names(.name) <- lapply(.position, "[[", "axisID")
			
			.values <- lapply(.position, "[[", "value")
			names(.values) <- lapply(.position, "[[", "axisID")
			if(any(is.na(names(.values)))) {
				warning("No axisID given, cannot name data.frame with them, trying 'name'.")
				names(.values) <- lapply(.position, "[[", "name")
			}
			
			if(verbose) {
				cat("[sosCoordinates] values: ", toString(.values),	"\n")
				str(.values)
				cat("[sosCoordinates] names: ", names(.values), "\n")
			}
			
			.frame <- data.frame(.values)
			
			.oldAttrs <- attributes(.frame)
			attributes(.frame) <- c(as.list(.oldAttrs),
					list(referenceFrame = .referenceFrame,
							uom = .uom, name = .name))
			
			if(!is.na(sosId(obj)))
				row.names(.frame) <- sosId(obj)
			
			if(verbose) cat("[sosCoordinates] row names: ", row.names(.frame),
						"\n")
			
			return(.frame)
		}
)

#
#
#
setMethod(f = "sosBoundedBy",
		signature = signature(obj = "SensorML"),
		def = function(obj, sos, verbose = FALSE) {
			
			.observedBBox <- getNodeSet(doc = obj@xml,
					path = .smlXPathObservedBBox,
					namespaces = .sosNamespaceDefinitionsSML)[[1]]
			
			.referenceFrame <- .observedBBox[["referenceFrame"]]
			
			.llVector <- parseVector(.observedBBox[["lowerCorner"]][["Vector"]],
					sos = weathersos, verbose = verbose)
			.uuVector <- parseVector(.observedBBox[["upperCorner"]][["Vector"]],
					sos = weathersos, verbose = verbose)
			
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
			
			return(.bb)
		})

#
#
#
.smlIdentifier <- function(doc, identifierName) {
	.xpath <- gsub(pattern = .xPathToken, replacement = identifierName,
			x = .smlXPathIdentifier)
	
	.result <- getNodeSet(doc = doc, path = .xpath,
			namespaces = .sosNamespaceDefinitionsSML)
	
	return(xmlValue(.result[[1]]))
}
