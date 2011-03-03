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
			
			.colNames <- sapply(.position, "[[", "name")
			.colUoms <- lapply(.position, "[[", "uomCode")
			.values <- lapply(.position, "[[", "value")
			names(.values) <- lapply(.position, "[[", "axisID")
			
			if(any(is.na(names(.values)))) {
				warning("No axisID given, cannot name data.frame with them, trying 'name'.")
				names(.values) <- lapply(.position, "[[", "name")
			}
			
			if(verbose) {
				cat("[sosCoordinates] values: ", toString(.values),	"\n")
				cat("[sosCoordinates] names: ", names(.values), "\n")
			}
			
			.frame <- data.frame(rbind(.values))
			
			for (i in seq(1, length(.colUoms))) {
				.newAttrs <- list(.colUoms[[i]], .colNames[[i]],
						.referenceFrame)
				names(.newAttrs) <- c("uom", "name", "referenceFrame")
				if(verbose) cat("[sosCoordinates] New attributes: ",
							toString(.newAttrs), "\n")

				.oldAttrs <- attributes(.frame[,i])
				attributes(.frame[,i]) <- c(as.list(.oldAttrs), .newAttrs)
			}
			
			if(verbose) cat("[sosCoordinates] row names: ",
						toString(sosId(obj)), "\n")
			
			if(!is.na(sosId(obj)))
				row.names(.frame) <- sosId(obj)
			
			.oldAttrs <- attributes(.frame)
			attributes(.frame) <- c(as.list(.oldAttrs),
					list(referenceFrame = .referenceFrame))
			
			return(.frame)
		}
)

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
