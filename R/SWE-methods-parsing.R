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
# Author: Daniel Nüst (daniel.nuest@uni-muenster.de)                           #
# Created: 2010-09-15                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

#
#
#
parseTextBlock <- function(obj) {
	.id <- xmlGetAttr(node = obj, name = "id", default = NA_character_)
	.tS <- xmlGetAttr(node = obj, name = "tokenSeparator")
	.bS <- xmlGetAttr(node = obj, name = "blockSeparator")
	.dS <- xmlGetAttr(node = obj, name = "decimalSeparator")
	
	.tb <- SweTextBlock(tokenSeparator = .tS, blockSeparator = .bS,
			decimalSeparator = .dS, id = .id)
	return(.tb)
}

#
#
#
parsePhenomenonProperty <- function(obj) {
	.obsProp <- NULL
	
	# check if reference or inline phenomenon
	.href <- xmlGetAttr(node = obj, name = "href")
	if(!is.null(.href)) {
		.obsProp <- SwePhenomenonProperty(href = .href)
	}
	else {
		.noneText <- .filterXmlChildren(node = obj, xmlTextNodeName,
				includeNamed = FALSE)
		.compPhen <- .noneText[[1]]
		# 52N SOS only returns swe:CompositePhenomenon
		.name <- xmlName(.compPhen)
		
		if(.name == sweCompositePhenomenonName) {
			.phen <- parseCompositePhenomenon(.compPhen)
			.obsProp <- SwePhenomenonProperty(phenomenon = .phen)
		}
		else {
			warning(paste("Unsupprted observed property: ", .name))
		}
	}
	
	return(.obsProp)
}

#
#
#
parseCompositePhenomenon <- function(obj) {
	.id <- xmlGetAttr(node = obj, name = "id", default = NA_character_)
	.dimension <- as.integer(
			xmlGetAttr(node = obj, name = "dimension", default = NA_character_))
	.name <- xmlValue(obj[[gmlNameName]])
	
	.components <- lapply(obj[sweComponentName], parseComponent)
	
	# optional:
	.description <- NA_character_
	if(!is.null(obj[[gmlDescriptionName]])) {
		.description <- parsePhenomenonProperty(obj[[sweBaseName]])
	}
	.base <- NULL
	if(!is.null(obj[[sweBaseName]])) {
		.base <- parsePhenomenonProperty(obj[[sweBaseName]])
	}
	
	.compPhen <- SweCompositePhenomenon(id = .id, name = .name, 
			description = .description, dimension = .dimension,
			components = .components, base = .base)
	
	return(.compPhen)
}

#
#
#
parseComponent <- function(obj) {
	# 52N SOS only sets the href property on swe components, but still reuse function
	.component <- parsePhenomenonProperty(obj)
	return(.component)
}


