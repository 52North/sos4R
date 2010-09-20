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
# Created: 2010-06-18                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

#
#
#
parseSosObservationOffering <- function(obj, sos) {
	.id <- xmlGetAttr(obj, "id")
	.name <- xmlValue(obj[[gmlNameName]])
	
	.time <- parseTimeGeometricPrimitiveFromParent(obj = obj[[sosTimeName]],
			timeFormat = sosTimeFormat(sos))
	.procedure <- sapply(obj[sosProcedureName], xmlGetAttr, "href")
	.observedProperty <- sapply(obj[sosObservedPropertyName], xmlGetAttr,
			"href")
	.featureOfInterest <- sapply(obj[sosFeatureOfInterestName], xmlGetAttr,
			"href")
	
	.responseFormat <- sapply(obj[sosResponseFormatName], xmlValue)
	.resultModel <- sapply(obj[sosResultModelName], xmlValue)
	.responseMode <- sapply(obj[sosResponseFormatName], xmlValue)
	
	.env <- obj[[gmlBoundedByName]][[gmlEnvelopeName]]
	.boundedBy <- list(
			srsName = xmlGetAttr(.env, "srsName"),
			lowerCorner = xmlValue(.env[[gmlLowerCornerName]]),
			upperCorner = xmlValue(.env[[gmlUpperCornerName]]))
	
	.ob <- SosObservationOffering(id = .id, name = .name, 
			time = .time, procedure = .procedure,
			observedProperty = .observedProperty,
			featureOfInterest = .featureOfInterest,
			responseFormat = .responseFormat, resultModel = .resultModel,
			responseMode = .responseMode, boundedBy = .boundedBy)
	
	return(.ob)
}

#
#
#
parseSosCapabilities <- function(obj, sos) {
	.caps.root <- xmlRoot(obj)
	
	# attributes:
	.caps.attrs <- xmlAttrs(.caps.root)
	.caps.version <- .caps.attrs[["version"]]
	.caps.updateSequence <- xmlGetAttr(node = .caps.root,
			name = "updateSequence", default = NA_character_)
	
	# as xml only: filter capabilities
	.caps.fc <- SosFilter_Capabilities(.caps.root[[sosFilterCapabilitiesName]])
	
	# parsed parts:
	.caps.si <- parseOwsServiceIdentification(
			.caps.root[[owsServiceIdentificationName]])
	.caps.sp <- parseOwsServiceProvider(.caps.root[[owsServiceProviderName]])
	.operationsXML <- .filterXmlChildren(
			node = .caps.root[[owsOperationsMetadataName]],
			childrenName = owsOperationName)
	
	.operations <- lapply(.operationsXML, parseOwsOperation)
	# add names for indexing of list
	names(.operations) <- lapply(.operations,
			function(obj) {
				return(obj@name)
			})
	.caps.om <- OwsOperationsMetadata(operations = .operations)
	
	.observationsXML <- .filterXmlChildren(
			node = .caps.root[[sosContentsName]][[sosObservationOfferingListName]],
			childrenName = sosObservationOfferingName)
	.observations = sapply(.observationsXML, parseSosObservationOffering,
			sos = sos)
	# add names to list
	names(.observations) <- lapply(.observations,
			function(obj) {
				return(obj@id)
			})
	
	.caps.contents <- SosContents(observationOfferings = .observations)
	
	.capabilities <- SosCapabilities(version = .caps.version,
			updateSequence = .caps.updateSequence,
			identification = .caps.si,
			provider = .caps.sp,
			operations = .caps.om,
			filterCaps = .caps.fc,
			contents = .caps.contents)
	
	return(.capabilities)
}
