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
parseSosObservationOffering <- function(ob) {
	.id <- xmlGetAttr(ob, "id")
	.name <- xmlValue(ob[["name"]])
	
	.time <- list(beginPosition = xmlValue(
					ob[["time"]][["TimePeriod"]][["beginPosition"]]),
			endPosition = xmlValue(
					ob[["time"]][["TimePeriod"]][["endPosition"]]))
	
	.procedure <- sapply(ob["procedure"], xmlGetAttr, "href")
	.observedProperty <- sapply(ob["observedProperty"], xmlGetAttr, "href")
	.featureOfInterest <- sapply(ob["featureOfInterest"], xmlGetAttr, "href")
	
	.responseFormat <- sapply(ob["responseFormat"], xmlValue)
	.resultModel <- sapply(ob["resultModel"], xmlValue)
	.responseMode <- sapply(ob["responseMode"], xmlValue)
	
	.boundedBy <- list(
			srsName = xmlGetAttr(ob[["boundedBy"]][["Envelope"]], "srsName"),
			lowerCorner = xmlValue(ob[["boundedBy"]][["Envelope"]][["lowerCorner"]]),
			upperCorner = xmlValue(ob[["boundedBy"]][["Envelope"]][["upperCorner"]]))
	
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
parseSosCapabilities <- function(document) {
	.caps.root <- xmlRoot(document)
	
	# attributes:
	.caps.attrs <- xmlAttrs(.caps.root)
	.caps.version <- .caps.attrs[["version"]]
	if(any(sapply(names(.caps.attrs), "==", "updateSequence")))
		.caps.updateSequence <- .caps.attrs[["updateSequence"]]
	else .caps.updateSequence <- as.character(NA)
	
	# as xml only: service identification, service provider and filter capabilities 
	.caps.si <- parseOwsServiceIdentification(.caps.root[["ServiceIdentification"]])
	.caps.sp <- OwsServiceProvider(.caps.root[["ServiceProvider"]])
	.caps.fc <- SosFilter_Capabilities(.caps.root[["Filter_Capabilities"]])
	
	# parsed: operations and contents section
	.operationsXML <- .filterXmlChildren(.caps.root[["OperationsMetadata"]],
			"Operation")
	.operations <- sapply(.operationsXML, parseOwsOperation)
	# add names for indexing of list
	names(.operations) <- lapply(.operations,
			function(obj) {
				return(obj@name)
			})
	
	.caps.om <- OwsOperationsMetadata(operations = .operations)
	
	.observationsXML <- .filterXmlChildren(.caps.root[["Contents"]][["ObservationOfferingList"]], "ObservationOffering")
	.observations = sapply(.observationsXML, parseSosObservationOffering)
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
