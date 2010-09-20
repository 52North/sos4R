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
# Created: 2010-09-17                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

#
# samplingTime is the only time that's really used, so set it as default
#
"TM_After" <- function(propertyName = sosDefaultTempOpPropertyName, time) {
	new("TM_After", propertyName = propertyName, time = time)
}
"TM_Before" <- function(propertyName = sosDefaultTempOpPropertyName, time) {
	new("TM_Before", propertyName = propertyName, time = time)
}
"TM_During" <- function(propertyName = sosDefaultTempOpPropertyName, time) {
	new("TM_During", propertyName = propertyName, time = time)
}
"TM_Equals" <- function(propertyName = sosDefaultTempOpPropertyName, time) {
	new("TM_Equals", propertyName = propertyName, time = time)
}


#
#
#
setMethod(f = "encodeXML",
		signature = signature(obj = "TM_After"),
		def = function(obj, verbose) {
			if(verbose) cat("Encoding XML TM_After with", toString(obj@time))
			
			.encoded <- .encodeTM(nodeName = ogcTempOpTMAfterName,
					propertyName = obj@propertyName, time = obj@time,
					verbose = verbose)
			return(.encoded)
		}
)
setMethod(f = "encodeXML",
		signature = signature(obj = "TM_Before"),
		def = function(obj, verbose) {
			if(verbose) cat("Encoding XML TM_After with", toString(obj@time),
						"\n")
			
			.encoded <- .encodeTM(nodeName = ogcTempOpTMBeforeName,
					propertyName = obj@propertyName, time = obj@time,
					verbose = verbose)
			return(.encoded)
		}
)
setMethod(f = "encodeXML",
		signature = signature(obj = "TM_During"),
		def = function(obj, verbose) {
			if(verbose) cat("Encoding XML TM_During with", toString(obj@time),
						"\n")
			
			.encoded <- .encodeTM(nodeName = ogcTempOpTMDuringName,
					propertyName = obj@propertyName, time = obj@time,
					verbose = verbose)
			return(.encoded)
		}
)
setMethod(f = "encodeXML",
		signature = signature(obj = "TM_Equals"),
		def = function(obj, verbose) {
			if(verbose) cat("Encoding XML TM_Equals with", toString(obj@time),
						"\n")
			
			.encoded <- .encodeTM(nodeName = ogcTempOpTMEqualsName,
					propertyName = obj@propertyName, time = obj@time,
					verbose = verbose)
			return(.encoded)
		}
)

.encodeTM <- function(nodeName, propertyName, time, verbose = FALSE) {
	if(verbose) cat("Encoding TM element ", nodeName, "\n")
	
	.tm <- xmlNode(name = nodeName,
			namespace = ogcNamespacePrefix)
	.pn <- xmlNode(name = ogcPropertyNameName,
			namespace = ogcNamespacePrefix)
	xmlValue(.pn) <- propertyName
	.tm$children[[1]] <- .pn
	.time <- encodeXML(time, verbose)
	.tm$children[[2]] <- .time
	
	return(.tm)
}

#
# see: http://www.oostethys.org/best-practices/best-practices-get
#
setMethod(f = "encodeKVP",
		signature = signature(obj = "OgcBinaryTemporalOpType"),
		def = function(obj, verbose) {
			if(verbose) cat("Encoding KVP temporalOps: ", toString(obj))
			.time <- NULL
			.tempOpTime <- obj@time
			
			if(class(.tempOpTime) == "GmlTimeInstant") {
				.time <- format(.tempOpTime@timePosition@time,
						sosDefaultKVPTimeFormat)
			}
			# ignore type, because temporal operators are not supportded by the
			# GET binding
			else if (class(.tempOpTime) == "GmlTimePeriod") {
				if(!is.null(.tempOpTime@begin) && !is.null(.tempOpTime@end)) {
					.begin <- format(.tempOpTime@begin@time@timePosition,
							sosDefaultKVPTimeFormat)
					.end <- format(.tempOpTime@end@time@timePosition,
							sosDefaultKVPTimeFormat)
					.time <- paste(.begin, "/", .end, sep = "")
				}
				else if(!is.null(.tempOpTime@beginPosition)
						&& !is.null(.tempOpTime@endPosition)) {
					.begin <- format(.tempOpTime@beginPosition@time,
							sosDefaultKVPTimeFormat)
					.end <- format(.tempOpTime@endPosition@time,
							sosDefaultKVPTimeFormat)
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
