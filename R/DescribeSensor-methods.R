################################################################################
# Copyright (C) 2010 by 52 North											   #
# Initiative for Geospatial Open Source Software GmbH						   #
# 																			   #
# Contact: Andreas Wytzisk													   #
# 52 North Initiative for Geospatial Open Source Software GmbH				   #
# Martin-Luther-King-Weg 24													   #
# 48155 Muenster, Germany													   #
# info@52north.org															   #
#																			   #
# This program is free software; you can redistribute and/or modify it under   #
# the terms of the GNU General Public License version 2 as published by the    #
# Free Software Foundation.													   #
#																			   #
# This program is distributed WITHOUT ANY WARRANTY; even without the implied   #
# WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU #
# General Public License for more details.									   #
#																			   #
# You should have received a copy of the GNU General Public License along with #
# this program (see gpl-2.0.txt). If not, write to the Free Software		   #
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or #
# visit the Free Software Foundation web page, http://www.fsf.org.			   #
#																			   #
# Author: Daniel Nüst (daniel.nuest@uni-muenster.de)                           #
# Created: 2010-06-18														   #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #                                              #
#                                                                              #
################################################################################

#
#
#
DescribeSensor <- function(
		service,
		version,
		procedure,
		outputFormat = "text/xml;subtype=&quot;sensorML/1.0.1&quot;") {
	new("DescribeSensor", service=service, version = version,
			procedure = procedure, outputFormat = outputFormat)
}


# encode as KVP
#
# see: http://www.oostethys.org/best-practices/best-practices-get
#
setMethod("kvp", "DescribeSensor", 
		function(obj) {
			
			if(obj@version == "1.0.0") {
				# mandatory:
				.service <- paste("service",
						.kvpEscapeSpecialCharacters(obj@service), sep = "=")
				.request <- "&request=DescribeSensor"
				.version <- paste("version", 
						.kvpEscapeSpecialCharacters(obj@version), sep = "=")
				.procedure <- paste("procedure",
						.kvpEscapeSpecialCharacters(obj@procedure), sep = "=")
				.format <- paste(
						"outputFormat",
						.kvpEscapeSpecialCharacters(
								gsub(obj@outputFormat, pattern = "&quot;",
										replacement = '"')),
						sep = "=")
				
				.kvpString <- paste(.service, .request, .version, .procedure,
						.format, sep="&")
				
				#cat(.kvpString)
				return(.kvpString)
			}
			else {
				warning("Version not supported!")
			}
		}
)

# encode as XML
setMethod("encode", "DescribeSensor", 
		function(obj) {
			xmlDoc <- xmlNode(name = "DescribeSensor", namespace = "sos",
					namespaceDefinitions = c(
							"sos" = "http://www.opengis.net/sos/1.0",
							"xsi" = "http://www.w3.org/2001/XMLSchema-instance"),
					attrs=c("xsi:schemaLocation" = "http://www.opengis.net/sos/1.0 http://schemas.opengis.net/sos/1.0.0/sosDescribeSensor.xsd",
							service = obj@service,
							outputFormat = obj@outputFormat,
							version = obj@version))
			
			procedure <- xmlNode(name = "procedure", namespace = "sos", obj@procedure)
			xmlDoc$children[[1]] <- procedure
			
			return(xmlDoc)
		}
)


# decode from XML
setMethod("decode", "DescribeSensor", 
		function(obj) {
			warning("Function decode is not implemented for DescribeSensor!")
			
			# TODO maybe it can become useful to parse an example request?
		}
)


#
summary.DescribeSensor = function(object, ...) {
	obj = list()
	
	# TODO implement summary method
	
	obj
}
setMethod("summary", "DescribeSensor", summary.DescribeSensor)

# saveXML(gc, file="/tmp/_testsave.xml")
setMethod("saveXML", "DescribeSensor",
		function(doc, file=NULL, compression=0, indent=TRUE, prefix = '<?xml version="1.0"?>\n', doctype = NULL, encoding = "", ...) {
			saveXML(doc = encode(doc), file=file, compression=compression, indent=indent, prefix=prefix, doctype=doctype, encoding=encoding, ...)
		}
)

