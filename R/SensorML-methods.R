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
SensorML <- function(xml) {
	new("SensorML", xml = xml)
}

# encode as XML
setMethod("encode", "SensorML", 
		function(obj) {
			warning("Function encode is not implemented for SensorML!")
		}
)


# decode from XML
setMethod("decode", "SensorML", 
		function(obj) {
			warning("Function decode is not implemented for SensorML!")
		}
)


#
summary.SensorML = function(object, ...) {
	obj = list()
	
	# TODO implement method
	
	obj
}
setMethod("summary", "SensorML", summary.SensorML)

# saveXML(gc, file="/tmp/_testsave.xml")
setMethod("saveXML", "SensorML",
		function(doc, file=NULL, compression=0, indent=TRUE, prefix = '<?xml version="1.0"?>\n', doctype = NULL, encoding = "", ...) {
			saveXML(doc = encode(doc), file=file, compression=compression, indent=indent, prefix=prefix, doctype=doctype, encoding=encoding, ...)
		}
)
