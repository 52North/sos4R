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

################################################################################
# parsing tests

datafolder <- "/home/daniel/Dokumente/2010_SOS4R/workspace/sos4R/sandbox"

setwd(datafolder)
library("XML")


# read test file
caps.file = paste(datafolder, "Capabilities_excerpt.xml", sep="/")

# file can be an URL, see 'isURL'
doc = xmlTreeParse(caps.file)
class(doc)
structure(doc)

docRoot = xmlRoot(doc)
class(docRoot)
structure(docRoot)

# the document, acces the content and the dtd part
names(doc)
doc$doc

# play around with the document root node
docRoot[[1]] # fetch children by index: 1 = ServiceIdentification and so on (4 = filter caps)

docRoot[[1]][[1]] # ows:Title

# get the contents of the ServiceIdentification element children
xmlApply(docRoot[[1]], xmlValue)

# all the names of the elements
xmlApply(docRoot, xmlName)

# get the number of sub elements
xmlSize(docRoot)
# number of sub elements for all after root 
xmlApply(docRoot, xmlSize)

# get names of elements
xmlApply(docRoot, names)

# get attributes
xmlAttrs(docRoot)

# all XMLNodes...
class(docRoot[[1]])

# is an XMLTextNode ::== a leaf node
class(docRoot[[1]][[1]][[1]])
docRoot[[1]][[1]][[1]] # ClimateSOS

# get the text content
text = xmlValue(docRoot[[1]][[1]][[1]])
class(text) # character

# just strips all xml markup:
xmlValue(docRoot[[1]])

# indexing works!
names(docRoot[2:4])
names(docRoot[["ServiceIdentification"]])
names(docRoot[["Contents"]])

# 
caps.xmlString = doc$doc
isXMLString(caps.xmlString)
capsTree = xmlParseString(xmlString)
class(capsTree)

#  SERVICE PROVIDER
sp = '<ows:ServiceProvider xmlns:ogc="http://www.opengis.net/ogc" xmlns:ows="http://www.opengis.net/ows/1.1" xmlns:om="http://www.opengis.net/om/1.0" xmlns:swe="http://www.opengis.net/swe/1.0" xmlns:xlink="http://www.w3.org/1999/xlink">
		<ows:ProviderName>Insitute for Geoinformatics, Unversity Muenster, Germany</ows:ProviderName>
		<ows:ProviderSite xlink:hdef="http://ifgi.uni-muenster.de/"/>
		<ows:ServiceContact>
		<ows:IndividualName>Juerrens, Eike Hinderk</ows:IndividualName>
		<ows:PositionName>Student Assistant</ows:PositionName>
		<ows:ContactInfo>
		<ows:Phone>
		<ows:Voice>+49(0)251/83-31972</ows:Voice>
		</ows:Phone>
		<ows:Address>
		<ows:DeliveryPoint>Weselerstrasse 253</ows:DeliveryPoint>
		<ows:City>Muenster</ows:City>
		<ows:AdministrativeArea>North Rhine-Westphalia</ows:AdministrativeArea>
		<ows:PostalCode>48151</ows:PostalCode>
		<ows:Country>Germany</ows:Country>
		<ows:ElectronicMailAddress>ehjuerrens@uni-muenster.de</ows:ElectronicMailAddress>
		</ows:Address>
		</ows:ContactInfo>
		<ows:Role/>
		</ows:ServiceContact>
		</ows:ServiceProvider>'
sp.xml <- xmlParseString(sp)
sp.parsed <- parseOwsServiceProvider(sp.xml)
sp.parsed

# RANGES
range = '<ows:Range xmlns:ows="http://www.opengis.net/ows/1.1">
<ows:MinimumValue>2008-02-14T11:03:02.000+01:00</ows:MinimumValue>
<ows:MaximumValue>2010-07-12T12:45:00.000+02:00</ows:MaximumValue>
</ows:Range>'

range.xml <- xmlParseString(range)
range.parsed <- parseOwsRange(range.xml)
range.parsed

