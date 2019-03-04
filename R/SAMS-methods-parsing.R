################################################################################
# Copyright (C) 2016 by 52 North                                               #
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
# Author: Benjamin Pross (b.pross@52north.org),                                #
#         Jürrens, Eike Hinderk (e.h.juerrens@52north.org)                     #
# Created: 2016-01-27                                                          #
# Project: sos4R - visit the project web page,                                 #
#      http://52north.org/communities/sensorweb/clients/sos4R/                 #
################################################################################

#
# parseSamsShape ----
#
parseSamsShape <- function(obj, sos) {
  #TODO other shape types, check syntax [[1]]
  .point <- parsePoint(obj[[1]][gmlPointName][[1]], sos = sos)

  SamsShape(point = .point)
}

#
# parseSams200SamplingFeature ----
#
# <sams:SF_SpatialSamplingFeature gml:id="ssf_88204F34D0B94590AA1EDE21577C9B5D907F4BAD">
#   <gml:identifier codeSpace="http://www.opengis.net/def/nil/OGC/0/unknown">foi-1</gml:identifier>
#   <gml:name codeSpace="http://www.opengis.net/def/nil/OGC/0/unknown">foi one</gml:name>
#   <sf:type xlink:href="http://www.opengis.net/def/samplingFeatureType/OGC-OM/2.0/SF_SamplingPoint"/>
#   <sf:sampledFeature xlink:href="http://www.52north.org/test/featureOfInterest/world"/>
#   <sams:shape>
#     <ns:Point xmlns:ns="http://www.opengis.net/gml/3.2" ns:id="gml-id-p1">
#       <ns:pos srsName="http://www.opengis.net/def/crs/EPSG/0/4326">51.883906 7.727958</ns:pos>
#     </ns:Point>
#   </sams:shape>
# </sams:SF_SpatialSamplingFeature>
#
# TODO add test for parsing!
#
parseSams200SamplingFeature <- function(obj, sos) {
  .gmlid <- xmlAttrs(node = obj)[["id"]]
  # TODO should we add checks here, e.g. if the element is really available, is only one element in the list?
  # TODO Implement tuple with codespace and value for identifier and name, if required
  .identifier <- XML::xmlValue(x = XML::xmlElementsByTagName(el = obj, name = "identifier")[[1]])
  .name <- XML::xmlValue(x = XML::xmlElementsByTagName(el = obj, name = "name")[[1]])
  .type <- xmlAttrs(node = XML::xmlElementsByTagName(el = obj, name = "type")[[1]])[["href"]]
  .sampledFeature <- xmlAttrs(node = XML::xmlElementsByTagName(el = obj, name = "sampledFeature")[[1]])[["href"]]
  .shape <- parseSamsShape(obj = XML::xmlElementsByTagName(el = obj, name = "shape"), sos = sos)
  SamsSamplingFeature(id = .gmlid,
                      identifier = .identifier,
                      name = .name,
                      type = .type,
                      sampledFeature = .sampledFeature,
                      shape = .shape)
}
