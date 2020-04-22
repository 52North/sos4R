############################################################################## #
# Copyright (C) 2019 by 52°North                                               #
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
# Created: 2019-03-28                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
context("parsing: sampling point")

samplingPointXml <- '<sa:SamplingPoint gml:id="sf_8D0EA55680FEC1646E1A01C441D7220F9BD9F57C" xmlns:gml="http://www.opengis.net/gml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:sa="http://www.opengis.net/sampling/1.0">
<gml:name codeSpace="uniquID">http://www.52north.org/test/featureOfInterest/6</gml:name>
<gml:name codeSpace="http://www.opengis.net/def/nil/OGC/0/unknown">Hochschule Bochum</gml:name>
<sa:sampledFeature xlink:href="http://www.52north.org/test/featureOfInterest/world" xlink:title="Hochschule Bochum"/>
<sa:position>
<gml:Point gml:id="point_sf_8D0EA55680FEC1646E1A01C441D7220F9BD9F57C">
<gml:pos srsName="urn:ogc:def:crs:EPSG::4326">51.447722 7.270806</gml:pos>
</gml:Point>
</sa:position>
</sa:SamplingPoint>'

testsos <- SOS_Test(name = "testsa")

test_that("point is parsed", {
  doc <- xml2::read_xml(x = samplingPointXml)
  point <- parseSamplingPoint(obj = doc, sos = testsos)

  coords <- sosCoordinates(point)
  expect_equal(coords$feature, "sf_8D0EA55680FEC1646E1A01C441D7220F9BD9F57C")
  expect_equal(coords$lat, 51.447722)
  expect_equal(coords$lon, 7.270806)
  expect_equal(as.character(coords$SRS), "urn:ogc:def:crs:EPSG::4326")
})
