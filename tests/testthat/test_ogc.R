############################################################################## #
# Copyright (C) 2019 by 52 North                                               #
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
# Created: 2019-03-27                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
context("encode filter: time")

testsos <- SOS_Test(name = "testogc")

test_that("during", {
  during <- TM_During(time = GmlTimePeriod(beginPosition = GmlTimePosition(time = parsedate::parse_iso_8601("2019-01-01")),
                                 endPosition = GmlTimePosition(time = parsedate::parse_iso_8601("2019-01-01"))))
  encoded <- encodeXML(obj = during, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(encoded), ">\\s*<", "><")

  expect_match(encodedString, "<ogc:PropertyName>om:samplingTime</ogc:PropertyName>")
  expect_match(encodedString, "2019-01-01T00:00:00\\+00:00</gml:endPosition></gml:TimePeriod></ogc:TM_During>")
})

test_that("bbox", {
  box <- OgcBBOX(envelope = GmlEnvelope(lowerCorner = GmlDirectPositionLatLon(1, 20),
                                        upperCorner = GmlDirectPositionLatLon(300, 4000)))
  encoded <- encodeXML(obj = box, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(encoded), ">\\s*<", "><")

  expect_match(encodedString, paste0(sosDefaultSpatialOpPropertyName, "</ogc:PropertyName>"))
  expect_match(encodedString, "300 4000</gml:upperCorner></gml:Envelope></ogc:BBOX>")
})

