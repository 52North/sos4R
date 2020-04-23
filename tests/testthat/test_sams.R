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
# Created: 2019-03-07                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
context("parsing: SF SpatialSampling")

sams1 <- '<sams:SF_SpatialSamplingFeature gml:id="ssf_88204F34D0B94590AA1EDE21577C9B5D907F4BAD"
    xmlns:gml="http://www.opengis.net/gml/3.2"
    xmlns:sam="http://www.opengis.net/sampling/2.0"
    xmlns:sams="http://www.opengis.net/samplingSpatial/2.0"
    xmlns:xlink="http://www.w3.org/1999/xlink">
  <gml:identifier codeSpace="http://www.opengis.net/def/nil/OGC/0/unknown">foi-1</gml:identifier>
  <gml:name codeSpace="http://www.opengis.net/def/nil/OGC/0/unknown">foi one</gml:name>
  <sam:type xlink:href="http://www.opengis.net/def/samplingFeatureType/OGC-OM/2.0/SF_SamplingPoint"/>
  <sam:sampledFeature xlink:href="http://www.52north.org/test/featureOfInterest/world"/>
  <sams:shape>
    <gml:Point gml:id="gml-id-p1">
      <gml:pos srsName="http://www.opengis.net/def/crs/EPSG/0/4326">51.883906 7.727958</gml:pos>
    </gml:Point>
  </sams:shape>
</sams:SF_SpatialSamplingFeature>'

testsos <- SOS_Test(name = "testsams", version = sos200_version)

test_that("id is parsed", {
  sams.doc <- xml2::xml_root(x = xml2::read_xml(x = sams1))
  samsSF <- parseSams200SamplingFeature(obj = sams.doc, sos = testsos)
  expect_equal(samsSF@id, "ssf_88204F34D0B94590AA1EDE21577C9B5D907F4BAD")
})

test_that("identifier is parsed", {
  sams.doc <- xml2::xml_root(x = xml2::read_xml(x = sams1))
  samsSF <- parseSams200SamplingFeature(obj = sams.doc, sos = testsos)
  expect_equal(samsSF@identifier, "foi-1")
  expect_equal(sosFeatureIds(samsSF), "foi-1")
})

test_that("name is parsed", {
  sams.doc <- xml2::xml_root(x = xml2::read_xml(x = sams1))
  samsSF <- parseSams200SamplingFeature(obj = sams.doc, sos = testsos)
  expect_equal(samsSF@name, "foi one")
})

test_that("type is parsed", {
  sams.doc <- xml2::xml_root(x = xml2::read_xml(x = sams1))
  samsSF <- parseSams200SamplingFeature(obj = sams.doc, sos = testsos)
  expect_equal(samsSF@type, "http://www.opengis.net/def/samplingFeatureType/OGC-OM/2.0/SF_SamplingPoint")
})

test_that("sampled feature is parsed", {
  sams.doc <- xml2::xml_root(x = xml2::read_xml(x = sams1))
  samsSF <- parseSams200SamplingFeature(obj = sams.doc, sos = testsos)
  expect_equal(samsSF@sampledFeature, "http://www.52north.org/test/featureOfInterest/world")
})

test_that("shape is parsed", {
  sams.doc <- xml2::xml_root(x = xml2::read_xml(x = sams1))
  samsSF <- parseSams200SamplingFeature(obj = sams.doc, sos = testsos)

  expect_s4_class(samsSF@shape, "SamsShape")
  expect_equal(sosCoordinates(samsSF), sosCoordinates(samsSF@shape))
  coords <- sosCoordinates(samsSF)
  expect_named(coords, c("lon", "lat", "SRS"))
  expect_lte(coords$lat - 51.88391, 0.000000000001)
  expect_lte(coords$lon - 7.727958, 0.000000000001)
  expect_equal(as.character(coords$SRS), "http://www.opengis.net/def/crs/EPSG/0/4326")
})
