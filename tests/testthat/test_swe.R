################################################################################
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
# Created: 2019-03-27                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
################################################################################
context("parsing: SWE position")

testsos <- SOS_Test(name = "testswe")

positionString <- '<swe:Position fixed="false" referenceFrame="urn:ogc:def:crs:EPSG::4326" xmlns:swe="http://www.opengis.net/swe/1.0.1">
	<swe:location>
		<swe:Vector>
			<swe:coordinate name="northing">
				<swe:Quantity axisID="y">
					<swe:uom code="degree"/>
					<swe:value>51.447722</swe:value>
				</swe:Quantity>
			</swe:coordinate>
			<swe:coordinate name="easting">
				<swe:Quantity axisID="x">
					<swe:uom code="degree"/>
					<swe:value>7.270806</swe:value>
				</swe:Quantity>
			</swe:coordinate>
			<swe:coordinate name="altitude">
				<swe:Quantity axisID="z">
					<swe:uom code="m"/>
					<swe:value>52.0</swe:value>
				</swe:Quantity>
			</swe:coordinate>
		</swe:Vector>
	</swe:location>
</swe:Position>'

test_that("reference frame", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = positionString))
  position <- parseSwePosition(obj = doc, sos = testsos)
  expect_equal(attr(position, "referenceFrame"), "urn:ogc:def:crs:EPSG::4326")
})

test_that("all coordinates", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = positionString))
  position <- parseSwePosition(obj = doc, sos = testsos)
  expect_length(position, 3)
})

test_that("coordinate names are the axisID", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = positionString))
  vector <- parseSweVector(obj = xml2::xml_find_first(doc, "//swe:Vector"), sos = testsos)
  expect_length(vector, 3)
  expect_named(vector, c("y", "x", "z"))
})

context("parsing: SWE position")

coordinateString <- '<swe:coordinate name="altitude" xmlns:swe="http://www.opengis.net/swe/1.0.1">
	<swe:Quantity axisID="z">
		<swe:uom code="m"/>
		<swe:value>52.42</swe:value>
	</swe:Quantity>
</swe:coordinate>'

test_that("name is parsed", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = coordinateString))
  coordinate <- parseSweCoordinate(obj = doc, sos = testsos)
  expect_equal(coordinate$name, "altitude")
})

test_that("axis ID is parsed", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = coordinateString))
  coordinate <- parseSweCoordinate(obj = doc, sos = testsos)
  expect_equal(coordinate$axisID, "z")
})

test_that("UOM code is parsed", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = coordinateString))
  coordinate <- parseSweCoordinate(obj = doc, sos = testsos)
  expect_equal(coordinate$uomCode, "m")
})

test_that("value is parsed", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = coordinateString))
  coordinate <- parseSweCoordinate(obj = doc, sos = testsos)
  expect_equal(coordinate$value, 52.42)
})
