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
#         Eike Hinderk Jürrens (e.h.juerrens@52north.org)                      #
# Created: 2019-03-27                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
context("parsing: SWE position")
#
#
#
testsos <- SOS_Test(name = "testswe")
#
#
#
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
#
# test parsing: SWE position::reference frame ----
#
test_that("reference frame", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = positionString))
  position <- parseSwePosition(obj = doc, sos = testsos)
  expect_equal(attr(position, "referenceFrame"), "urn:ogc:def:crs:EPSG::4326")
})
#
# test parsing: SWE position::all coordinates ----
#
test_that("all coordinates", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = positionString))
  position <- parseSwePosition(obj = doc, sos = testsos)
  expect_length(position, 3)
})
#
# test parsing: SWE position::coordinate names are the axisID ----
#
test_that("coordinate names are the axisID", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = positionString))
  vector <- parseSweVector(obj = xml2::xml_find_first(doc, "//swe:Vector"), sos = testsos)
  expect_length(vector, 3)
  expect_named(vector, c("y", "x", "z"))
})
#
#
#
context("parsing: SWE coordinate")
#
#
#
coordinateString <- '<swe:coordinate name="altitude" xmlns:swe="http://www.opengis.net/swe/1.0.1">
	<swe:Quantity axisID="z">
		<swe:uom code="m"/>
		<swe:value>52.42</swe:value>
	</swe:Quantity>
</swe:coordinate>'
#
# test parsing: SWE coordinate::name is parsed ----
#
test_that("name is parsed", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = coordinateString))
  coordinate <- parseSweCoordinate(obj = doc, sos = testsos)
  expect_equal(coordinate$name, "altitude")
})
#
# test parsing: SWE coordinate::axis ID is parsed ----
#
test_that("axis ID is parsed", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = coordinateString))
  coordinate <- parseSweCoordinate(obj = doc, sos = testsos)
  expect_equal(coordinate$axisID, "z")
})
#
# test parsing: SWE coordinate::UOM code is parsed ----
#
test_that("UOM code is parsed", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = coordinateString))
  coordinate <- parseSweCoordinate(obj = doc, sos = testsos)
  expect_equal(coordinate$uomCode, "m")
})
#
# test parsing: SWE coordinate::value is parsed ----
#
test_that("value is parsed", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = coordinateString))
  coordinate <- parseSweCoordinate(obj = doc, sos = testsos)
  expect_equal(coordinate$value, 52.42)
})
#
#
#
context("parsing: SWE fields")
#
# test data
#
fieldsString <- '<swe:DataRecord
                    xmlns:swe="http://www.opengis.net/swe/1.0.1"
                    xmlns:xlink="http://www.w3.org/1999/xlink">
                  <swe:field name="field_1_name">
                      <swe:Quantity definition="field_1_definition">
                          <swe:uom code="field_1_uom"/>
                      </swe:Quantity>
                  </swe:field>
                  <swe:field name="field_2_name">
                      <swe:Category name="category_name" definition="field_2_definition">
                          <swe:codeSpace xlink:href="category_codespace"/>
                      </swe:Category>
                  </swe:field>
                  <swe:field name="field_3_name">
                      <swe:Count definition="field_3_definition"/>
                  </swe:field>
                  <swe:field name="field_4_name">
                      <swe:Text definition="field_4_definition"/>
                  </swe:field>
                  <swe:field name="field_5_name">
                      <swe:Boolean definition="field_5_definition"/>
                  </swe:field>
                  <swe:field name="field_6_name">
                      <swe:Time definition="field_6_definition">
                          <swe:uom xlink:href="field_6_uom"/>
                      </swe:Time>
                  </swe:field>
                </swe:DataRecord>'
#
# test parsing: SWE fields::name ----
#
test_that("name is parsed", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = fieldsString))
  xmlFields <- xml2::xml_children(doc)
  fields <- lapply(xmlFields, parseField, testsos)
  expect_length(fields, 6)
  expect_equal(fields[[1]][["name"]], "field_1_name")
  expect_equal(fields[[2]][["name"]], "field_2_name")
  expect_equal(fields[[3]][["name"]], "field_3_name")
  expect_equal(fields[[4]][["name"]], "field_4_name")
  expect_equal(fields[[5]][["name"]], "field_5_name")
  expect_equal(fields[[6]][["name"]], "field_6_name")
})
#
# test parsing: SWE fields::definition ----
#
test_that("definition is parsed", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = fieldsString))
  xmlFields <- xml2::xml_children(doc)
  fields <- lapply(xmlFields, parseField, testsos)
  expect_equal(fields[[1]][["definition"]], "field_1_definition")
  expect_equal(fields[[2]][["definition"]], "field_2_definition")
  expect_equal(fields[[3]][["definition"]], "field_3_definition")
  expect_equal(fields[[4]][["definition"]], "field_4_definition")
  expect_equal(fields[[5]][["definition"]], "field_5_definition")
  expect_equal(fields[[6]][["definition"]], "field_6_definition")
})
#
# test parsing: SWE fields::quantity/time->uom code ----
#
test_that("quantity/time->uom code is parsed", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = fieldsString))
  xmlFields <- xml2::xml_children(doc)
  fields <- lapply(xmlFields, parseField, testsos)
  expect_equal(fields[[1]][["uom"]], "field_1_uom")
  expect_error(fields[[2]][["uom"]], "subscript out of bounds")
  expect_error(fields[[3]][["uom"]], "subscript out of bounds")
  expect_error(fields[[4]][["uom"]], "subscript out of bounds")
  expect_error(fields[[5]][["uom"]], "subscript out of bounds")
  expect_equal(fields[[6]][["uom"]], "field_6_uom")
})
#
# test parsing: SWE fields::category->category, codespace ----
#
test_that("quantity->category, codespace are parsed", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = fieldsString))
  xmlFields <- xml2::xml_children(doc)
  fields <- lapply(xmlFields, parseField, testsos)
  expect_error(fields[[1]][["category"]], "subscript out of bounds")
  expect_equal(fields[[2]][["category"]], "category_name")
  expect_error(fields[[3]][["category"]], "subscript out of bounds")
  expect_error(fields[[4]][["category"]], "subscript out of bounds")
  expect_error(fields[[5]][["category"]], "subscript out of bounds")
  expect_error(fields[[6]][["category"]], "subscript out of bounds")

  expect_error(fields[[1]][["codeSpace"]], "subscript out of bounds")
  expect_equal(fields[[2]][["codeSpace"]], "category_codespace")
  expect_error(fields[[3]][["codeSpace"]], "subscript out of bounds")
  expect_error(fields[[4]][["codeSpace"]], "subscript out of bounds")
  expect_error(fields[[5]][["codeSpace"]], "subscript out of bounds")
  expect_error(fields[[6]][["codeSpace"]], "subscript out of bounds")
})
#
# test parsing: SWE fields::rClass ----
#
test_that("rClass is set correctly", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = fieldsString))
  xmlFields <- xml2::xml_children(doc)
  fields <- lapply(xmlFields, parseField, testsos)
  expect_equal(fields[[1]][["rClass"]], "numeric")
  expect_equal(fields[[2]][["rClass"]], "factor")
  expect_equal(fields[[3]][["rClass"]], "integer")
  expect_equal(fields[[4]][["rClass"]], "character")
  expect_equal(fields[[5]][["rClass"]], "logical")
  expect_equal(fields[[6]][["rClass"]], "POSIXct")
})
#
#
#
context("parsing: SWE values")
#
# test parsing: SWE values::quantity ----
#
test_that("quantity values are parsed", {
  values <- xml2::read_xml('<swe:values xmlns:swe="http://www.opengis.net/swe/1.0.1">25.0,24.5,24.0</swe:values>')
  fields <- list(c(name = "field_1_name", uom = "field_1_uom", definition = "field_1_definition", rClass = "numeric"))
  encoding <- SweTextEncoding(";", ",", ".")
  parsedValues <- parseValues(values = values, fields = fields, encoding = encoding, sos = testsos)
  expect_true(is.data.frame(parsedValues))
  expect_true(nrow(parsedValues) == 3)
  expect_length(colnames(parsedValues), 1)
  expect_equal(colnames(parsedValues)[[1]], "field_1_name")
  expect_true(is.double(parsedValues[[1,1]]))
  expect_true(is.double(parsedValues[[2,1]]))
  expect_true(is.double(parsedValues[[3,1]]))
  expect_false(is.integer(parsedValues[[1,1]]))
  expect_false(is.integer(parsedValues[[2,1]]))
  expect_false(is.integer(parsedValues[[3,1]]))
  expect_true(parsedValues[[1,1]] == 25.0)
  expect_true(parsedValues[[2,1]] == 24.5)
  expect_true(parsedValues[[3,1]] == 24.0)
})
#
# test parsing: SWE values::count ----
#
test_that("count values are parsed", {
  values <- xml2::read_xml('<swe:values xmlns:swe="http://www.opengis.net/swe/1.0.1">25,24,23</swe:values>')
  fields <- list(c(name = "field_1_name", definition = "field_1_definition", rClass = "integer"))
  encoding <- SweTextEncoding(";", ",", ".")
  parsedValues <- parseValues(values = values, fields = fields, encoding = encoding, sos = testsos)
  expect_true(is.data.frame(parsedValues))
  expect_true(nrow(parsedValues) == 3)
  expect_length(colnames(parsedValues), 1)
  expect_equal(colnames(parsedValues)[[1]], "field_1_name")
  expect_true(is.integer(parsedValues[[1,1]]))
  expect_true(is.integer(parsedValues[[2,1]]))
  expect_true(is.integer(parsedValues[[3,1]]))
  expect_false(is.double(parsedValues[[1,1]]))
  expect_false(is.double(parsedValues[[2,1]]))
  expect_false(is.double(parsedValues[[3,1]]))
  expect_true(parsedValues[[1,1]] == 25)
  expect_true(parsedValues[[2,1]] == 24)
  expect_true(parsedValues[[3,1]] == 23)
})
#
# test parsing: SWE values::boolean ----
#
test_that("boolean values are parsed", {
  values <- xml2::read_xml('<swe:values xmlns:swe="http://www.opengis.net/swe/1.0.1">true,false</swe:values>')
  fields <- list(c(name = "field_1_name", definition = "field_1_definition", rClass = "logical"))
  encoding <- SweTextEncoding(";", ",", ".")
  parsedValues <- parseValues(values = values, fields = fields, encoding = encoding, sos = testsos)
  expect_true(is.data.frame(parsedValues))
  expect_true(nrow(parsedValues) == 2)
  expect_length(colnames(parsedValues), 1)
  expect_equal(colnames(parsedValues)[[1]], "field_1_name")
  expect_true(is.logical(parsedValues[[1,1]]))
  expect_true(is.logical(parsedValues[[2,1]]))
  expect_true(parsedValues[[1,1]] == TRUE)
  expect_true(parsedValues[[2,1]] == FALSE)
})
#
# test parsing: SWE values::text ----
#
test_that("text values are parsed", {
  values <- xml2::read_xml('<swe:values xmlns:swe="http://www.opengis.net/swe/1.0.1">text,and more text here</swe:values>')
  fields <- list(c(name = "field_1_name", definition = "field_1_definition", rClass = "character"))
  encoding <- SweTextEncoding(";", ",", ".")
  parsedValues <- parseValues(values = values, fields = fields, encoding = encoding, sos = testsos)
  expect_true(is.data.frame(parsedValues))
  expect_true(nrow(parsedValues) == 2)
  expect_length(colnames(parsedValues), 1)
  expect_equal(colnames(parsedValues)[[1]], "field_1_name")
  expect_true(is.character(parsedValues[[1,1]]))
  expect_true(is.character(parsedValues[[2,1]]))
  expect_true(parsedValues[[1,1]] == "text")
  expect_true(parsedValues[[2,1]] == "and more text here")
})
#
# test parsing: SWE values::category ----
#
test_that("category values are parsed", {
  values <- xml2::read_xml('<swe:values xmlns:swe="http://www.opengis.net/swe/1.0.1">text,and more text here</swe:values>')
  fields <- list(c(name = "field_1_name", definition = "field_1_definition", rClass = "factor", uom = "field_1_uom", codeSpace = "field_1_codespace"))
  encoding <- SweTextEncoding(";", ",", ".")
  parsedValues <- parseValues(values = values, fields = fields, encoding = encoding, sos = testsos)
  expect_true(is.data.frame(parsedValues))
  expect_true(nrow(parsedValues) == 2)
  expect_length(colnames(parsedValues), 1)
  expect_equal(colnames(parsedValues)[[1]], "field_1_name")
  expect_true(is.factor(parsedValues[[1,1]]))
  expect_true(is.factor(parsedValues[[2,1]]))
  expect_true(parsedValues[[1,1]] == "text")
  expect_true(parsedValues[[2,1]] == "and more text here")
})
#
# test parsing: SWE values::time ----
#
test_that("time values are parsed", {
  values <- xml2::read_xml('<swe:values xmlns:swe="http://www.opengis.net/swe/1.0.1">2019-06-18,2019-05-18</swe:values>')
  fields <- list(c(name = "field_1_name", definition = "field_1_definition", rClass = "POSIXct"))
  encoding <- SweTextEncoding(";", ",", ".")
  parsedValues <- parseValues(values = values, fields = fields, encoding = encoding, sos = testsos)
  expect_true(is.data.frame(parsedValues))
  expect_true(nrow(parsedValues) == 2)
  expect_length(colnames(parsedValues), 1)
  expect_equal(colnames(parsedValues)[[1]], "field_1_name")
  expect_s3_class(parsedValues[[1,1]], "POSIXct")
  expect_s3_class(parsedValues[[2,1]], "POSIXct")
  expect_true(parsedValues[[1,1]] == parsedate::parse_iso_8601("2019-06-18"))
  expect_true(parsedValues[[2,1]] == parsedate::parse_iso_8601("2019-05-18"))
})
#
# test parsing: SWE fields::time without uom ----
#
#
# test data
#
fieldsString <- '<swe:DataRecord
                    xmlns:swe="http://www.opengis.net/swe/1.0.1"
                    xmlns:xlink="http://www.w3.org/1999/xlink">
                  <swe:field name="field_6_name">
                      <swe:Time definition="field_6_definition" />
                  </swe:field>
                </swe:DataRecord>'
test_that("time without uom has uom NA", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = fieldsString))
  xmlFields <- xml2::xml_children(doc)
  fields <- lapply(xmlFields, parseField, testsos)
  expect_true(is.na(fields[[1]][["uom"]]))
})