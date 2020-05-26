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
# Created: 2019-03-20                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
context("parsing: observation collection")

testsos <- SOS_Test(name = "testobscoll")

test_that("bounded by is parsed from collection", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml")
  expect_warning(collection <- parseObservationCollection(obj = doc, sos = testsos), "Error converting string")
  expect_false(is.null(collection@boundedBy))
  expect_named(collection@boundedBy, c("srsName", "lowerCorner", "upperCorner"))
  expect_equal(collection@boundedBy$srsName, "http://www.opengis.net/def/crs/EPSG/0/4326")
})

test_that("all members are parsed from collection", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml")
  expect_warning(collection <- parseObservationCollection(obj = doc, sos = testsos), "Error converting string")
  expect_equal(length(collection), 2)
  expect_equal(length(collection@members), 2)
})

test_that("all collection members have the same result columns", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml")
  expect_warning(collection <- parseObservationCollection(obj = doc, sos = testsos), "Error converting string")
  expect_equal(names(collection[[1]]@result), names(collection[[2]]@result))
})

test_that("name available for first observation, but not for second", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml")
  expect_warning(collection <- parseObservationCollection(obj = doc, sos = testsos), "Error converting string")
  expect_named(collection@members, c("1234", "Observation"))
})

test_that("procedures are parsed from both members", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml")
  expect_warning(collection <- parseObservationCollection(obj = doc, sos = testsos), "Error converting string")
  expect_equal(collection[[1]]@procedure, "urn:ogc:object:Sensor:MyOrg:12349")
  expect_equal(collection[[2]]@procedure, "urn:ogc:object:Sensor:MyOrg:12350")
})

test_that("all fields are parsed from both members", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml")
  expect_warning(collection <- parseObservationCollection(obj = doc, sos = testsos), "Error converting string")
  expect_named(collection[[1]]@result, c("time", "longitude", "latitude", "DPM", "MS"))
  expect_named(collection[[2]]@result, c("time", "longitude", "latitude", "DPM", "MS"))
})

emptyCollection <- '<om:ObservationCollection id="oc_1553941031703" xmlns:om="http://www.opengis.net/om/1.0" xmlns:gml="http://www.opengis.net/gml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink">
<om:member xlink:href="urn:ogc:def:nil:OGC:inapplicable"/>
</om:ObservationCollection>'

testsos <- SOS_Test(name = "testobscoll")

test_that("can handle empty observation collection", {
  doc <- xml2::read_xml(x = emptyCollection)
  expect_warning(
    obs <- parseObservationCollection(obj = doc, sos = testsos),
    "urn:ogc:def:nil:OGC:inapplicable"
  )

  expect_true(is.list(obs@members))
  expect_s4_class(obs@members[[1]], "OmObservationProperty")
  expect_named(obs@members, c("member"))
  expect_null(obs@members[[1]]@obs)
  expect_equal(obs@members[[1]]@href, "urn:ogc:def:nil:OGC:inapplicable")
})


context("parsing: DataArray")

test_that("parse encoding", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml")
  encodingXml <- xml2::xml_find_first(x = doc, xpath = "//swe:encoding")

  encoding <- parseEncoding(obj = encodingXml, sos = testsos)
  expect_s4_class(encoding, "SweTextBlock")
  expect_equal(encoding@tokenSeparator, ",")
  expect_equal(encoding@decimalSeparator, ".")
  expect_equal(encoding@blockSeparator, "@@")
})

test_that("parse time field", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml")
  fieldsXml <- xml2::xml_find_all(x = doc, xpath = "//swe:field")

  timeField <- parseField(obj = fieldsXml[[1]], sos = testsos)
  expect_equal(timeField[["name"]], "time")
  expect_equal(timeField[["definition"]], "urn:ogc:property:time:iso8601")
})

test_that("parse quantity field", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml")
  fieldsXml <- xml2::xml_find_all(x = doc, xpath = "//swe:field")

  quantityField <- parseField(obj = fieldsXml[[2]], sos = testsos)

  expect_equal(quantityField[["name"]], "longitude")
  expect_equal(quantityField[["definition"]], "urn:ogc:property:location:EPSG:4326:longitude")
  expect_equal(quantityField[["uom"]], "deg")
})

test_that("parse category field", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml")
  fieldsXml <- xml2::xml_find_all(x = doc, xpath = "//swe:field")

  categoryField <- parseField(obj = fieldsXml[[5]], sos = testsos)
  expect_equal(categoryField[["name"]], "MS")
  expect_equal(categoryField[["definition"]], "http://www.52north.org/test/observableProperty/6")
  expect_equal(categoryField[["codeSpace"]], "test_unit_6")
})

test_that("parse data array finds all data", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml")
  dataArrayXml <- xml2::xml_find_first(x = doc, xpath = "//swe:DataArray")
  dataArray <- parseDataArray(obj = dataArrayXml, sos = testsos)

  expect_s3_class(dataArray, "data.frame")
  expect_named(dataArray, c("time", "longitude", "latitude", "DPM", "MS"))
  expect_length(dataArray, 5)
})

test_that("parse data array has correct classes for fields", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml")
  dataArrayXml <- xml2::xml_find_first(x = doc, xpath = "//swe:DataArray")
  dataArray <- parseDataArray(obj = dataArrayXml, sos = testsos)

  # TODO uncomment test depending on our environment ;-)
  expect_equivalent(lapply(dataArray, class), list(c("POSIXct", "POSIXt"), "numeric", "numeric", "numeric", "factor"))
  #expect_equivalent(lapply(dataArray, class), list(c("POSIXct", "POSIXt"), "numeric", "numeric", "numeric", "character"))
})

context("parsing: composite phenomenon")

compositePhenomenon <- '<swe:CompositePhenomenon xmlns:gml="http://www.opengis.net/gml"
xmlns:swe="http://www.opengis.net/swe/1.0.1" xmlns:xlink="http://www.w3.org/1999/xlink"
gml:id="WaterQuality" dimension="4">
<gml:name>WaterQuality</gml:name>
<swe:component xlink:href="urn:ogc:def:property:OGC-SWE:1:ID"/>
<swe:component xlink:href="urn:ogc:def:property:OGC-SWE:2:ID"/>
</swe:CompositePhenomenon>'

testsos <- SOS_Test(name = "testcompphen")
axiomCaps <- parseSosCapabilities(xml2::read_xml(x = "../responses/Capabilities_52N-SOS_Axiom.xml"), testsos)
testsos@capabilities <- axiomCaps

test_that("composite phenomenon name is parsed from snippet", {
  doc <- xml2::read_xml(x = compositePhenomenon)
  phen <- parseCompositePhenomenon(obj = doc, sos = testsos)
  expect_equal(phen@name, "WaterQuality")
})

test_that("composite phenomenon id is parsed from snippet", {
  doc <- xml2::read_xml(x = compositePhenomenon)
  phen <- parseCompositePhenomenon(obj = doc, sos = testsos)
  expect_equal(phen@id, "WaterQuality")
})

test_that("composite phenomenon dimension is parsed from snippet", {
  doc <- xml2::read_xml(x = compositePhenomenon)
  phen <- parseCompositePhenomenon(obj = doc, sos = testsos)
  expect_equal(phen@dimension, 4)
})

test_that("composite phenomenon components are parsed from snippet", {
  doc <- xml2::read_xml(x = compositePhenomenon)
  phen <- parseCompositePhenomenon(obj = doc, sos = testsos)
  expect_equal(length(phen@components), 2)
  expect_equal(phen@components[[2]]@href, "urn:ogc:def:property:OGC-SWE:2:ID")
})

context("parsing: time")

samplingTimeInstantXml <- '<om:samplingTime xmlns:om="http://www.opengis.net/om/1.0" xmlns:gml="http://www.opengis.net/gml">
    <gml:TimeInstant>
      <gml:timePosition>2005-08-05T12:21:13Z</gml:timePosition>
    </gml:TimeInstant>
  </om:samplingTime>'

test_that("time instant", {
  doc <- xml2::read_xml(x = samplingTimeInstantXml)
  samplingTime <- parseTime(obj = doc, sos = testsos)
  expect_equal(strftime(samplingTime@timePosition@time, format = "%Y%d"), "200505")
  expect_s3_class(sosTime(samplingTime), "POSIXt")
})

test_that("warning on non-matching time format", {
  doc <- xml2::read_xml(x = '<om:samplingTime xmlns:om="http://www.opengis.net/om/1.0" xmlns:gml="http://www.opengis.net/gml">
    <gml:TimeInstant>
      <gml:timePosition>2005-08T12:13+24:00</gml:timePosition>
    </gml:TimeInstant>
  </om:samplingTime>')
  expect_warning(samplingTime <- parseTime(obj = doc, sos = testsos), "Error converting string '2005-08T12:13\\+24:00' to 'ISO8601' format! Returning 'NA'")
  expect_true(is.na(sosTime(samplingTime)))
  expect_s3_class(sosTime(samplingTime), "POSIXt")
  expect_s4_class(samplingTime, "GmlTimeObjectOrNULL")
})

samplingTimePeriodXml <- '<om:samplingTime xmlns:om="http://www.opengis.net/om/1.0" xmlns:gml="http://www.opengis.net/gml">
    <gml:TimePeriod gml:id="phenomenonTime_1">
				<gml:beginPosition>2015-05-18T08:35:00.000Z</gml:beginPosition>
				<gml:endPosition>2015-12-02T12:00:00.000Z</gml:endPosition>
			</gml:TimePeriod>
  </om:samplingTime>'

test_that("time period", {
  doc <- xml2::read_xml(x = samplingTimePeriodXml)
  samplingTime <- parseTime(obj = doc, sos = testsos)
  expect_equal(samplingTime@id, "phenomenonTime_1")
  expect_s4_class(samplingTime@beginPosition, "GmlTimePosition")
  expect_s4_class(samplingTime@endPosition, "GmlTimePosition")
})

samplingTimeReferenceXml <- '<om:samplingTime xmlns:om="http://www.opengis.net/om/1.0" xlink:href="#abc" xmlns:xlink="http://www.w3.org/1999/xlink" />'

test_that("time reference", {
  doc <- xml2::read_xml(x = samplingTimeReferenceXml)
  samplingTime <- parseTime(obj = doc, sos = testsos)
  expect_equal(samplingTime@href, "#abc")
})

observationXml <- '<om:Observation gml:id="o_1553776324284" xmlns:om="http://www.opengis.net/om/1.0" xmlns:gml="http://www.opengis.net/gml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:sa="http://www.opengis.net/sampling/1.0" xmlns:swe="http://www.opengis.net/swe/1.0.1">
      <om:samplingTime>
<gml:TimePeriod gml:id="phenomenonTime_172">
<gml:beginPosition>2012-11-19T13:01:00.000Z</gml:beginPosition>
<gml:endPosition>2012-11-19T13:09:00.000Z</gml:endPosition>
</gml:TimePeriod>
</om:samplingTime>
<om:resultTime>
<gml:TimeInstant gml:id="resultTime_172">
<gml:timePosition>2012-11-19T13:01:00.000Z</gml:timePosition>
</gml:TimeInstant>
</om:resultTime>
<om:procedure xlink:href="http://www.52north.org/test/procedure/6"/>
<om:observedProperty xlink:href="http://www.52north.org/test/observableProperty/6"/>
<om:featureOfInterest>
<sa:SamplingPoint gml:id="sf_8D0EA55680FEC1646E1A01C441D7220F9BD9F57C">
<gml:name codeSpace="uniquID">http://www.52north.org/test/featureOfInterest/6</gml:name>
<gml:name codeSpace="http://www.opengis.net/def/nil/OGC/0/unknown">Hochschule Bochum</gml:name>
<sa:sampledFeature xlink:href="http://www.52north.org/test/featureOfInterest/world" xlink:title="Hochschule Bochum"/>
<sa:position>
<gml:Point gml:id="point_sf_8D0EA55680FEC1646E1A01C441D7220F9BD9F57C">
<gml:pos srsName="urn:ogc:def:crs:EPSG::4326">51.447722 7.270806</gml:pos>
</gml:Point>
</sa:position>
</sa:SamplingPoint>
</om:featureOfInterest>
<om:result>
<swe:DataArray>
<swe:elementCount>
<swe:Count>
<swe:value>9</swe:value>
</swe:Count>
</swe:elementCount>
<swe:elementType name="Components">
<swe:DataRecord xmlns:ns="http://www.opengis.net/swe/1.0.1">
<ns:field name="phenomenonTime">
<ns:Time definition="http://www.opengis.net/def/property/OGC/0/PhenomenonTime">
<ns:uom xlink:href="http://www.opengis.net/def/uom/ISO-8601/0/Gregorian"/>
</ns:Time>
</ns:field>
<ns:field name="http://www.52north.org/test/observableProperty/6">
<ns:Quantity definition="http://www.52north.org/test/observableProperty/6">
<ns:uom code="ppm"/>
</ns:Quantity>
</ns:field>
</swe:DataRecord>
</swe:elementType>
<swe:encoding>
<swe:TextBlock blockSeparator="@@" decimalSeparator="." tokenSeparator=","/>
</swe:encoding>
<swe:values>2012-11-19T13:01:00.000Z,2.1@@2012-11-19T13:02:00.000Z,2.2@@2012-11-19T13:03:00.000Z,2.3@@2012-11-19T13:04:00.000Z,2.4@@2012-11-19T13:05:00.000Z,2.5@@2012-11-19T13:06:00.000Z,2.6@@2012-11-19T13:07:00.000Z,2.7@@2012-11-19T13:08:00.000Z,2.8@@2012-11-19T13:09:00.000Z,2.9</swe:values>
</swe:DataArray>
</om:result>
</om:Observation>'

test_that("warning if time cannot be parsed", {
  badTimestamps <- stringr::str_replace_all(string = observationXml,
                                            pattern = "2012-11-19T13:02:00.000Z",
                                            replacement =  "2012-11-1913:02:00.000Z")
  badTimestamps <- stringr::str_replace_all(string = badTimestamps,
                                            pattern = "2012-11-19T13:09",
                                            replacement =  "11-19T13:09")
  doc <- xml2::read_xml(x = badTimestamps)
  expect_warning(obs <- parseObservation(obj = doc, sos = testsos), "Error converting string")
  data <- sosResult(obs)
  expect_equal(which(is.na(data$phenomenonTime)), c(2,length(data$phenomenonTime)))
})

test_that("correct class is returned", {
  doc <- xml2::read_xml(x = observationXml)
  obs <- parseObservation(obj = doc, sos = testsos)
  expect_s4_class(obs, "OmObservation")
})

test_that("coordinates are parsed from observation", {
  doc <- xml2::read_xml(x = observationXml)
  obs <- parseObservation(obj = doc, sos = testsos)
  coords <- sosCoordinates(obs)

  expect_equal(coords$lat, 51.447722)
  expect_equal(coords$lon, 7.270806)
  expect_equal(as.character(coords$SRS), "urn:ogc:def:crs:EPSG::4326")
})

test_that("result parsed from observation", {
  doc <- xml2::read_xml(x = observationXml)
  obs <- parseObservation(obj = doc, sos = testsos)
  result <- sosResult(obs)

  expect_equal(dim(result), c(9, 2))
  expect_equal(result[7,2], 2.7)
  expect_named(result, c("phenomenonTime", "http://www.52north.org/test/observableProperty/6"))
  expect_s3_class(result[,1], "POSIXt")
})

test_that("observation metadata is in result", {
  doc <- xml2::read_xml(x = observationXml)
  obs <- parseObservation(obj = doc, sos = testsos)
  result <- sosResult(obs)

  expect_named(attributes(result[,2]), c("name", "definition", "unit of measurement", "R class of values"))
  expect_equivalent(sosUOM(result), c("http://www.opengis.net/def/uom/ISO-8601/0/Gregorian", "ppm"))
  expect_named(sosUOM(result), c("phenomenonTime", "http://www.52north.org/test/observableProperty/6"))
})

test_that("feature ID parsed from observation", {
  doc <- xml2::read_xml(x = observationXml)
  obs <- parseObservation(obj = doc, sos = testsos)
  fid <- sosFeatureIds(obs)

  expect_length(fid, 1)
  expect_equal(fid, "sf_8D0EA55680FEC1646E1A01C441D7220F9BD9F57C")
})

test_that("observed property parsed from observation", {
  doc <- xml2::read_xml(x = observationXml)
  obs <- parseObservation(obj = doc, sos = testsos)
  obsProps <- sosObservedProperties(obs)

  expect_length(obsProps, 1)
  expect_equal(obsProps, "http://www.52north.org/test/observableProperty/6")
})
