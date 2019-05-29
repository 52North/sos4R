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
context("encoding GML: spatial")

testsos <- SOS_Test(name = "testgml")
#
# point minimal ----
#
test_that("point minimal", {
  point <- GmlPoint(pos = GmlDirectPositionLatLon(10, 20))
  encoded <- encodeXML(obj = point, sos = testsos)
  encodedString <- stringr::str_remove_all(toString(encoded), "\n")
  expect_match(encodedString, "10 20</gml:pos></gml:Point>")
})
#
# position minimal ----
#
test_that("position minimal", {
  position <- GmlDirectPosition(pos = "1 2")
  encoded <- encodeXML(obj = position, sos = testsos)
  expect_match(toString(encoded), "<gml:pos")
  expect_match(toString(encoded), "1 2</gml:pos>")
})
#
# position full ----
#
test_that("position full", {
  position <- GmlDirectPosition(pos = "1 2", srsName = "the_srs", srsDimension = as.integer(3),
                                axisLabels = c("one two"), uomLabels = c("m deg"))
  encoded <- encodeXML(obj = position, sos = testsos)
  encodedString <- stringr::str_remove_all(toString(encoded), "\n")
  expect_match(encodedString, 'srsName="the_srs"')
  expect_match(encodedString, 'srsDimension="3"')
  expect_match(encodedString, 'axisLabels="one two"')
  expect_match(encodedString, 'uomLabels="m deg"')
})
#
# envelope ----
#
test_that("envelope", {
  env <- GmlEnvelope(lowerCorner = GmlDirectPositionLatLon(1, 2),
                     upperCorner = GmlDirectPositionLatLon(8, 9))
  encoded <- encodeXML(obj = env, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(encoded), ">\\s*<", "><")

  expect_match(encodedString, '<gml:Envelope')
  expect_match(encodedString, '1 2</gml:lowerCorner>')
  expect_match(encodedString, '8 9</gml:upperCorner>')
})

context("encoding GML: temporal classes to XML")

testsos <- SOS_Test(name = "testgml")
#
# time position minimal ----
#
test_that("time position minimal", {
  tpos <- GmlTimePosition(time = parsedate::parse_iso_8601("2019-01-01"))

  encoded <- encodeXML(obj = tpos, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(encoded), ">\\s*<", "><")

  expect_match(encodedString, '2019-01-01T00:00:00\\+00:00</gml:timePosition>')
  expect_match(encodedString, 'gml:id="id_[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"')
})
#
# time position all ----
#
test_that("time position all", {
  tpos <- GmlTimePosition(time = parsedate::parse_iso_8601("2019-01-01"),
                          frame = "the_frame",
                          calendarEraName = "the_era",
                          indeterminatePosition = "yes")

  encoded <- encodeXML(obj = tpos, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(encoded), ">\\s*<", "><")

  expect_match(encodedString, 'frame="the_frame"')
  expect_match(encodedString, 'calendarEraName="the_era"')
  expect_match(encodedString, 'indeterminatePosition="yes"')
  expect_match(encodedString, '2019-01-01T00:00:00\\+00:00</gml:timePosition>')
})
#
# time instant ----
#
test_that("time instant", {
  instant <- GmlTimeInstant(timePosition = GmlTimePosition(time = parsedate::parse_iso_8601("2019-01-01")))
  encoded <- encodeXML(obj = instant, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(encoded), ">\\s*<", "><")

  expect_match(encodedString, '<gml:TimeInstant')
  expect_match(encodedString, '2019-01-01T00:00:00\\+00:00</gml:timePosition></gml:TimeInstant>')
  expect_match(encodedString, 'gml:id="id_[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"')
})
#
# GML namespace of SOS is used ----
#
test_that("GML namespace of SOS is used", {
  sos1 <- SOS_Test(version = sos100_version)
  sos2 <- SOS_Test(version = sos200_version)
  instant <- GmlTimeInstant(timePosition = GmlTimePosition(time = parsedate::parse_iso_8601("2019-01-01")))

  encoded1 <- encodeXML(obj = instant, sos = sos1)
  encoded2 <- encodeXML(obj = instant, sos = sos2)

  expect_match(toString(encoded1), paste0('xmlns:gml="', gmlNamespace))
  expect_match(toString(encoded2), paste0('xmlns:gml="', gml32Namespace))
})
#
# time period ----
#
test_that("time period", {
  period <- sosCreateTimePeriod(sos = testsos, begin = parsedate::parse_iso_8601("2019-01-01"), end = parsedate::parse_iso_8601("2019-02-03"))
  encoded <- encodeXML(obj = period, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(encoded), ">\\s*<", "><")

  expect_match(encodedString, '2019-01-01T00:00:00\\+00:00</gml:beginPosition>')
  expect_match(encodedString, '2019-02-03T00:00:00\\+00:00</gml:endPosition></gml:TimePeriod>')
  expect_match(encodedString, 'gml:id="id_[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"')
})
#
# time interval ----
#
test_that("time interval", {
  interval <- sosCreateTimePeriod(sos = testsos, begin = parsedate::parse_iso_8601("2019-01-01"), end = parsedate::parse_iso_8601("2019-02-03"),
                                  timeInterval = GmlTimeInterval(interval = "everyother", unit = "hr", radix = as.integer(17), factor = as.integer(2)))
  encoded <- encodeXML(obj = interval, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(encoded), ">\\s*<", "><")

  expect_match(encodedString, '<gml:timeInterval unit="hr" radix="17" factor="2">everyother</gml:timeInterval>')
  expect_match(encodedString, 'gml:id="id_[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"')
})
#
# envelope ----
#
test_that("envelope", {
  bbox <- sosCreateBBOX(lowLat = 5.0, lowLon = 1.0,
                        uppLat = 10.0, uppLon = 3.0,
                        srsName = "urn:ogc:def:crs:EPSG::4326")

  encoded <- encodeXML(obj = bbox, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(encoded), ">\\s*<", "><")

  expect_match(encodedString, '<ogc:BBOX')
  expect_match(encodedString, 'urn:ogc:data:location</ogc:PropertyName>')
  expect_false(grepl(pattern = 'gml:id="id_[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"', x = encodedString))
  expect_match(encodedString, '<gml:lowerCorner>5 1</gml:lowerCorner><gml:upperCorner>10 3</gml:upperCorner></gml:Envelope></ogc:BBOX>')

  testsos20 <- SOS_Test(name = "gml_200", version = sos200_version)
  encoded <- encodeXML(obj = bbox, sos = testsos20)
  encodedString <- stringr::str_replace_all(toString(encoded), ">\\s*<", "><")

  expect_match(encodedString, '<ogc:BBOX')
  expect_match(encodedString, 'urn:ogc:data:location</ogc:PropertyName>')
  expect_match(encodedString, 'gml:id="id_[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"')
  expect_match(encodedString, '<gml:lowerCorner>5 1</gml:lowerCorner><gml:upperCorner>10 3</gml:upperCorner></gml:Envelope></ogc:BBOX>')
})

context("encoding GML: temporal classes to KVP")
#
# time instant ----
#
test_that("time instant", {
  instant <- GmlTimeInstant(timePosition = GmlTimePosition(time = parsedate::parse_iso_8601("2019-01-01")))
  encodedString <- encodeKVP(obj = instant, sos = testsos)
  expect_equal(encodedString, '2019-01-01T00:00:00+00:00')
})
#
# time period (SOS 1.0.0) ----
#
test_that("time period (SOS 1.0.0)", {
  period <- sosCreateTimePeriod(sos = testsos, begin = parsedate::parse_iso_8601("2019-01-01"), end = parsedate::parse_iso_8601("2019-02-03"))
  encodedString <- encodeKVP(obj = period, sos = testsos)
  expect_equal(encodedString, '2019-01-01T00:00:00+00:00::2019-02-03T00:00:00+00:00')
})
#
# time period (SOS 2.0.0) ----
#
test_that("time period (SOS 2.0.0)", {
  testsos20 <- SOS_Test(name = "testgml20", version = sos200_version)
  period <- sosCreateTimePeriod(sos = testsos20, begin = parsedate::parse_iso_8601("2019-01-01"), end = parsedate::parse_iso_8601("2019-02-03"))
  encodedString <- encodeKVP(obj = period, sos = testsos20)
  expect_equal(encodedString, '2019-01-01T00:00:00+00:00/2019-02-03T00:00:00+00:00')
})

context("parsing: GML")

periodXml <- '<gml:TimePeriod xmlns:gml="http://www.opengis.net/gml">
  <gml:beginPosition>2006-01-01T00:10:00Z</gml:beginPosition>
  <gml:endPosition>2007-01-01T00:10:00Z</gml:endPosition>
</gml:TimePeriod>'
#
# indetermindate end position in TimePeriod ----
#
test_that("indetermindate end position in TimePeriod", {
  period <- parseTimePeriod(obj = xml2::read_xml(periodXml), sos = testsos)

  expect_s4_class(period, "GmlTimePeriod")
  expect_equal(period@begin, NULL)
  expect_equal(period@end, NULL)
  expect_s4_class(period@beginPosition, "GmlTimePosition")
  expect_s4_class(period@endPosition, "GmlTimePosition")
  expect_s3_class(period@beginPosition@time, "POSIXt")
  expect_s3_class(period@endPosition@time, "POSIXt")
  expect_equal(period@beginPosition@time, sosConvertTime("2006-01-01T00:10:00Z", testsos))
  expect_equal(period@endPosition@time, sosConvertTime("2007-01-01T00:10:00Z", testsos))
})

indeterminatePeriodXml <- '<gml:TimePeriod xmlns:gml="http://www.opengis.net/gml">
  <gml:beginPosition>2006-01-01T00:10:00Z</gml:beginPosition>
  <gml:endPosition indeterminatePosition="now"/>
</gml:TimePeriod>'
#
# indeterminate end position in TimePeriod ----
#
test_that("indeterminate end position in TimePeriod", {
  period <- parseTimePeriod(obj = xml2::read_xml(indeterminatePeriodXml), sos = testsos)

  expect_s4_class(period, "GmlTimePeriod")
  expect_equal(period@begin, NULL)
  expect_equal(period@end, NULL)
  expect_s4_class(period@beginPosition, "GmlTimePosition")
  expect_s4_class(period@endPosition, "GmlTimePosition")
  expect_true(is.na(period@endPosition@time))
  expect_equal(period@endPosition@indeterminatePosition, "now")
})
#
# accessor function translates indeterminate time 'now' to current time ----
#
test_that("accessor function translates indeterminate time 'now' to current time", {
  period <- parseTimePeriod(obj = xml2::read_xml(indeterminatePeriodXml), sos = testsos)

  time <- sosTime(period)
  expect_s3_class(time$begin, "POSIXt")
  expect_s3_class(time$end, "POSIXt")

  expect_equal(toString(time$begin), toString(sosConvertTime(x = "2006-01-01T00:10:00Z", sos = testsos)))
  diff <- time$end - Sys.time()
  expect_equal(attr(diff, "units"), "secs")
  expect_true(abs(diff) < 1)
})

envelopeXml <- '<gml:Envelope srsName="http://www.opengis.net/def/crs/EPSG/0/4326" xmlns:gml="http://www.opengis.net/gml">
    <gml:lowerCorner>-55 -178.343</gml:lowerCorner>
    <gml:upperCorner>71.758 180</gml:upperCorner>
  </gml:Envelope>'
#
# boundedBy ----
#
test_that("boundedBy", {
  env <- parseEnvelope(obj = xml2::read_xml(envelopeXml), sos = testsos)

  expect_true(is.list(env))
  expect_named(env, c("srsName", "lowerCorner", "upperCorner"))
  expect_equal(env$srsName, "http://www.opengis.net/def/crs/EPSG/0/4326")
  expect_equal(env$lowerCorner, "-55 -178.343")
  expect_equal(env$upperCorner, "71.758 180")
})
#
# boundedBy with coordinate swichting ----
#
test_that("boundedBy with coordinate swichting", {
  switchsos <- SOS_Test(name = "testswitchgml", switchCoordinates = TRUE)
  expect_warning(env <- parseEnvelope(obj = xml2::read_xml(envelopeXml), sos = switchsos),
                 "Switching coordinates in envelope")

  expect_true(is.list(env))
  expect_named(env, c("srsName", "lowerCorner", "upperCorner"))
  expect_equal(env$srsName, "http://www.opengis.net/def/crs/EPSG/0/4326")
  expect_equal(env$lowerCorner, "-178.343 -55")
  expect_equal(env$upperCorner, "180 71.758")
})

testthat::context("GML 3.2 -> parsePoint")

library(webmockr)
library(httr)
library(stringr)
webmockr::enable("httr")
webmockr::httr_mock()
point1 <- '<gml32:Point xmlns:gml32="http://www.opengis.net/gml/3.2" gml32:id="Point_ssf_033EE116F7D14A5A9735B651830F38A438CEFF1F">
              <gml32:pos srsName="http://www.opengis.net/def/crs/EPSG/0/4326">51.9347763061523 7.65237522125244</gml32:pos>
           </gml32:Point>'

#
# gml:Point with gml:pos string ----
#
test_that("gml:Point with gml:pos string", {
  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://example.com/sos-list-phenomena?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
    webmockr::wi_th(
      headers = list("Accept" = "application/xml")
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/Capabilities_200_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")

  point <- parsePoint(obj = xml2::xml_root(xml2::read_xml(x = point1)), sos = sos)

  expect_true(isS4(point))
  expect_true(inherits(x = point, "GmlPoint"))
  expect_true(isS4(point@pos))
  expect_true(inherits(x = point@pos, "GmlDirectPosition"))
  expect_false(is.na(point@pos@srsName))
  expect_equal("http://www.opengis.net/def/crs/EPSG/0/4326", point@pos@srsName)
  expect_false(is.na(point@pos@pos))
  expect_equal("51.9347763061523 7.65237522125244", point@pos@pos)
  expect_false(is.na(point@id))
  expect_equal("Point_ssf_033EE116F7D14A5A9735B651830F38A438CEFF1F", point@id)
})

webmockr::disable("httr")

context("GML: accessors")
#
# featureOfInterest from GmlFeaturePropert ----
#
test_that("featureOfInterest from GmlFeatureProperty", {
  sos <- SOS_Test(name = "testgml")
  propertyHref <- GmlFeatureProperty(href = "http://the_feature")
  expect_equal(sosFeaturesOfInterest(propertyHref),  "http://the_feature")

  pos <- GmlDirectPositionLatLon(lat = 1, lon = 2, srsName = "http://www.opengis.net/def/crs/EPSG/0/4326")
  samplingPoint <- SaSamplingPoint(sampledFeatures = list("feature1"),
                                   position = GmlPointProperty(point = GmlPoint(pos = pos)))
  property <- GmlFeatureProperty(feature = samplingPoint)
  expect_equal(sosFeaturesOfInterest(property),  samplingPoint)
})

context("decoding GML: temporal classes from XML")

gmlTimeInstantXml <- '<gml:TimeInstant xmlns:gml="http://www.opengis.net/gml/3.2" gml:id="gml-time-instant-id">
          <gml:timePosition>2000-01-02T13:00:00.000+13:00</gml:timePosition>
        </gml:TimeInstant>'
#
# timezones are parsed correctly ----
#
test_that("timezones are parsed correctly", {
  sos <- SOS_Test(name = "testgml", version = sos200_version)
  gmlTimeInstant <- parseTimeInstant(obj = xml2::read_xml(x = gmlTimeInstantXml), sos = sos)
  expect_equal(parsedate::format_iso_8601(gmlTimeInstant@timePosition@time), "2000-01-02T00:00:00+00:00")
  expect_equal(gmlTimeInstant@id, "gml-time-instant-id")
})