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
# Created: 2019-04-26                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
library(webmockr)
library(httr)
context("parsing: OM 2.0 result")

resultXml <- '<om:result xmlns:om="http://www.opengis.net/om/2.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ns="http://www.opengis.net/gml/3.2" uom="test_unit_9_3" xsi:type="ns:MeasureType">50.28</om:result>'
#
# result value parsing works ----
#
test_that("result value parsing works", {
  testsos <- SOS_Test(name = "omresult")
  result <- parseResult(obj = xml2::read_xml(x = resultXml), sos = testsos)

  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), c(1,1))
  expect_equal(result[1,1], 50.28)
})

context("parsing: OM_Observation from sos:observation")

observationXml <- '<sos:observation xmlns:sos="http://www.opengis.net/sos/2.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:om="http://www.opengis.net/om/2.0" xmlns:gml="http://www.opengis.net/gml/3.2" xmlns:xlink="http://www.w3.org/1999/xlink">
    <om:OM_Observation gml:id="o_121">
      <gml:identifier codeSpace="http://www.opengis.net/def/nil/OGC/0/unknown">http://www.52north.org/test/observation/1</gml:identifier>
      <om:type xlink:href="http://www.opengis.net/def/observationType/OGC-OM/2.0/OM_Measurement"/>
      <om:phenomenonTime>
        <gml:TimeInstant gml:id="phenomenonTime_121">
          <gml:timePosition>2012-11-19T13:00:00.000Z</gml:timePosition>
        </gml:TimeInstant>
      </om:phenomenonTime>
      <om:resultTime xlink:href="#phenomenonTime_121"/>
      <om:procedure xlink:href="http://www.52north.org/test/procedure/1"/>
      <om:observedProperty xlink:href="http://www.52north.org/test/observableProperty/1"/>
      <om:featureOfInterest xlink:href="http://www.52north.org/test/featureOfInterest/1" xlink:title="con terra"/>
      <om:result xmlns:ns="http://www.opengis.net/gml/3.2" uom="test_unit_1" xsi:type="ns:MeasureType">1.23</om:result>
    </om:OM_Observation>
  </sos:observation>'

testsos <- SOS_Test(name = "omresult", version = sos200_version)

#
# correct class is returned ----
#
test_that("correct class is returned", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, retrieveFOI = FALSE)
  expect_s4_class(observation, "OmOM_Observation")
})
#
# time parsing works #####
#
test_that("time parsing works", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, retrieveFOI = FALSE)

  times <- sosTime(observation)
  expect_equal(times$resultTime, times$phenomenonTime)
  expect_true(is.list(times))
  expect_s3_class(times[[1]], "POSIXct")
  expect_s3_class(times[[2]], "POSIXct")
  expect_named(times, c("resultTime", "phenomenonTime"))
})
#
# coordinates are not available without FOI with a warning ----
#
test_that("coordinates are not available without FOI with a warning", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, retrieveFOI = FALSE)

  expect_warning(coords <- sosCoordinates(observation),
                 "contains a feature")
  expect_true(is.na(coords))
})
#
# coordinates are available with retrieved FOI ----
#
webmockr::enable("httr")
webmockr::httr_mock()
test_that("coordinates are available with retrieved FOI", {
  webmockr::stub_registry_clear()
  webmockr::stub_request('get', uri = 'http://sensorweb.demo.52north.org/52n-sos-webapp/service/kvp?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml') %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/Capabilities_200_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  webmockr::stub_request('get', uri = 'http://sensorweb.demo.52north.org/52n-sos-webapp/service/kvp?service=SOS&version=2.0.0&request=GetFeatureOfInterest&featureOfInterest=http%3A%2F%2Fwww.52north.org%2Ftest%2FfeatureOfInterest%2F1') %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetFeatureOfInterest_200_O&M_Test.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/kvp",
               binding = "KVP",
               useDCPs = FALSE,
               version = sos200_version)
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = mySOS)

  coords <- sosCoordinates(observation)
  expect_named(coords, c("lon", "lat", "SRS"))
  expect_equal(coords[["lon"]], 7.727958)
  expect_equal(coords[["lat"]], 51.883906)
})
webmockr::disable("httr")
#
# result parsed from observation ----
#
test_that("result parsed from observation", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, retrieveFOI = FALSE)
  result <- sosResult(observation)

  expect_equal(dim(result), c(1, 1))
  expect_equal(result[1,1], 1.23)
  expect_named(result, c("test_unit_1"))
})
#
# observation metadata is in result ----
#
test_that("observation metadata is in result", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, retrieveFOI = FALSE)
  result <- sosResult(observation)

  expect_named(result, c("test_unit_1"))
  skip("Result data.frame not unified yet.")
  expect_named(attributes(result[,1]), c("name", "definition", "unit of measurement"))
  expect_equal(sosUOM(result), c("test_unit_1"))
})
#
# feature ID parsed from observation ----
#
test_that("feature ID parsed from observation", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, retrieveFOI = FALSE)
  fid <- sosFeatureIds(observation)

  expect_length(fid, 1)
  expect_equal(fid, "http://www.52north.org/test/featureOfInterest/1")
})
#
# observed property parsed from observation ----
#
test_that("observed property parsed from observation", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, retrieveFOI = FALSE)

  obsProps <- sosObservedProperties(observation)

  expect_length(obsProps, 1)
  expect_equal(obsProps, "http://www.52north.org/test/observableProperty/1")
})

context("OM: accessors")
#
# coordinates from om:ObservationProperty returns data.frame with NAs ----
#
test_that("coordinates from om:ObservationProperty returns data.frame with NAs", {
  property <- OmObservationProperty(href = "http://obs/prop/1")
  sos <- SOS_Test(name = "testgml")
  expect_warning(coords <- sosCoordinates(property),
                  "No coordinates")
  expect_named(coords, c("lon", "lat", "SRS"))
  expect_equal(as.list(coords[1,]), list(lon = NA, lat = NA, SRS = NA))
})
