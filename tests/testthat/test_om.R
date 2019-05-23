################################################################################
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
################################################################################
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
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)
  expect_s4_class(observation, "OmOM_Observation")
})
#
# time parsing works #####
#
test_that("time parsing works", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)

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
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)

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
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = mySOS, featureCache = list())

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
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)
  result <- sosResult(observation)

  expect_equal(dim(result), c(1, 1))
  expect_equal(result[1,1], 1.23)
  expect_named(result, c("test_unit_1"))
})
#
# observation metadata is in result ----
#
test_that("observation metadata is in result", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)
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
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)
  fid <- sosFeatureIds(observation)

  expect_length(fid, 1)
  expect_equal(fid, "http://www.52north.org/test/featureOfInterest/1")
})
#
# observed property parsed from observation ----
#
test_that("observed property parsed from observation", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)
  obsProps <- sosObservedProperties(observation)

  expect_length(obsProps, 1)
  expect_equal(obsProps, "http://www.52north.org/test/observableProperty/1")
})

context("OM 2.0: parse observation with swe20:DataArray")

observationXml20 <- '<sos:observation xmlns:sos="http://www.opengis.net/sos/2.0"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xmlns:om="http://www.opengis.net/om/2.0"
        xmlns:gml="http://www.opengis.net/gml/3.2"
        xmlns:xlink="http://www.w3.org/1999/xlink">
      <om:OM_Observation gml:id="o_19">
      <om:type xlink:href="http://www.opengis.net/def/observationType/OGC-OM/2.0/OM_Measurement"/>
      <om:phenomenonTime>
        <gml:TimePeriod xmlns:gml="http://www.opengis.net/gml/3.2" gml:id="phenomenonTime_19">
          <gml:beginPosition>2000-01-01T13:45:00.000+13:00</gml:beginPosition>
          <gml:endPosition>2000-01-02T13:00:00.000+13:00</gml:endPosition>
        </gml:TimePeriod>
      </om:phenomenonTime>
      <om:resultTime>
        <gml:TimeInstant xmlns:gml="http://www.opengis.net/gml/3.2" gml:id="ti_a24f792082e3114273665b1b1e0645c99da9a8207d427445a23b689fee63ca71">
          <gml:timePosition>2000-01-02T13:00:00.000+13:00</gml:timePosition>
        </gml:TimeInstant>
      </om:resultTime>
      <om:procedure xlink:href="Hydrometric_Station"/>
      <om:parameter>
        <om:NamedValue xmlns:om="http://www.opengis.net/om/2.0" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
          <om:name xlink:href="offering"/>
          <om:value xmlns:xs="http://www.w3.org/2001/XMLSchema" xsi:type="xs:string">QR.Master@58902</om:value>
        </om:NamedValue>
      </om:parameter>
      <om:observedProperty xlink:href="Discharge" xlink:title="Discharge"/>
      <om:featureOfInterest xmlns:xlink="http://www.w3.org/1999/xlink" xlink:href="#mp_a2f42077f7b87d26d7bf42352a619a2903b5e16754376eeb216a813530b49e12"/>
      <om:result>
        <swe:DataArray xmlns:swe="http://www.opengis.net/swe/2.0" xmlns:xlink="http://www.w3.org/1999/xlink">
          <swe:elementCount>
            <swe:Count>
              <swe:value>10</swe:value>
            </swe:Count>
          </swe:elementCount>
          <swe:elementType name="Components">
            <swe:DataRecord xmlns:ns="http://www.opengis.net/swe/2.0">
              <ns:field name="phenomenonTime">
                <ns:Time definition="http://www.opengis.net/def/property/OGC/0/PhenomenonTime">
                  <ns:uom xlink:href="http://www.opengis.net/def/uom/ISO-8601/0/Gregorian"/>
                </ns:Time>
              </ns:field>
              <ns:field name="Discharge">
                <ns:Quantity definition="Discharge">
                  <ns:uom code="m^3/s"/>
                </ns:Quantity>
              </ns:field>
            </swe:DataRecord>
          </swe:elementType>
          <swe:encoding>
            <swe:TextEncoding xmlns:ns="http://www.opengis.net/swe/2.0" blockSeparator="@@" decimalSeparator="." tokenSeparator=","/>
          </swe:encoding>
          <swe:values>2000-01-01T13:45:00.000+13:00,6.357@@2000-01-01T14:15:00.000+13:00,6.576@@2000-01-01T15:15:00.000+13:00,7.185@@2000-01-01T15:45:00.000+13:00,7.594@@2000-01-01T16:00:00.000+13:00,7.883@@2000-01-01T16:15:00.000+13:00,23.22@@2000-01-01T16:30:00.000+13:00,51.21@@2000-01-01T16:45:00.000+13:00,60.73@@2000-01-01T17:00:00.000+13:00,63.32@@2000-01-01T17:15:00.000+13:00,62.98</swe:values>
        </swe:DataArray>
      </om:result>
    </om:OM_Observation>
  </sos:observation>'
#
# SWE 2.0 data array can be parsed ----
#
test_that("SWEv2.0: data array can be parsed", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml20), sos = testsos, featureCache = list(), retrieveFOI = FALSE)
  obsProps <- sosObservedProperties(observation)
  expect_length(obsProps, 1)
  expect_equal(obsProps, "Discharge")
  expect_equal(sosProcedures(observation)[[1]], "Hydrometric_Station")
  expect_equal(parsedate::format_iso_8601(observation@resultTime@timePosition@time), "2000-01-02T00:00:00+00:00")
  expect_equal(observation@featureOfInterest@href, "#mp_a2f42077f7b87d26d7bf42352a619a2903b5e16754376eeb216a813530b49e12")
  expect_true(is.data.frame(observation@result))
  expect_length(observation@result[[1]], 10)
  expect_length(colnames(observation@result), 2)
  expect_equal(colnames(observation@result)[[1]], "phenomenonTime")
  expect_equal(colnames(observation@result)[[2]], "Discharge")
  expect_equal(parsedate::format_iso_8601(observation@result[[ 1,1]]), "2000-01-01T00:45:00+00:00")
  expect_equal(parsedate::format_iso_8601(observation@result[[ 2,1]]), "2000-01-01T01:15:00+00:00")
  expect_equal(parsedate::format_iso_8601(observation@result[[ 3,1]]), "2000-01-01T02:15:00+00:00")
  expect_equal(parsedate::format_iso_8601(observation@result[[ 4,1]]), "2000-01-01T02:45:00+00:00")
  expect_equal(parsedate::format_iso_8601(observation@result[[ 5,1]]), "2000-01-01T03:00:00+00:00")
  expect_equal(parsedate::format_iso_8601(observation@result[[ 6,1]]), "2000-01-01T03:15:00+00:00")
  expect_equal(parsedate::format_iso_8601(observation@result[[ 7,1]]), "2000-01-01T03:30:00+00:00")
  expect_equal(parsedate::format_iso_8601(observation@result[[ 8,1]]), "2000-01-01T03:45:00+00:00")
  expect_equal(parsedate::format_iso_8601(observation@result[[ 9,1]]), "2000-01-01T04:00:00+00:00")
  expect_equal(parsedate::format_iso_8601(observation@result[[10,1]]), "2000-01-01T04:15:00+00:00")
  expect_equal(observation@result[[ 1,2]], 6.357)
  expect_equal(observation@result[[ 2,2]], 6.576)
  expect_equal(observation@result[[ 3,2]], 7.185)
  expect_equal(observation@result[[ 4,2]], 7.594)
  expect_equal(observation@result[[ 5,2]], 7.883)
  expect_equal(observation@result[[ 6,2]], 23.22)
  expect_equal(observation@result[[ 7,2]], 51.21)
  expect_equal(observation@result[[ 8,2]], 60.73)
  expect_equal(observation@result[[ 9,2]], 63.32)
  expect_equal(observation@result[[10,2]], 62.98)
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
