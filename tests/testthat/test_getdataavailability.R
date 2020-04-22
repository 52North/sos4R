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
# Author: Eike Hinderk Jürrens (e.h.juerrens@52north.org)                      #
# Created: 2019-03-28                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
context("GetDataAvailability: parsing")
library(webmockr)
library(httr)
library(stringr)

webmockr::enable("httr")
webmockr::httr_mock()

test_that("parseGetDataAvailabilityResponse() returns an empty list if an empty response is received", {
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
  doc <- xml2::read_xml("../responses/GetDataAvailability_100_Example.com_empty.xml")
  dams <- parseGetDataAvailabilityResponse(obj = doc, sos)

  expect_false(is.null(dams))
  expect_true(is.list(dams))
  expect_length(dams, 0)
})

test_that("parseGetDataAvailabilityResponse() returns a correct parsed list of DataAvailabilityMembers if a response is received", {
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
  doc <- xml2::read_xml("../responses/GetDataAvailability_100_Example.com_single.xml")
  dams <- parseGetDataAvailabilityResponse(obj = doc, sos)

  expect_false(is.null(dams))
  expect_true(is.list(dams))
  expect_length(dams, 1)

  # <gda:dataAvailabilityMember gml:id="dam_1">
  #   <gda:procedure xlink:href="ws2500" xlink:title="ws2500"/>
  #   <gda:observedProperty xlink:href="WindDirection" xlink:title="WindDirection"/>
  #   <gda:featureOfInterest xlink:href="elv-ws2500" xlink:title="ELV WS2500"/>
  #   <gda:phenomenonTime>
  #     <gml:TimePeriod gml:id="tp_1">
  #       <gml:beginPosition>2019-03-01T00:30:00.000Z</gml:beginPosition>
  #       <gml:endPosition>2019-03-28T23:45:00.000Z</gml:endPosition>
  #     </gml:TimePeriod>
  #   </gda:phenomenonTime>
  #  </gda:dataAvailabilityMember>
  #
  # new("DataAvailabilityMember",
  #     procedure = procedure,
  #     observedProperty = observedProperty,
  #     featureOfInterest = featureOfInterest,
  #     phenomenonTime = phenomenonTime)

  dam <- dams[[1]]

  expect_equal(dam@procedure, "ws2500", info = "procedure")
  expect_equal(dam@observedProperty, "WindDirection", info = "observedProperty")
  expect_equal(dam@featureOfInterest, "elv-ws2500", info = "featureOfInterest")
  expect_true(is.object(dam@phenomenonTime))
  expect_true(isS4(dam@phenomenonTime))
  expect_true(is(dam@phenomenonTime,"GmlTimePeriod"))
  expect_equal(parsedate::format_iso_8601(dam@phenomenonTime@beginPosition@time), "2019-03-01T00:30:00+00:00", info = "phenomenonTime::start")
  expect_equal(parsedate::format_iso_8601(dam@phenomenonTime@endPosition@time), "2019-03-28T23:45:00+00:00", info = "phenomenonTime::end")
})

test_that("accessing feature ID from dataAvailabilityMember elements works", {
  sos <- SOS_Test(name = "gda", version = sos200_version)
  dams <- parseGetDataAvailabilityResponse(obj = xml2::read_xml("../responses/GetDataAvailability_100_timerefs.xml"), sos = sos)

  expect_equal(sosFeatureIds(dams), c("elv-ws2500", "wwu-ws-kli-hsb", "wwu-ws-kli-hsb", "wwu-ws-kli-hsb", "elv-ws2500"))
  expect_equal(sosFeaturesOfInterest(dams), list("elv-ws2500", "wwu-ws-kli-hsb", "wwu-ws-kli-hsb", "wwu-ws-kli-hsb", "elv-ws2500"))
})

test_that("accessing observed properties from dataAvailabilityMember elements works", {
  sos <- SOS_Test(name = "gda", version = sos200_version)
  dams <- parseGetDataAvailabilityResponse(obj = xml2::read_xml("../responses/GetDataAvailability_100_timerefs.xml"), sos = sos)

  expect_equal(sosObservedProperties(dams), c("WindDirection", "WindDirection", "WindMaxGust", "Humidity", "Humidity"))
})

test_that("accessing time from dataAvailabilityMember elements works", {
  sos <- SOS_Test(name = "gda", version = sos200_version)
  dams <- parseGetDataAvailabilityResponse(obj = xml2::read_xml("../responses/GetDataAvailability_100_timerefs.xml"), sos = sos)

  expect_equivalent(sosTime(dams[[1]]), list(begin = sosConvertTime("2019-03-01T00:30:00.000Z", sos = sos),
                                        end = sosConvertTime("2019-03-28T23:45:00.000Z", sos = sos)))
})

test_that("accessing procedures from dataAvailabilityMember elements works", {
  sos <- SOS_Test(name = "gda", version = sos200_version)
  dams <- parseGetDataAvailabilityResponse(obj = xml2::read_xml("../responses/GetDataAvailability_100_timerefs.xml"), sos = sos)

  expect_equal(unique(sosProcedures(dams)), c("ws2500", "wwu-ws-kli-hsb"))
})

test_that("parseGetDataAvailabilityResponse() can handle in-document references to time stamps", {
  sos <- SOS_Test(name = "gda", version = sos200_version)
  dams <- parseGetDataAvailabilityResponse(obj = xml2::read_xml("../responses/GetDataAvailability_100_timerefs.xml"), sos = sos)

  expect_length(dams, 5)

  tp1 <- list(begin = sosConvertTime("2019-03-01T00:30:00.000Z", sos = sos),
       end = sosConvertTime("2019-03-28T23:45:00.000Z", sos = sos))
  tp2 <- list(begin = sosConvertTime("2019-03-01T00:10:00.000Z", sos = sos),
              end = sosConvertTime("2019-03-28T23:50:00.000Z", sos = sos))

  expect_equivalent(sosTime(dams[[1]]), tp1)
  expect_equivalent(sosTime(dams[[2]]), tp2)
  expect_equivalent(sosTime(dams[[3]]), tp2)
  expect_equivalent(sosTime(dams[[4]]), tp2)
  expect_equivalent(sosTime(dams[[5]]), tp1)
})

test_that("parseGetDataAvailabilityResponse() returns a correct parsed list of DataAvailabilityMembers if a response is received", {
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
  doc <- xml2::read_xml("../responses/Issue-158.xml")
  dams <- parseGetDataAvailabilityResponse(obj = doc, sos)

  expect_false(is.null(dams))
  expect_true(is.list(dams))
  expect_length(dams, 4)

  dam <- dams[[1]]

  expect_equal(dam@procedure, "40458D_7ad6ee986167b4a706a6f791e8196cb4", info = "procedure")
  expect_equal(dam@observedProperty, "https://example.com/vocab-service/Carbon_monoxide", info = "observedProperty")
  expect_equal(dam@featureOfInterest, "40458D_IT_0e32b32a52a0cded425a4075afa65f93", info = "featureOfInterest")
  expect_true(is.object(dam@phenomenonTime))
  expect_true(isS4(dam@phenomenonTime))
  expect_true(is(dam@phenomenonTime,"GmlTimePeriod"))
  expect_equal(parsedate::format_iso_8601(dam@phenomenonTime@beginPosition@time), "2020-01-05T17:02:00+00:00", info = "phenomenonTime::start")
  expect_equal(parsedate::format_iso_8601(dam@phenomenonTime@endPosition@time), "2020-01-30T14:01:00+00:00", info = "phenomenonTime::end")
})

webmockr::disable("httr")

test_that("there is an error if GDA with SOS 1.0.0 is attempted", {
  mySOS <- SOS_Test(name = "gda_100", version = sos100_version)
  expect_error(getDataAvailability(sos = mySOS), "unable to find an inherited method")
})

context("GetDataAvailability: encoding")

test_that("KVP (1.0.0)", {
  testsos <- SOS_Test(name = "gda_100", version = sos200_version)
  gda <- SosGetDataAvailability_1.0.0(service = "ser", version = testsos@version,
                               observedProperties = list("prop1", "http://prop2"),
                               procedures = list("proc1", "https://proc2"),
                               featuresOfInterest = list("foi1", "https://foi2"),
                               offerings = list("off1", "off2"))
  request <- encodeRequestKVP(obj = gda, sos = testsos)

  expect_match(request, "service=SOS&version=2.0.0&request=GetDataAvailability&")
  expect_match(request, "procedure=proc1,https%3A%2F%2Fproc2")
  expect_match(request, "observedProperty=prop1,http%3A%2F%2Fprop2")
  expect_match(request, "featureOfInterest=foi1,https%3A%2F%2Ffoi2")
  expect_match(request, "offering=off1,off2")
})

context("GetDataAvailability: integration tests\n")

test_that("KVP (2.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/kvp",
               binding = "KVP", useDCPs = FALSE,
               version = sos200_version)

  dataAvailability <- getDataAvailability(sos = mySOS)

  expect_length(dataAvailability, 44)
  expect_equal(sapply(dataAvailability, class), rep("DataAvailabilityMember", length(dataAvailability)))
  expect_equal(unique(sosProcedures(dataAvailability)), c("wxt520", "ws2500-internal", "ws2500", "wwu-ws-kli-hsb"))
})

test_that("POX (2.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/pox",
               binding = "POX",
               useDCPs = FALSE,
               version = sos200_version)

  skip("GDA for POX not implemented yet.")
  dataAvailability <- getDataAvailability(sos = mySOS)

  expect_length(dataAvailability, 44)
  expect_equal(sapply(dataAvailability, class), rep("DataAvailabilityMember", length(dataAvailability)))
  expect_equal(unique(sosProcedures(dataAvailability)), c("wxt520", "ws2500-internal", "ws2500", "wwu-ws-kli-hsb"))
})
