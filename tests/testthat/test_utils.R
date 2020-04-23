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
# Created: 2013-03-06                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

context("utils")

test_that(".sosFilterDCPs works", {
	dcps <- c("ows:Post" = "http://url/with/endpoint/one",
			"ows:Post" = "url.to/endpoint/two",
			"ows:Get" = "some.thing.com/different/")

	expect_equal(length(sos4R:::.sosFilterDCPs(dcp = dcps, pattern = "*")), 3)
	expect_equivalent(sos4R:::.sosFilterDCPs(dcp = dcps, pattern = list("POX" = "/endpoint"))[[2]], "url.to/endpoint/two")
	expect_equal(sos4R:::.sosFilterDCPs(dcp = dcps, pattern = list("POX" = "/endpoint")),
	             c("ows:Post" = "http://url/with/endpoint/one", "ows:Post" = "url.to/endpoint/two"))
	expect_equivalent(sos4R:::.sosFilterDCPs(dcps, list("POX" = "/one")), "http://url/with/endpoint/one")
})

test_that("addional KVPs are concatenated correctly", {
  expected <- "this=is&working=correctly"
  actual <- list("this" = "is", "working" = "correctly")
  expect_equal(.encodeAdditionalKVPs(actual), expected)
})

test_that("can get CRS from URN", {
  obj <- sosGetCRS("urn:ogc:def:crs:EPSG::4326")
  expect_s4_class(obj, "CRS")
  expect_equal(slotNames(obj), "projargs")
})

test_that("can get CRS from lowercase URN", {
  obj <- sosGetCRS("urn:ogc:def:crs:epsg::4326")
  expect_s4_class(obj, "CRS")
  expect_equal(slotNames(obj), "projargs")
})

test_that("can get CRS from URN with version", {
  obj <- sosGetCRS("urn:ogc:def:crs:EPSG:99:4326")
  expect_s4_class(obj, "CRS")
  expect_equal(slotNames(obj), "projargs")
})

test_that("can get CRS from URL", {
  obj <- sosGetCRS("http://www.opengis.net/def/crs/EPSG/0/4326")
  expect_s4_class(obj, "CRS")
  expect_equal(slotNames(obj), "projargs")
})

library("webmockr")
webmockr::enable("httr")
webmockr::httr_mock()

test_that("can create time instant from POSIXct and encode as XML", {
  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://example.com/sos-utils?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
    webmockr::wi_th(
      headers = list("Accept" = "application/xml")
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/Capabilities_200_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(url = "http://example.com/sos-utils", binding = "KVP", version = sos200_version)
  instant <- sosCreateTimeInstant(sos = sos, time = parsedate::parse_iso_8601("2019-01-01 01:01:01"))

  expect_s4_class(instant, "GmlTimeObject")
  xml <- encodeXML(instant, sos = sos)
  expect_match(toString(xml), "2019-01-01T01:01:01\\+00:00</gml:timePosition>")
})

test_that("can create time period from POSIXct objects and encode as XML", {
  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://example.com/sos-utils?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
    webmockr::wi_th(
      headers = list("Accept" = "application/xml")
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/Capabilities_200_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(url = "http://example.com/sos-utils", binding = "KVP", version = sos200_version)
  period <- sosCreateTimePeriod(sos = sos,
                                 begin = parsedate::parse_iso_8601("2019-01-01 01:01:01"),
                                 end = parsedate::parse_iso_8601("2019-01-01 02:02:02"))

  expect_s4_class(period, "GmlTimeObject")
  xml <- encodeXML(period, sos = sos)
  expect_match(toString(xml), "2019-01-01T01:01:01\\+00:00</gml:beginPosition>")
  expect_match(toString(xml), "2019-01-01T02:02:02\\+00:00</gml:endPosition>")
})

webmockr::disable("httr")
