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
# Created: 2019-04-01                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
library(webmockr)
library(httr)
library(stringr)
webmockr::enable("httr")
webmockr::httr_mock()
context("convenience layer -> siteList()")

.checkEmptySitesDataFrame <- function(sitesDataFrame) {
  expect_false(is.null(sitesDataFrame))
  expect_true(is.data.frame(sitesDataFrame))
  expect_equal(length(colnames(sitesDataFrame)), 1, info = "number of columns in sites data.frame")
  expect_equal(colnames(sitesDataFrame)[[1]], "siteID", info = "correct column name")
  expect_equal(nrow(sitesDataFrame), 0, info = "number of unique sites")
}

test_that("KVP::siteList(sos, empty = TRUE) returns an empty list of sites as one column data.frame if the SOS is empty", {
  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://example.com/sos-list-phenomena-empty?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
    webmockr::wi_th(
      headers = list("Accept" = "application/xml")
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/Capabilities_200_Example.com_empty.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  webmockr::stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetFeatureOfInterest') %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetFeatureOfInterest_200_Example.com_empty.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena-empty", binding = "KVP")
  sitesDataFrame <- siteList(sos, empty = TRUE)

  .checkEmptySitesDataFrame(sitesDataFrame)
})


.checkSitesDataFrame <- function(sitesDataFrame) {
  expect_false(is.null(sitesDataFrame))
  expect_true(is.data.frame(sitesDataFrame))
  expect_equal(length(colnames(sitesDataFrame)), 1, info = "number of columns in sites data.frame")
  expect_equal(colnames(sitesDataFrame)[[1]], "siteID", info = "correct column name")
  expect_equal(nrow(sitesDataFrame), 4, info = "number of unique sites")
  # check all values
  expect_equal("elv-ws2500",          sitesDataFrame[ 1, 1])
  expect_equal("elv-ws2500-internal", sitesDataFrame[ 2, 1])
  expect_equal("vaisala-wxt520",      sitesDataFrame[ 3, 1])
  expect_equal("wwu-ws-kli-hsb",      sitesDataFrame[ 4, 1])
}

test_that("KVP::siteList(sos, empty = TRUE) returns the current list of sites as one column data.frame", {
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
  webmockr::stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetFeatureOfInterest') %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetFeatureOfInterest_200_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")
  sitesDataFrame <- siteList(sos, empty = TRUE)

  .checkSitesDataFrame(sitesDataFrame)
})

.checkSitesDataFrameWithSitesWithData <- function(sitesDataFrame) {
  expect_false(is.null(sitesDataFrame))
  expect_true(is.data.frame(sitesDataFrame))
  expect_equal(length(colnames(sitesDataFrame)), 1, info = "number of columns in sites data.frame")
  expect_equal(colnames(sitesDataFrame)[[1]], "siteID", info = "correct column name")
  expect_equal(nrow(sitesDataFrame), 3, info = "number of unique sites")
  # check all values
  expect_equal("elv-ws2500-internal", sitesDataFrame[ 1, 1])
  expect_equal("vaisala-wxt520",      sitesDataFrame[ 2, 1])
  expect_equal("wwu-ws-kli-hsb",      sitesDataFrame[ 3, 1])
}

test_that("KVP::siteList(sos) or siteList(sos, empty = FALSE) returns a list of stations as one column data.frame that contain data", {
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
  webmockr::stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetDataAvailability') %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetDataAvailability_100_Example.com_OneFeatureLess.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")

  sitesDataFrame <- siteList(sos, empty = FALSE)
  .checkSitesDataFrameWithSitesWithData(sitesDataFrame)

  sitesDataFrame <- siteList(sos)
  .checkSitesDataFrameWithSitesWithData(sitesDataFrame)
})

test_that("KVP::siteList(sos) or siteList(sos, empty = FALSE) returns an empty list of stations as one column data.frame if GDA response is empty", {
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
  webmockr::stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetDataAvailability') %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetDataAvailability_100_Example.com_empty.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")

  sitesDataFrame <- siteList(sos, empty = FALSE)
  .checkEmptySitesDataFrame(sitesDataFrame)

  sitesDataFrame <- siteList(sos)
  .checkEmptySitesDataFrame(sitesDataFrame)
})

.checkTimeIntervalFilteredSitesDataFrame <- function(sitesDataFrame) {
  expect_false(is.null(sitesDataFrame))
  expect_true(is.data.frame(sitesDataFrame))
  expect_equal(length(colnames(sitesDataFrame)), 1, info = "number of columns in sites data.frame")
  expect_equal(colnames(sitesDataFrame)[[1]], "siteID", info = "correct column name")
  expect_equal(nrow(sitesDataFrame), 5, info = "number of unique sites")
  # check all values
  expect_equal("feature-1", sitesDataFrame[ 1, 1])
  expect_equal("feature-2", sitesDataFrame[ 2, 1])
  expect_equal("feature-3", sitesDataFrame[ 3, 1])
  expect_equal("feature-4", sitesDataFrame[ 4, 1])
  expect_equal("feature-6", sitesDataFrame[ 5, 1])
}

#
# ts1     : *   *   *  *     *
# ts2     : +  + +
# ts3     :     " " "
# ts4     :        = =  =  =  =
# ts5     :                ~ ~   ~
# ts6     :    ..........
# ts7     : ° °
# interval:    ||||||||||
#
# result 1: ts1, ts2, ts3, ts4, ts6
#
test_that("KVP::siteList(sos, begin, end) or siteList(sos, empty = FALSE, begin, end) return a list of stations that provide data at least 'touching' the given time window", {
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
  webmockr::stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetDataAvailability') %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetDataAvailability_100_Example.com_ForTimeFilter.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")

  .begin <- sosConvertTime(x = "1970-06-04T12:00:00.000Z", sos = sos)
  .end <- sosConvertTime(x = "1970-06-13T12:00:00.000Z", sos = sos)

  sitesDataFrame <- siteList(sos, empty = FALSE, begin = .begin, end = .end)
  .checkTimeIntervalFilteredSitesDataFrame(sitesDataFrame)

  sitesDataFrame <- siteList(sos, begin = .begin, end = .end)
  .checkTimeIntervalFilteredSitesDataFrame(sitesDataFrame)
})

.checkTimeIntervalFilteredSitesDataFrameWithMerge <- function(sitesDataFrame) {
  expect_false(is.null(sitesDataFrame))
  expect_true(is.data.frame(sitesDataFrame))
  expect_equal(length(colnames(sitesDataFrame)), 1, info = "number of columns in sites data.frame")
  expect_equal(colnames(sitesDataFrame)[[1]], "siteID", info = "correct column name")
  expect_equal(nrow(sitesDataFrame), 6, info = "number of unique sites")
  # check all values
  expect_equal("feature-1", sitesDataFrame[ 1, 1])
  expect_equal("feature-2", sitesDataFrame[ 2, 1])
  expect_equal("feature-3", sitesDataFrame[ 3, 1])
  expect_equal("feature-4", sitesDataFrame[ 4, 1])
  expect_equal("feature-6", sitesDataFrame[ 5, 1])
  expect_equal("feature-7", sitesDataFrame[ 6, 1])
}

test_that("KVP::siteList(sos, begin, end) or siteList(sos, empty = FALSE, begin, end) return a list of stations that provide data at least 'touching' the given time window", {
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
  webmockr::stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetDataAvailability') %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetDataAvailability_100_Example.com_ForTimeFilterWithMerge.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")

  .begin <- sosConvertTime(x = "1970-06-04T12:00:00.000Z", sos = sos)
  .end <- sosConvertTime(x = "1970-06-13T12:00:00.000Z", sos = sos)

  sitesDataFrame <- siteList(sos, empty = FALSE, begin = .begin, end = .end)
  .checkTimeIntervalFilteredSitesDataFrameWithMerge(sitesDataFrame)

  sitesDataFrame <- siteList(sos, begin = .begin, end = .end)
  .checkTimeIntervalFilteredSitesDataFrameWithMerge(sitesDataFrame)
})

#
# test KVP::siteList(sos, includePhenomena = TRUE, includeTemporalBBox = TRUE) ----
#
test_that("KVP::siteList(sos, includePhenomena = TRUE, includeTemporalBBox = TRUE) return a list of stations that provide data at least 'touching' the given time window with metadata", {
  skip("TODO implement: https://github.com/52North/sos4R/issues/90")
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
  webmockr::stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetDataAvailability') %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetDataAvailability_100_Example.com_ForTimeFilter.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")

  sitesDataFrame <- siteList(sos, empty = FALSE, includePhenomena = TRUE, includeTemporalBBox = TRUE)
  .checkSitesDataFrameWithTemporalBBox(sitesDataFrame)

  sitesDataFrame <- siteList(sos, includePhenomena = TRUE, includeTemporalBBox = TRUE)
  .checkSitesDataFrameWithTemporalBBox(sitesDataFrame)
})

webmockr::disable("httr")

context("siteList: integration tests\n")

test_that("siteList gives error if unsupported SOS version", {
  skip_on_cran()

  sos <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service",
             version = sos100_version, useDCPs = FALSE)
  expect_error(
    siteList(sos = sos),
    "unable to find an inherited method"
    )
})

test_that("can retrieve site list from an online SOS 2.0.0 (KVP)", {
  skip_on_cran()

  sos <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/kvp",
             version = sos200_version, binding = "KVP", useDCPs = FALSE)
  siteList <- siteList(sos = sos)

  expect_s3_class(siteList, "data.frame")
  expect_named(siteList, c("siteID"))
  expect_length(siteList$siteID, 5)
})
