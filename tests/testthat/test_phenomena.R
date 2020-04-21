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
# Created: 2019-03-20                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

webmockr::enable("httr")
webmockr::httr_mock()
context("convenience layer -> phenomena()")
#
# test KVP::phenomena(sos) ----
#
test_that("KVP::phenomena(sos) returns the current list of phenomena as one column data.frame", {
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
  dataFrameOfPhenomena <- phenomena(sos)

  expect_false(is.null(dataFrameOfPhenomena))
  expect_true(is.data.frame(dataFrameOfPhenomena))
  expect_equal(length(colnames(dataFrameOfPhenomena)), 1, info = "number of columns in phenomena data.frame")
  expect_equal(colnames(dataFrameOfPhenomena)[[1]], "phenomenon", info = "correct column name")
  expect_equal(nrow(dataFrameOfPhenomena), 33, info = "number of unique phenomena")
  # check first dozen values
  expect_equal("AirTemperature",        dataFrameOfPhenomena[ 1, 1])
  expect_equal("AthmosphericPressure",  dataFrameOfPhenomena[ 2, 1])
  expect_equal("Dewpoint",              dataFrameOfPhenomena[ 3, 1])
  expect_equal("HailAccumulated",       dataFrameOfPhenomena[ 4, 1])
  expect_equal("HailDuration",          dataFrameOfPhenomena[ 5, 1])
  expect_equal("HailIntensity",         dataFrameOfPhenomena[ 6, 1])
  expect_equal("HailPeakIntensity",     dataFrameOfPhenomena[ 7, 1])
  expect_equal("Humidity",              dataFrameOfPhenomena[ 8, 1])
  expect_equal("InSystemTemperature",   dataFrameOfPhenomena[ 9, 1])
  expect_equal("Luminance",             dataFrameOfPhenomena[10, 1])
  expect_equal("RainfallAccumulated",   dataFrameOfPhenomena[11, 1])
  expect_equal("RainfallDuration",      dataFrameOfPhenomena[12, 1])
})
#
# test KVP::phenomena(sos) empty sos ----
#
test_that("KVP::phenomena(sos) returns an empty list of phenomena as one column data.frame if the SOS is empty", {
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

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena-empty", binding = "KVP")
  dataFrameOfPhenomena <- phenomena(sos)

  expect_true(!is.null(dataFrameOfPhenomena))
  expect_true(is.data.frame(dataFrameOfPhenomena))
  expect_equal(length(colnames(dataFrameOfPhenomena)), 1, info = "number of columns in phenomena data.frame")
  expect_equal(colnames(dataFrameOfPhenomena)[[1]], "phenomenon", info = "correct column name")
  expect_equal(nrow(dataFrameOfPhenomena), 0, info = "number of unique phenomena")
})
#
# test KVP::phenomena(sos, includeTemporalBBox = TRUE) with empty sos ----
#
test_that("KVP::phenomena(sos, includeTemporalBBox = TRUE) returns an empty list of phenomena as three column data.frame if the SOS is empty", {
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
  webmockr::stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetDataAvailability') %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetDataAvailability_100_Example.com_empty.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena-empty", binding = "KVP")
  dataFrameOfPhenomena <- phenomena(sos, includeTemporalBBox = TRUE)

  expect_false(is.null(dataFrameOfPhenomena))
  expect_true(is.data.frame(dataFrameOfPhenomena))
  expect_equal(length(colnames(dataFrameOfPhenomena)), 3, info = "number of columns in phenomena data.frame")
  expect_equal(colnames(dataFrameOfPhenomena)[[1]], "phenomenon", info = "correct column name")
  expect_equal(colnames(dataFrameOfPhenomena)[[2]], "timeBegin", info = "correct column name")
  expect_equal(colnames(dataFrameOfPhenomena)[[3]], "timeEnd", info = "correct column name")
  expect_equal(nrow(dataFrameOfPhenomena), 0, info = "number of unique phenomena")
})
#
# test KVP::phenomena(sos, includeTemporalBBox = TRUE) ----
#
test_that("KVP::phenomena(sos, includeTemporalBBox = TRUE) returns the current list of phenomena as three column data.frame", {
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
      body = readr::read_file("../responses/GetDataAvailability_100_Example.com_merge.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")
  dataFrameOfPhenomena <- phenomena(sos, includeTemporalBBox = TRUE)

  expect_true(!is.null(dataFrameOfPhenomena))
  expect_true(is.data.frame(dataFrameOfPhenomena))
  expect_equal(length(colnames(dataFrameOfPhenomena)), 3, info = "number of columns in phenomena data.frame")
  expect_equal(colnames(dataFrameOfPhenomena)[[1]], "phenomenon", info = "column #1 name")
  expect_equal(colnames(dataFrameOfPhenomena)[[2]], "timeBegin", info = "column #2 name")
  expect_equal(colnames(dataFrameOfPhenomena)[[3]], "timeEnd", info = "column #3 name")
  expect_equal(nrow(dataFrameOfPhenomena), 2, info = "number of unique phenomena")
  # check all values
  expect_equal("WindDirection", dataFrameOfPhenomena[ 1, 1], info = "phenomenon")
  expect_equal("2019-03-01T00:30:00+00:00", parsedate::format_iso_8601(dataFrameOfPhenomena[ 1, 2]), info = "timeBegin")
  expect_equal("2019-04-28T23:45:00+00:00", parsedate::format_iso_8601(dataFrameOfPhenomena[ 1, 3]), info = "timeEnd")
  expect_equal("WindDirectionTest", dataFrameOfPhenomena[ 2, 1], info = "phenomenon")
  expect_equal("2019-03-01T00:30:00+00:00", parsedate::format_iso_8601(dataFrameOfPhenomena[ 2, 2]), info = "timeBegin")
  expect_equal("2019-05-28T23:45:00+00:00", parsedate::format_iso_8601(dataFrameOfPhenomena[ 2, 3]), info = "timeEnd")
})
#
# test KVP::phenomena(sos, includeSiteId = TRUE) ----
#
test_that("KVP::phenomena(sos, includeSiteId = TRUE) returns a data.frame with two columns with phenomenon id and site id.", {
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
      body = readr::read_file("../responses/GetDataAvailability_100_Example.com_short.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")
  dataFrameOfPhenomena <- phenomena(sos, includeSiteId = TRUE)

  expect_true(!is.null(dataFrameOfPhenomena))
  expect_true(is.data.frame(dataFrameOfPhenomena))
  expect_equal(length(colnames(dataFrameOfPhenomena)), 2, info = "number of columns in phenomena data.frame")
  expect_equal(colnames(dataFrameOfPhenomena)[[1]], "phenomenon", info = "column #1 name")
  expect_equal(colnames(dataFrameOfPhenomena)[[2]], "siteID", info = "column #2 name")
  expect_equal(nrow(dataFrameOfPhenomena), 15, info = "number of phenomenon and site pairs")
  # check all values
  expect_equal("phen-1", dataFrameOfPhenomena[ 1, 1])
  expect_equal("phen-1", dataFrameOfPhenomena[ 2, 1])
  expect_equal("phen-2", dataFrameOfPhenomena[ 3, 1])
  expect_equal("phen-3", dataFrameOfPhenomena[ 4, 1])
  expect_equal("phen-3", dataFrameOfPhenomena[ 5, 1])
  expect_equal("phen-3", dataFrameOfPhenomena[ 6, 1])
  expect_equal("phen-3", dataFrameOfPhenomena[ 7, 1])
  expect_equal("phen-4", dataFrameOfPhenomena[ 8, 1])
  expect_equal("phen-5", dataFrameOfPhenomena[ 9, 1])
  expect_equal("phen-6", dataFrameOfPhenomena[10, 1])
  expect_equal("phen-6", dataFrameOfPhenomena[11, 1])
  expect_equal("phen-6", dataFrameOfPhenomena[12, 1])
  expect_equal("phen-6", dataFrameOfPhenomena[13, 1])
  expect_equal("phen-7", dataFrameOfPhenomena[14, 1])
  expect_equal("phen-8", dataFrameOfPhenomena[15, 1])

  expect_equal("station-1", dataFrameOfPhenomena[ 1, 2])
  expect_equal("station-2", dataFrameOfPhenomena[ 2, 2])
  expect_equal("station-2", dataFrameOfPhenomena[ 3, 2])
  expect_equal("station-2", dataFrameOfPhenomena[ 4, 2])
  expect_equal("station-1", dataFrameOfPhenomena[ 5, 2])
  expect_equal("station-4", dataFrameOfPhenomena[ 6, 2])
  expect_equal("station-3", dataFrameOfPhenomena[ 7, 2])
  expect_equal("station-2", dataFrameOfPhenomena[ 8, 2])
  expect_equal("station-2", dataFrameOfPhenomena[ 9, 2])
  expect_equal("station-3", dataFrameOfPhenomena[10, 2])
  expect_equal("station-2", dataFrameOfPhenomena[11, 2])
  expect_equal("station-1", dataFrameOfPhenomena[12, 2])
  expect_equal("station-4", dataFrameOfPhenomena[13, 2])
  expect_equal("station-2", dataFrameOfPhenomena[14, 2])
  expect_equal("station-2", dataFrameOfPhenomena[15, 2])
})
#
# test KVP::phenomena(sos, includeSiteId = TRUE) #2 ----
#
test_that("KVP::phenomena(sos, includeSiteId = TRUE) returns a data.frame with two columns with phenomenon id and site id #2.", {
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
      body = readr::read_file("../responses/GetDataAvailability_100_Example.com_double.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")
  dataFrameOfPhenomena <- phenomena(sos, includeSiteId = TRUE)

  expect_true(!is.null(dataFrameOfPhenomena))
  expect_true(is.data.frame(dataFrameOfPhenomena))
  expect_equal(length(colnames(dataFrameOfPhenomena)), 2, info = "number of columns in phenomena data.frame")
  expect_equal(colnames(dataFrameOfPhenomena)[[1]], "phenomenon", info = "column #1 name")
  expect_equal(colnames(dataFrameOfPhenomena)[[2]], "siteID", info = "column #2 name")
  expect_equal(nrow(dataFrameOfPhenomena), 2, info = "number of phenomenon and site pairs")
  # check all values
  expect_equal("WindDirection", dataFrameOfPhenomena[ 1, 1])
  expect_equal("WindDirection", dataFrameOfPhenomena[ 2, 1])

  expect_equal("elv-ws2500",   dataFrameOfPhenomena[ 1, 2])
  expect_equal("elv-ws2500-2", dataFrameOfPhenomena[ 2, 2])
})
#
# test KVP::phenomena(sos, includeTemporalBBox = TRUE, includeSiteId = TRUE) ----
#
test_that("KVP::phenomena(sos, includeTemporalBBox = TRUE, includeSiteId = TRUE)", {
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
      body = readr::read_file("../responses/GetDataAvailability_100_Example.com_double.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")
  dataFrameOfPhenomena <- phenomena(sos, includeTemporalBBox = TRUE, includeSiteId = TRUE)

  expect_true(!is.null(dataFrameOfPhenomena))
  expect_true(is.data.frame(dataFrameOfPhenomena))
  expect_equal(length(colnames(dataFrameOfPhenomena)), 4, info = "number of columns in phenomena data.frame")
  expect_equal(colnames(dataFrameOfPhenomena)[[1]], "phenomenon", info = "column #1 name")
  expect_equal(colnames(dataFrameOfPhenomena)[[2]], "siteID", info = "column #2 name")
  expect_equal(colnames(dataFrameOfPhenomena)[[3]], "timeBegin", info = "column #3 name")
  expect_equal(colnames(dataFrameOfPhenomena)[[4]], "timeEnd", info = "column #4 name")
  expect_equal(nrow(dataFrameOfPhenomena), 2, info = "number of phenomenon and site pairs")
  # check all values
  expect_equal("WindDirection", dataFrameOfPhenomena[ 1, 1])
  expect_equal("WindDirection", dataFrameOfPhenomena[ 2, 1])

  expect_equal("elv-ws2500",   dataFrameOfPhenomena[ 1, 2])
  expect_equal("elv-ws2500-2", dataFrameOfPhenomena[ 2, 2])

  expect_equal("2019-03-01T00:30:00+00:00", parsedate::format_iso_8601(dataFrameOfPhenomena[ 1, 3]))
  expect_equal("2019-03-01T00:30:00+00:00", parsedate::format_iso_8601(dataFrameOfPhenomena[ 2, 3]))

  expect_equal("2019-03-28T23:45:00+00:00", parsedate::format_iso_8601(dataFrameOfPhenomena[ 1, 4]))
  expect_equal("2019-03-28T23:45:00+00:00", parsedate::format_iso_8601(dataFrameOfPhenomena[ 2, 4]))
})

webmockr::disable("httr")

context("phenomena: integration tests\n")
#
# test gives error if unsupported SOS version ----
#
test_that("gives error if unsupported SOS version", {
  skip_on_cran()

  sos <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service",
             version = sos100_version, useDCPs = FALSE)
  expect_error(
    phenomena(sos = sos),
    "unable to find an inherited method"
  )
})
#
# test can retrieve phenomena from an online SOS 2.0.0 (KVP) ----
#
test_that("can retrieve phenomena from an online SOS 2.0.0 (KVP)", {
  skip_on_cran()

  sos <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/kvp",
             version = sos200_version, binding = "KVP", useDCPs = FALSE)
  phen <- phenomena(sos = sos)

  expect_s3_class(phen, "data.frame")
  expect_named(phen, c("phenomenon"))
  expect_length(phen$phenomenon, 33)
})

