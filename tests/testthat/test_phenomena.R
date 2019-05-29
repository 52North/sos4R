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
  # check all values
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
  expect_equal("RainfallIntensity",     dataFrameOfPhenomena[13, 1])
  expect_equal("RainfallPeakIntensity", dataFrameOfPhenomena[14, 1])
  expect_equal("ShortwaveRadiation",    dataFrameOfPhenomena[15, 1])
  expect_equal("Sunshine",              dataFrameOfPhenomena[16, 1])
  expect_equal("Visibility",            dataFrameOfPhenomena[17, 1])
  expect_equal("WeatherCode",           dataFrameOfPhenomena[18, 1])
  expect_equal("WeatherCodeText",       dataFrameOfPhenomena[19, 1])
  expect_equal("Windchill",             dataFrameOfPhenomena[20, 1])
  expect_equal("WindDirection",         dataFrameOfPhenomena[21, 1])
  expect_equal("WindDirectionAverage",  dataFrameOfPhenomena[22, 1])
  expect_equal("WindDirectionMaximum",  dataFrameOfPhenomena[23, 1])
  expect_equal("WindDirectionMinimum",  dataFrameOfPhenomena[24, 1])
  expect_equal("WindDirectionText",     dataFrameOfPhenomena[25, 1])
  expect_equal("WindMaxGust",           dataFrameOfPhenomena[26, 1])
  expect_equal("WindSpeed",             dataFrameOfPhenomena[27, 1])
  expect_equal("WindSpeedAverage",      dataFrameOfPhenomena[28, 1])
  expect_equal("WindSpeedBft",          dataFrameOfPhenomena[29, 1])
  expect_equal("WindSpeedKmh",          dataFrameOfPhenomena[30, 1])
  expect_equal("WindSpeedMaximum",      dataFrameOfPhenomena[31, 1])
  expect_equal("WindSpeedMinimum",      dataFrameOfPhenomena[32, 1])
  expect_equal("WindSpeedMperSec",      dataFrameOfPhenomena[33, 1])
})

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

test_that("KVP::phenomena(sos, includeTemporalBBox) returns an empty list of phenomena as three column data.frame if the SOS is empty", {
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

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena-empty", binding = "KVP", verboseOutput = FALSE)
  dataFrameOfPhenomena <- phenomena(sos, includeTemporalBBox = TRUE)

  expect_false(is.null(dataFrameOfPhenomena))
  expect_true(is.data.frame(dataFrameOfPhenomena))
  expect_equal(length(colnames(dataFrameOfPhenomena)), 3, info = "number of columns in phenomena data.frame")
  expect_equal(colnames(dataFrameOfPhenomena)[[1]], "phenomenon", info = "correct column name")
  expect_equal(colnames(dataFrameOfPhenomena)[[2]], "timeBegin", info = "correct column name")
  expect_equal(colnames(dataFrameOfPhenomena)[[3]], "timeEnd", info = "correct column name")
  expect_equal(nrow(dataFrameOfPhenomena), 0, info = "number of unique phenomena")
})

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

webmockr::disable("httr")

context("phenomena: integration tests\n")

test_that("gives error if unsupported SOS version", {
  skip_on_cran()

  sos <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service",
             version = sos100_version, useDCPs = FALSE)
  expect_error(
    phenomena(sos = sos),
    "unable to find an inherited method"
  )
})

test_that("can retrieve phenomena from an online SOS 2.0.0 (KVP)", {
  skip_on_cran()

  sos <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/kvp",
             version = sos200_version, binding = "KVP", useDCPs = FALSE)
  phen <- phenomena(sos = sos)

  expect_s3_class(phen, "data.frame")
  expect_named(phen, c("phenomenon"))
  expect_length(phen$phenomenon, 33)
})

