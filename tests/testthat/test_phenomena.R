################################################################################
# Copyright (C) 2015 by 52 North                                               #
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
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################
library(webmockr)
library(httr)
library(stringr)
webmockr::enable("httr")
webmockr::httr_mock()
context("convenience layer -> phenomena()")

test_that("phenomena() returns the current list of phenomena as one column data.frame", {
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

  expect_true(!is.null(dataFrameOfPhenomena))
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
# test_that("phenomena() with wrong inputs stops", {
#   expect_error(
#     phenomena(SOS_Test()),
#     "unable to find an inherited method for function 'phenomena' for signature '\"SOS_Test\"'",
#     fixed=TRUE)
#
#   expect_error(
#     phenomena(SOS_2.0.0_Test(), includeTemporalBBox = 2.0),
#     "is.logical(includeTemporalBBox) is not TRUE",
#     fixed=TRUE)
#
#   expect_error(
#     phenomena(SOS_2.0.0_Test(), includeSiteId = 2.0),
#     "is.logical(includeSiteId) is not TRUE",
#     fixed=TRUE)
#
#   expect_error(
#     phenomena(SOS_2.0.0_Test(), includeSiteId = 2.0, includeTemporalBBox = 2.0),
#     "is.logical(includeTemporalBBox) is not TRUE",
#     fixed=TRUE)
#
#   expect_error(
#     phenomena(SOS_2.0.0_Test(), includeSiteId = 2.0, includeTemporalBBox = FALSE),
#     "is.logical(includeSiteId) is not TRUE",
#     fixed=TRUE)
# })
