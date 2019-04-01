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
# Created: 2019-04-01                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################
library(webmockr)
library(httr)
library(stringr)
webmockr::enable("httr")
webmockr::httr_mock()
context("convenience layer -> siteList()")

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

  stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetFeatureOfInterest') %>%
    wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetFeatureOfInterest_200_Example.com_empty.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena-empty", binding = "KVP")
  sitesDataFrame <- siteList(sos, empty = TRUE)

  expect_false(is.null(sitesDataFrame))
  expect_true(is.data.frame(sitesDataFrame))
  expect_equal(length(colnames(sitesDataFrame)), 1, info = "number of columns in sites data.frame")
  expect_equal(colnames(sitesDataFrame)[[1]], "siteID", info = "correct column name")
  expect_equal(nrow(sitesDataFrame), 0, info = "number of unique sites")
})

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
  stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetFeatureOfInterest') %>%
    wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetFeatureOfInterest_200_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")
  sitesDataFrame <- siteList(sos, empty = TRUE)

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
})

