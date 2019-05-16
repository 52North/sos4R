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
# Created: 2019-04-04                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################
library(webmockr)
library(httr)
library(stringr)
webmockr::enable("httr")
webmockr::httr_mock()
context("convenience layer -> sites()")

.checkEmptySitesDataFrame <- function(sitesDataFrame) {
  expect_false(is.null(sitesDataFrame))
  expect_true(inherits(sitesDataFrame, "SpatialPointsDataFrame"))
  expect_length(colnames(sitesDataFrame@data), 1)
  expect_equal(colnames(sitesDataFrame@data)[[1]], "siteID", info = "correct column name")
  expect_equal(nrow(sitesDataFrame@data), 0, info = "number of unique sites")
  expect_length(sitesDataFrame@coords.nrs, 0)
}

.checkSitesDataFrameWithIdsAndCoords <- function(sitesDataFrame) {
  expect_false(is.null(sitesDataFrame))
  expect_true(inherits(sitesDataFrame, "SpatialPointsDataFrame"))
  data <- sitesDataFrame@data
  expect_length(colnames(data), 1)
  expect_equal(colnames(data)[[1]], "siteID", info = "correct column name")
  expect_equal(nrow(sitesDataFrame), 4, info = "number of unique sites")
  # check all values
  expect_equal("elv-ws2500",          data[ 1, 1])
  expect_equal("elv-ws2500-internal", data[ 2, 1])
  expect_equal("vaisala-wxt520",      data[ 3, 1])
  expect_equal("wwu-ws-kli-hsb",      data[ 4, 1])
  expect_equal("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", sitesDataFrame@proj4string@projargs, "CRS correct defined")
  coords <- sitesDataFrame@coords
  expect_equal("lon", colnames(coords)[[1]])
  expect_equal("lat", colnames(coords)[[2]])
  expect_equal(4, nrow(coords))
  expect_true(7.652428150177   == coords[1,1])
  expect_true(51.934814453125  == coords[1,2])
  expect_true(7.65234184265137 == coords[2,1])
  expect_true(51.934814453125  == coords[2,2])
  expect_true(7.65237522125244 == coords[3,1])
  expect_true(51.9347763061523 == coords[3,2])
  expect_true(7.59587907791138 == coords[4,1])
  expect_true(51.9692611694336 == coords[4,2])
}

test_that("KVP::sites(sos, empty = TRUE|FALSE) returns an empty list of sites as SpatialPointsDataFrame if the SOS is empty", {
  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://example.com/sos-list-phenomena?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
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

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")

  sitesDataFrame <- sites(sos)
  .checkEmptySitesDataFrame(sitesDataFrame)

  sitesDataFrame <- sites(sos, empty = TRUE)
  .checkEmptySitesDataFrame(sitesDataFrame)

  sitesDataFrame <- sites(sos, empty = FALSE)
  .checkEmptySitesDataFrame(sitesDataFrame)
})

test_that("KVP::sites(sos, empty = TRUE|FALSE) returns a list of stations as SpatialPointsDataFrame that contain siteIDs and coordinates", {
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
  stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetDataAvailability') %>%
    wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetDataAvailability_100_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")

  sitesDataFrame <- sites(sos, empty = TRUE)
  .checkSitesDataFrameWithIdsAndCoords(sitesDataFrame)
})

webmockr::disable("httr")
