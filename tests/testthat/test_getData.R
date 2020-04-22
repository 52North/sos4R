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
# Author: Daniel Nüst (daniel.nuest@uni-muenster.de)                           #
#         Eike Hinderk Jürrens (e.h.juerrens@52north.org)                      #
# Created: 2019-05-16                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

webmockr::enable("httr")
webmockr::httr_mock()
context("convenience layer -> getData()")

emptyGetObservationResponse <- '<?xml version="1.0" encoding="UTF-8"?>
<sos:GetObservationResponse xmlns:sos="http://www.opengis.net/sos/2.0" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.opengis.net/waterml/2.0 http://schemas.opengis.net/waterml/2.0/waterml2.xsd http://www.opengis.net/waterml/2.0 http://schemas.opengis.net/waterml/2.0/timeseries.xsd http://www.opengis.net/sos/2.0 http://schemas.opengis.net/sos/2.0/sos.xsd"/>'

# download mock responses
#niwaHydro <- SOS(url = "https://hydro-sos.niwa.co.nz/", binding = "KVP", useDCPs = FALSE, version = "2.0.0")
#obs <- getObservation(sos = niwaHydro,
#               offering = sosOfferings(niwaHydro)[1:10],
#               observedProperty = list("Height of Gauge (River Stage)"),
#               eventTime = sosCreateTime(sos = niwaHydro, time = "2019-05-01 00:00:00::2019-05-01 01:00:00"),
#               saveOriginal = "tests/responses/getData_gauge_niwa.xml")
webmockr::stub_registry_clear()
webmockr::stub_request("get", uri = "http://example.com/sos-get-data?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
  webmockr::wi_th(
    headers = list("Accept" = "application/xml")
  ) %>%
  webmockr::to_return(
    status = 200,
    body = readr::read_file("../responses/hydro-sos.niwa.co.nz.xml"),
    headers = list("Content-Type" = "application/xml")
  )
webmockr::stub_request("get", uri = "http://example.com/sos-get-data?service=SOS&version=2.0.0&request=GetObservation&observedProperty=pH&featureOfInterest=29808") %>%
  webmockr::wi_th(
    headers = list("Accept" = "application/xml")
  ) %>%
  webmockr::to_return(
    status = 200,
    body = emptyGetObservationResponse,
    headers = list("Content-Type" = "application/xml")
  )
webmockr::stub_request("get", uri = "http://example.com/sos-get-data?service=SOS&version=2.0.0&request=GetObservation&observedProperty=Discharge&featureOfInterest=29808") %>%
  webmockr::wi_th(
    headers = list("Accept" = "application/xml")
  ) %>%
  webmockr::to_return(
    status = 200,
    body = readr::read_file("../responses/hydro-sos.niwa.co.nz_Discharge_29808.om.xml"),
    headers = list("Content-Type" = "application/xml")
  )
webmockr::stub_request("get", uri = ".*GetObservation.*") %>%
  webmockr::wi_th(
    headers = list("Accept" = "application/xml")
  ) %>%
  webmockr::to_return(
    status = 200,
    body = readr::read_file("../responses/getData_gauge_niwa.xml"),
    headers = list("Content-Type" = "application/xml")
  )
#
# KVP::getData(sos) returns data values in data.frame ----
#
test_that("KVP::getData(sos) returns data values in data.frame", {
  sos <- SOS(version = sos200_version, url = "http://example.com/sos-get-data", binding = "KVP", useDCPs = FALSE)

  observationData <- getData(sos, phenomena = c("Discharge"), sites = c("29808"), retrieveFOI = FALSE)

  expect_equal(dim(observationData), c(1,3))
  expect_named(observationData, c("siteID", "timestamp", "m^3/s"))
})
#
# KVP::getData(sos) stops if inputs are incorrect ----
#
test_that("KVP::getData(sos) stops if inputs are incorrect", {
  skip("Not implemented!")

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-get-data", binding = "KVP", useDCPs = FALSE)

  expect_error(getData(sos, phenomena = c(), sites = c("test")),
               "is not TRUE")
  expect_error(getData(sos, phenomena = NULL, sites = c("test")),
               "is not TRUE")
  expect_error(getData(sos, phenomena = c(NA_character_, "test"), sites = c("test")),
               "is not TRUE")
  expect_error(getData(sos, phenomena = c("test"), sites = NULL),
               "is not TRUE")
  expect_error(getData(sos, phenomena = c("test"), sites = c()),
               "is not TRUE")
  expect_error(getData(sos, phenomena = c("test"), sites = c(NA, "test")),
               "is not TRUE")

  expect_error(getData(sos, phenomena = list("test"), sites = list(NA, "test")),
               "is not TRUE")
  expect_error(getData(sos, phenomena = list(), sites = c("test")),
               "is not TRUE")
  expect_error(getData(sos, phenomena = list("test"), sites = list()),
               "is not TRUE")
})
#
# KVP::getData(sos) returns empty data.frame if no data found ----
#
test_that("KVP::getData(sos) returns empty data.frame if no data found", {
  sos <- SOS(version = sos200_version, url = "http://example.com/sos-get-data", binding = "KVP", useDCPs = FALSE)
  observationData <- getData(sos = sos, phenomena = c("pH"), sites = c("29808"))
  expect_true(is.data.frame(observationData))
  expect_equal(dim(observationData), c(0, 0))
})
#
# KVP::getData(sos) returns empty data.frame if no data found (list input) ----
#
test_that("KVP::getData(sos) returns empty data.frame if no data found (list input)", {
  sos <- SOS(version = sos200_version, url = "http://example.com/sos-get-data", binding = "KVP", useDCPs = FALSE)
  observationData <- getData(sos = sos, phenomena = list("pH"), sites = list("29808"))
  expect_true(is.data.frame(observationData))
  expect_equal(dim(observationData), c(0, 0))
})
#
# KVP::getData(sos) passes verbose paramter on ----
#
test_that("KVP::getData(sos) passes verbose paramter on", {
  sos <- SOS(version = sos200_version, url = "http://example.com/sos-get-data", binding = "KVP", useDCPs = FALSE)
  output <- capture_output(getData(sos = sos, phenomena = c("pH"), sites = c("29808"), verbose = TRUE))
  expect_match(output, "Requesting list of offerings")
  expect_match(output, "Finished getObservation to http://example.com/sos-get-data")
})
#
# test siteIDs, column metadata, and correct values ----
#
test_that("KVP::getData() provides the siteIDs, column metadata, and correct values", {
  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://example.com/sos-get-data?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
    webmockr::wi_th(
      headers = list("Accept" = "application/xml")
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/hydro-sos.niwa.co.nz.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  webmockr::stub_request('get', uri = 'http://example.com/sos-get-data?service=SOS&version=2.0.0&request=GetObservation&observedProperty=MTHLY_STATS__EXTREME_MAXIMUM_TEMPERATURE__MTHLY__EXTR_MAX_TEMP_&featureOfInterest=1056,11234') %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetObservationResponse_200_sweDataArray.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  webmockr::stub_request('get', uri = 'http://example.com/sos-get-data?service=SOS&version=2.0.0&request=GetFeatureOfInterest&featureOfInterest=1056') %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetFeatureOfInterestResponse_200_niwa_1056.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  webmockr::stub_request('get', uri = 'http://example.com/sos-get-data?service=SOS&version=2.0.0&request=GetFeatureOfInterest&featureOfInterest=11234') %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetFeatureOfInterestResponse_200_niwa_11234.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-get-data", binding = "KVP", useDCPs = FALSE)
  observationData <- getData(sos = sos, phenomena = c("MTHLY_STATS__EXTREME_MAXIMUM_TEMPERATURE__MTHLY__EXTR_MAX_TEMP_"), sites = c("1056", "11234"), verbose = FALSE)
  colnames <- colnames(observationData)
  #
  # test meta data
  #
  expect_length(colnames, 3)
  expect_equal(colnames[1], "siteID")
  expect_equal(colnames[2], "timestamp")
  expect_equal(colnames[3], "MTHLY_STATS__EXTREME_MAXIMUM_TEMPERATURE__MTHLY__EXTR_MAX_TEMP_")
  attributes <- attributes(observationData[[3]])
  expect_equal(attributes$name, "MTHLY_STATS__EXTREME_MAXIMUM_TEMPERATURE__MTHLY__EXTR_MAX_TEMP_")
  expect_equal(attributes$definition, "MTHLY_STATS: EXTREME MAXIMUM TEMPERATURE (MTHLY: EXTR MAX TEMP)")
  expect_equal(attributes$"unit of measurement", "DEGREE_CELSIUS")
  #
  # test data
  #
  expect_length(observationData[[1]], 26)

  expect_equal(as.character(observationData[[ 1,1]]), "1056")
  expect_equal(as.character(observationData[[ 2,1]]), "1056")
  expect_equal(as.character(observationData[[ 3,1]]), "1056")
  expect_equal(as.character(observationData[[ 4,1]]), "1056")
  expect_equal(as.character(observationData[[ 5,1]]), "1056")
  expect_equal(as.character(observationData[[ 6,1]]), "1056")
  expect_equal(as.character(observationData[[ 7,1]]), "1056")
  expect_equal(as.character(observationData[[ 8,1]]), "1056")
  expect_equal(as.character(observationData[[ 9,1]]), "1056")
  expect_equal(as.character(observationData[[10,1]]), "1056")
  expect_equal(as.character(observationData[[11,1]]), "1056")
  expect_equal(as.character(observationData[[12,1]]), "1056")
  expect_equal(as.character(observationData[[13,1]]), "1056")
  expect_equal(as.character(observationData[[14,1]]), "11234")
  expect_equal(as.character(observationData[[15,1]]), "11234")
  expect_equal(as.character(observationData[[16,1]]), "11234")
  expect_equal(as.character(observationData[[17,1]]), "11234")
  expect_equal(as.character(observationData[[18,1]]), "11234")
  expect_equal(as.character(observationData[[19,1]]), "11234")
  expect_equal(as.character(observationData[[20,1]]), "11234")
  expect_equal(as.character(observationData[[21,1]]), "11234")
  expect_equal(as.character(observationData[[22,1]]), "11234")
  expect_equal(as.character(observationData[[23,1]]), "11234")
  expect_equal(as.character(observationData[[24,1]]), "11234")
  expect_equal(as.character(observationData[[25,1]]), "11234")
  expect_equal(as.character(observationData[[26,1]]), "11234")

  expect_equal(observationData[[ 1,2]], parsedate::parse_iso_8601("1999-01"))
  expect_equal(observationData[[ 2,2]], parsedate::parse_iso_8601("1999-02"))
  expect_equal(observationData[[ 3,2]], parsedate::parse_iso_8601("1999-03"))
  expect_equal(observationData[[ 4,2]], parsedate::parse_iso_8601("1999-04"))
  expect_equal(observationData[[ 5,2]], parsedate::parse_iso_8601("1999-05"))
  expect_equal(observationData[[ 6,2]], parsedate::parse_iso_8601("1999-06"))
  expect_equal(observationData[[ 7,2]], parsedate::parse_iso_8601("1999-07"))
  expect_equal(observationData[[ 8,2]], parsedate::parse_iso_8601("1999-08"))
  expect_equal(observationData[[ 9,2]], parsedate::parse_iso_8601("1999-09"))
  expect_equal(observationData[[10,2]], parsedate::parse_iso_8601("1999-10"))
  expect_equal(observationData[[11,2]], parsedate::parse_iso_8601("1999-11"))
  expect_equal(observationData[[12,2]], parsedate::parse_iso_8601("1999-12"))
  expect_equal(observationData[[13,2]], parsedate::parse_iso_8601("2000-01"))
  expect_equal(observationData[[14,2]], parsedate::parse_iso_8601("1999-01"))
  expect_equal(observationData[[15,2]], parsedate::parse_iso_8601("1999-02"))
  expect_equal(observationData[[16,2]], parsedate::parse_iso_8601("1999-03"))
  expect_equal(observationData[[17,2]], parsedate::parse_iso_8601("1999-04"))
  expect_equal(observationData[[18,2]], parsedate::parse_iso_8601("1999-05"))
  expect_equal(observationData[[19,2]], parsedate::parse_iso_8601("1999-06"))
  expect_equal(observationData[[20,2]], parsedate::parse_iso_8601("1999-07"))
  expect_equal(observationData[[21,2]], parsedate::parse_iso_8601("1999-08"))
  expect_equal(observationData[[22,2]], parsedate::parse_iso_8601("1999-09"))
  expect_equal(observationData[[23,2]], parsedate::parse_iso_8601("1999-10"))
  expect_equal(observationData[[24,2]], parsedate::parse_iso_8601("1999-11"))
  expect_equal(observationData[[25,2]], parsedate::parse_iso_8601("1999-12"))
  expect_equal(observationData[[26,2]], parsedate::parse_iso_8601("2000-01"))
  #
  # consider the quite impressive measurment precision ;-)
  #
  expect_equal(observationData[[ 1,3]], 29)
  expect_equal(observationData[[ 2,3]], 26.89999999999999857891452847979962825775146484375)
  expect_equal(observationData[[ 3,3]], 25.300000000000000710542735760100185871124267578125)
  expect_equal(observationData[[ 4,3]], 25.5)
  expect_equal(observationData[[ 5,3]], 21.89999999999999857891452847979962825775146484375)
  expect_equal(observationData[[ 6,3]], 20.699999999999999289457264239899814128875732421875)
  expect_equal(observationData[[ 7,3]], 19.89999999999999857891452847979962825775146484375)
  expect_equal(observationData[[ 8,3]], 18.60000000000000142108547152020037174224853515625)
  expect_equal(observationData[[ 9,3]], 21.800000000000000710542735760100185871124267578125)
  expect_equal(observationData[[10,3]], 22.800000000000000710542735760100185871124267578125)
  expect_equal(observationData[[11,3]], 23.10000000000000142108547152020037174224853515625)
  expect_equal(observationData[[12,3]], 25.39999999999999857891452847979962825775146484375)
  expect_equal(observationData[[13,3]], 25.89999999999999857891452847979962825775146484375)
  expect_equal(observationData[[14,3]], 33.7999999999999971578290569595992565155029296875)
  expect_equal(observationData[[15,3]], 31.60000000000000142108547152020037174224853515625)
  expect_equal(observationData[[16,3]], 29.800000000000000710542735760100185871124267578125)
  expect_equal(observationData[[17,3]], 26.699999999999999289457264239899814128875732421875)
  expect_equal(observationData[[18,3]], 22.300000000000000710542735760100185871124267578125)
  expect_equal(observationData[[19,3]], 18.39999999999999857891452847979962825775146484375)
  expect_equal(observationData[[20,3]], 15.699999999999999289457264239899814128875732421875)
  expect_equal(observationData[[21,3]], 18.39999999999999857891452847979962825775146484375)
  expect_equal(observationData[[22,3]], 23.5)
  expect_equal(observationData[[23,3]], 24.199999999999999289457264239899814128875732421875)
  expect_equal(observationData[[24,3]], 26.10000000000000142108547152020037174224853515625)
  expect_equal(observationData[[25,3]], 26.5)
  expect_equal(observationData[[26,3]], 29.199999999999999289457264239899814128875732421875)
})
#
# test merge with two ts with same feature but different phenomenon ----
#
test_that("KVP::test merge with two ts with same feature but different phenomenon", {
  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://example.com/sos-get-data?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
    webmockr::wi_th(
      headers = list("Accept" = "application/xml")
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/hydro-sos.niwa.co.nz.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  webmockr::stub_request('get', uri = 'http://example.com/sos-get-data?service=SOS&version=2.0.0&request=GetObservation&observedProperty=MTHLY_STATS__EXTREME_MAXIMUM_TEMPERATURE__MTHLY__EXTR_MAX_TEMP_&featureOfInterest=1056') %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetObservationResponse_200_sweDataArray_two_phenomena.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  webmockr::stub_request('get', uri = 'http://example.com/sos-get-data?service=SOS&version=2.0.0&request=GetFeatureOfInterest&featureOfInterest=1056') %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetFeatureOfInterestResponse_200_niwa_1056.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-get-data", binding = "KVP", useDCPs = FALSE)
  observationData <- getData(sos = sos, phenomena = c("MTHLY_STATS__EXTREME_MAXIMUM_TEMPERATURE__MTHLY__EXTR_MAX_TEMP_"), sites = c("1056"), verbose = FALSE)
  colnames <- colnames(observationData)
  #
  # test meta data
  #
  expect_length(colnames, 4)
  expect_equal(colnames[1], "siteID")
  expect_equal(colnames[2], "timestamp")
  expect_equal(colnames[3], "MTHLY_STATS__EXTREME_MAXIMUM_TEMPERATURE__MTHLY__EXTR_MAX_TEMP_")
  expect_equal(colnames[4], "secondPhenomenon")

  attributes <- attributes(observationData[[3]])
  expect_equal(attributes$name, "MTHLY_STATS__EXTREME_MAXIMUM_TEMPERATURE__MTHLY__EXTR_MAX_TEMP_")
  expect_equal(attributes$definition, "MTHLY_STATS: EXTREME MAXIMUM TEMPERATURE (MTHLY: EXTR MAX TEMP)")
  expect_equal(attributes$"unit of measurement", "DEGREE_CELSIUS")

  attributes <- attributes(observationData[[4]])
  expect_equal(attributes$name, "secondPhenomenon")
  expect_equal(attributes$definition, "second-phenomenon for testing only")
  expect_equal(attributes$"unit of measurement", "secondUom")
  #
  # test data
  #
  expect_length(observationData[[1]], 2)

  expect_equal(as.character(observationData[[ 1,1]]), "1056")
  expect_equal(as.character(observationData[[ 2,1]]), "1056")

  expect_equal(observationData[[ 1,2]], parsedate::parse_iso_8601("1999-01"))
  expect_equal(observationData[[ 2,2]], parsedate::parse_iso_8601("1999-02"))

  expect_equal(observationData[[1,3]], 29)
  expect_equal(observationData[[2,3]], 26)

  expect_equal(observationData[[1,4]], 26.5)
  expect_equal(observationData[[2,4]], 29)
})
#
# KVP::getData(sos) parses WML 2.0 encoded observations ----
#
test_that("WaterML 2.0 Observations are decoded", {
  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://example.com/sos-get-data?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
    webmockr::wi_th(
      headers = list("Accept" = "application/xml")
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/hydro-sos.niwa.co.nz.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  webmockr::stub_request('get', uri = 'http://example.com/sos-get-data?service=SOS&version=2.0.0&request=GetObservation&observedProperty=MTHLY_EXTR_MAX_TEMP&featureOfInterest=1056') %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetObservationResponse_200_WML_200.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  webmockr::stub_request('get', uri = 'http://example.com/sos-get-data?service=SOS&version=2.0.0&request=GetFeatureOfInterest&featureOfInterest=1056') %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetFeatureOfInterestResponse_200_niwa_1056.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-get-data", binding = "KVP", useDCPs = FALSE)
  observationData <- getData(sos = sos, phenomena = c("MTHLY_EXTR_MAX_TEMP"), sites = c("1056"), verbose = FALSE)
  expect_true(is(observationData, "data.frame"))
  colnames <- colnames(observationData)
  #
  # test meta data
  #
  expect_length(colnames, 3)
  expect_equal(colnames[1], "siteID")
  expect_equal(colnames[2], "timestamp")
  expect_equal(colnames[3], "MTHLY_EXTR_MAX_TEMP")
  #
  # test the data
  #
  expect_length(observationData[,1], 6)
  expect_true(all(observationData[,1] == "1056"))
  expect_true(observationData[1,2] == parsedate::parse_iso_8601("2019-01"))
  expect_true(observationData[2,2] == parsedate::parse_iso_8601("2019-02"))
  expect_true(observationData[3,2] == parsedate::parse_iso_8601("2019-03"))
  expect_true(observationData[4,2] == parsedate::parse_iso_8601("2019-04"))
  expect_true(observationData[5,2] == parsedate::parse_iso_8601("2019-05"))
  expect_true(observationData[6,2] == parsedate::parse_iso_8601("2019-06"))
  expect_equal(observationData[1,3], 31.2)
  expect_equal(observationData[2,3], 29.4)
  expect_equal(observationData[3,3], 27.0)
  expect_equal(observationData[4,3], 23.5)
  expect_equal(observationData[5,3], 22.1)
  expect_equal(observationData[6,3], 20.2)
})

webmockr::disable("httr")

context("getData: integration tests")
#
# KVP::can retrieve data from an online SOS 2.0.0 ----
#
test_that("KVP::can retrieve data from an online SOS 2.0.0", {
  skip_on_cran()

  sos <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/kvp",
             version = sos200_version, binding = "KVP", useDCPs = FALSE)
  obs <- getData(sos = sos, phenomena = c("AirTemperature"),
                 sites = c("Vaisala-WXT520", "wwu-ws-kli-hsb"),
                 begin = parsedate::parse_iso_8601("2015-03-29 22:00:00"), end = parsedate::parse_iso_8601("2015-03-30 23:00:00"))

  expect_s3_class(obs, "data.frame")
  expect_named(obs, c("siteID", "timestamp", "degC"))
  expect_length(obs$degC, 150)
})
