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
# Author: Daniel Nüst (daniel.nuest@uni-muenster.de)                           #
# Created: 2019-05-16                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
################################################################################

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
webmockr::stub_request("get", uri = "http://example.com/sos-get-data?service=SOS&version=2.0.0&request=GetObservation&observedProperty=pH&featureOfInterest=29808&responseFormat=http%3A%2F%2Fwww.opengis.net%2Fom%2F2.0") %>%
  webmockr::wi_th(
    headers = list("Accept" = "application/xml")
  ) %>%
  webmockr::to_return(
    status = 200,
    body = emptyGetObservationResponse,
    headers = list("Content-Type" = "application/xml")
  )
webmockr::stub_request("get", uri = "http://example.com/sos-get-data?service=SOS&version=2.0.0&request=GetObservation&observedProperty=Discharge&featureOfInterest=29808&responseFormat=http%3A%2F%2Fwww.opengis.net%2Fom%2F2.0") %>%
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

test_that("KVP::getData(sos) returns data values in data.frame", {
  sos <- SOS(version = sos200_version, url = "http://example.com/sos-get-data", binding = "KVP", useDCPs = FALSE)

  observationData <- getData(sos, phenomena = c("Discharge"), sites = c("29808"), retrieveFOI = FALSE)

  expect_true(is.data.frame(observationData))
  expect_equal(dim(observationData), c(1,1))
  expect_named(observationData, c("m^3/s"))
})

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

test_that("KVP::getData(sos) returns empty data.frame if no data found", {
  sos <- SOS(version = sos200_version, url = "http://example.com/sos-get-data", binding = "KVP", useDCPs = FALSE)
  observationData <- getData(sos = sos, phenomena = c("pH"), sites = c("29808"))
  expect_true(is.data.frame(observationData))
  expect_equal(dim(observationData), c(0, 0))
})

test_that("KVP::getData(sos) returns empty data.frame if no data found (list input)", {
  sos <- SOS(version = sos200_version, url = "http://example.com/sos-get-data", binding = "KVP", useDCPs = FALSE)
  observationData <- getData(sos = sos, phenomena = list("pH"), sites = list("29808"))
  expect_true(is.data.frame(observationData))
  expect_equal(dim(observationData), c(0, 0))
})

test_that("KVP::getData(sos) passes verbose paramter on", {
  sos <- SOS(version = sos200_version, url = "http://example.com/sos-get-data", binding = "KVP", useDCPs = FALSE)
  output <- capture_output(getData(sos = sos, phenomena = c("pH"), sites = c("29808"), verbose = TRUE))
  expect_match(output, "Requesting list of offerings")
  expect_match(output, "Finished getObservation to http://example.com/sos-get-data")
})

webmockr::disable("httr")

context("getData: integration tests\n")

test_that("can retrieve data from an online SOS 2.0.0 (KVP)", {
  skip("Takes too long - debug!")
  skip_on_cran()

  sos <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/kvp",
             version = sos200_version, binding = "KVP", useDCPs = FALSE)
  obs <- getData(sos = sos, phenomena = c("AirTemperature", "AthmosphericPressure"),
                 sites = c("Vaisala-WXT520", "wwu-ws-kli-hsb"),
                 begin = as.POSIXct("2015-03-29 22:00:00"), end = as.POSIXct("2015-03-30 23:00:00"),
                 verbose = TRUE)

  expect_s3_class(obs, "data.frame")
  expect_named(obs, c("phenomenon"))
  expect_length(obs$phenomenon, 33)
})
