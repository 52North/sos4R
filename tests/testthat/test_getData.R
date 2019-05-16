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

# download mock responses
#niwaHydro <- SOS(url = "https://hydro-sos.niwa.co.nz/", binding = "KVP", useDCPs = FALSE, version = "2.0.0")
#niwaOffering <- sosOfferings(niwaHydro)[[3]]
#obs <- getObservation(sos = niwaHydro,
#               offering = niwaOffering,
#               observedProperty = list("Height of Gauge (River Stage)"),
#               eventTime = sosCreateTime(sos = niwaHydro, time = "2019-05-01 00:00:00::2019-05-01 01:00:00"),
#               saveOriginal = "tests/responses/getData_gauge_niwa.xml")

test_that("KVP::getData(sos) returns data values in data.frame", {
  skip("Not implemented!")

  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://example.com/sos-get-data?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
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
  expect_equal("WindSpeed",             dataFrameOfPhenomena[27, 1])
  expect_equal("WindSpeedMperSec",      dataFrameOfPhenomena[33, 1])
})

webmockr::disable("httr")

context("getData: integration tests\n")

test_that("can retrieve data from an online SOS 2.0.0 (KVP)", {
  skip("Not implemented!")

  skip_on_cran()

  sos <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/kvp",
             version = sos200_version, binding = "KVP", useDCPs = FALSE)
  phen <- phenomena(sos = sos)

  expect_s3_class(phen, "data.frame")
  expect_named(phen, c("phenomenon"))
  expect_length(phen$phenomenon, 33)
})
