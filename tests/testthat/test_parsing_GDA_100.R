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
# Created: 2019-03-28                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################
library(webmockr)
library(httr)
library(stringr)

# todo move to common test function file
parseXmlSnippet <- function(obj) {
  .doc <- xmlParseDoc(obj, asText = TRUE, options = NOERROR)
  .docRoot <- xmlRoot(.doc)
  return(.docRoot)
}

webmockr::enable("httr")
webmockr::httr_mock()
context("Parsing -> GetDataAvailability -> v1.0.0")

test_that("parseGetDataAvailabilityResponse() returns an empty list if an empty response is received", {
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
  .doc <- parseXmlSnippet(readr::read_file("../responses/GetDataAvailability_100_Example.com_empty.xml"))
  .dams <- parseGetDataAvailabilityResponse(obj = .doc, sos)

  expect_false(is.null(.dams))
  expect_true(is.list(.dams))
  expect_true(length(.dams) == 0)
})
