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
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)                          #
# Created: 2019-03-28                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
context("GetObservationById: encoding")

test_that("POX encoding works (1.0.0)", {
  testsos <- SOS_Test(name = "getobsbyidpox")
  getobs <- SosGetObservationById(service = "ser",
                                  version = "ver",
                                  observationId = "http://obs/1",
                                  responseFormat = "fmt"
  )
  request <- encodeRequestXML(obj = getobs, sos = testsos)
  expect_match(toString(request), 'service="ser"')
  expect_match(toString(request), ' version="ver"')
  expect_match(toString(request), "<sos:ObservationId>http://obs/1</sos:ObservationId>")
  expect_match(toString(request), "<sos:responseFormat>fmt</sos:responseFormat>")
})

test_that("POX encoding works (2.0.0)", {
  testsos <- SOS_Test(name = "getobsbyidpox", version = sos200_version, binding = "POX")
  getobs <- SosGetObservationById(service = "ser",
                                  version = "1.2.3",
                                  observationId = "http://obs/1")
  request <- encodeRequestXML(obj = getobs, sos = testsos)
  expect_match(toString(request), 'service="ser"')
  expect_match(toString(request), ' version="1.2.3"')
  expect_match(toString(request), "<sos20:observation>http://obs/1</sos20:observation>")
})

test_that("POX encoding works for multiple IDs (2.0.0)", {
  testsos <- SOS_Test(name = "getobsbyidpox", version = sos200_version, binding = "POX")
  getobs <- SosGetObservationById(service = "ser",
                                  version = "ver",
                                  observationId = c("http://obs/1", "http://obs/2"))
  request <- encodeRequestXML(obj = getobs, sos = testsos)
  expect_match(toString(request), 'service="ser"')
  expect_match(toString(request), ' version="ver"')
  expect_match(toString(request), "<sos20:observation>http://obs/1</sos20:observation>")
  expect_match(toString(request), "<sos20:observation>http://obs/2</sos20:observation>")
})

test_that("KVP encoding works (2.0.0)", {
  testsos <- SOS_Test(name = "getobsbyidpox", version = sos200_version, binding = "KVP")
  getobs <- SosGetObservationById(service = "ser",
                                  version = "ver",
                                  observationId = "http://observation/100")
  request <- encodeRequestKVP(obj = getobs, sos = testsos)
  expect_match(request, 'service=ser')
  expect_match(request, 'version=ver')
  expect_match(request, "request=GetObservationById")
  expect_match(request, "observation=http%3A%2F%2Fobservation%2F100")
})

test_that("KVP encoding works (2.0.0)", {
  testsos <- SOS_Test(name = "getobsbyidpox", version = sos200_version, binding = "KVP")
  getobs <- SosGetObservationById(service = "ser",
                                  version = "ver",
                                  observationId = c("http://observation/100", "http://observation/200"))
  request <- encodeRequestKVP(obj = getobs, sos = testsos)
  expect_match(request, 'service=ser')
  expect_match(request, 'version=ver')
  expect_match(request, "request=GetObservationById")
  expect_match(request, "observation=http%3A%2F%2Fobservation%2F100,http%3A%2F%2Fobservation%2F200")
})

context("GetObservationById: integration tests\n")

test_that("KVP (2.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/kvp",
               binding = "KVP",
               useDCPs = FALSE,
               version = sos200_version)

  obs <- getObservationById(sos = mySOS, observationId = "http://www.52north.org/test/observation/2")

  expect_s4_class(obs, "OmOM_Observation")
  expect_length(sosFeatureIds(obs), 1)
  foi <- sosFeaturesOfInterest(obs)@feature
  expect_equal(sosName(foi), "con terra")
  expect_equal(foi@sampledFeature, "http://www.52north.org/test/featureOfInterest/world")

  result <- sosResult(obs)
  expect_named(result, c("test_unit_1"))
  expect_equal(result[1,1], 1.1)
  coords <- sosCoordinates(obs)
  expect_named(coords, c("lon", "lat", "SRS"))
  expect_true((coords$lat - 51.88391) < 0.00000000001)
})

test_that("KVP (SOS 1.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/kvp", binding = "KVP", version = sos100_version)
  expect_error(getObservationById(sos = mySOS, observationId = "http://www.52north.org/test/observation/1"), "not supported")
})

test_that("POX (SOS 1.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/pox",
               binding = "POX", useDCPs = FALSE)
  obs <- getObservationById(sos = mySOS, observationId = "http://www.52north.org/test/observation/2")

  expect_s4_class(obs, "OmObservationCollection")
  expect_length(obs, 1)
  expect_s4_class(obs[[1]], "OmMeasurement")
  expect_s3_class(sosResult(obs), "data.frame")
  expect_equal(sosResult(obs)[1,1], 1.1)
})

test_that("POX (2.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/pox",
               binding = "POX",
               useDCPs = FALSE,
               version = sos200_version)

  obs <- getObservationById(sos = mySOS, observationId = "http://www.52north.org/test/observation/2")
  expect_s4_class(obs, "OmOM_Observation")
  expect_length(obs, 1)
  expect_s3_class(sosResult(obs), "data.frame")
  expect_equal(sosResult(obs)[1,1], 1.1)
})

test_that("file saving works (2.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/pox",
               binding = "POX",
               useDCPs = FALSE,
               version = sos200_version)

  tempfile <- tempfile(fileext = ".xml")
  expect_output(
    obs <- getObservationById(sos = mySOS,
                              observationId = "http://www.52north.org/test/observation/2",
                              saveOriginal = tempfile),
    paste0("document saved:(.*)", tempfile))
  expect_s4_class(obs, "OmOM_Observation")

  expect_true(file.exists(tempfile))
  observationFromFile <- xml2::read_xml(tempfile)
  expect_equal(xml2::xml_name(observationFromFile), "GetObservationByIdResponse")
  file.remove(tempfile)
  expect_false(file.exists(tempfile))
})
