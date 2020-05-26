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
# Created: 2019-01-07                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
context("GetObservation")
#
# creation of request fails if eventTime contains objects of wrong class ----
#
test_that("creation of request fails if eventTime contains objects of wrong class", {
  testsos <- SOS_Test(name = "testcaps")
  axiomCaps <- parseSosCapabilities(xml2::read_xml(x = "../responses/Capabilities_52N-SOS_Axiom.xml"), testsos)
  testsos@capabilities <- axiomCaps

  eventTimeList <- sosCreateEventTimeList(sosCreateTimePeriod(sos = testsos,
                                                              begin = parsedate::parse_iso_8601(Sys.time() - 3600 * 24),
                                                              end = parsedate::parse_iso_8601(Sys.time())))
  eventTimeList[[2]] <- eventTimeList[[1]]
  eventTimeList[[3]] <- eventTimeList[[1]]
  getobs <- SosGetObservation(service = "test", version = "test", offering = "test", observedProperty = list("test"),
                              responseFormat = "test",
                              eventTime = eventTimeList)
  expect_equal(getobs@eventTime, eventTimeList)
  eventTimeList[[2]] <- list("not right")
  expect_error(SosGetObservation(service = "test", version = "test", offering = "test", observedProperty = list("test"),
                                 responseFormat = "test",
                                 eventTime = eventTimeList), "must extend SosEventTime")
})

context("GetObservation: KVP encoding")
#
# creation temporal filter has a valueReference in SOS 2.0, but not for SOS 1.0.0 ----
#
test_that("creation temporal filter has a valueReference in SOS 2.0, but not for SOS 1.0.0", {
  testsos100 <- SOS_Test(name = "onezerozero", version = sos100_version)
  testsos200 <- SOS_Test(name = "twozero", version = sos200_version)
  period <- sosCreateTimePeriod(sos = testsos100,
                                begin = parsedate::parse_iso_8601("2019-01-01"),
                                end = parsedate::parse_iso_8601("2019-01-02"))
  eventTimeList <- sosCreateEventTimeList(period)

  request100 <- SosGetObservation(service = sosService, version = testsos100@version,
                                  offering = "off", observedProperty = list("prop"), responseFormat = "fmt",
                                  eventTime = eventTimeList)
  requestString100 <- encodeRequestKVP(request100, testsos100)

  request200 <- SosGetObservation_2.0.0(service = sosService, version = testsos200@version,
                                        temporalFilter = list(period), valueReferenceTemporalFilter = "om:time")
  requestString200 <- encodeRequestKVP(request200, testsos200)

  expect_match(requestString100, "&eventTime=2019-01-01")
  expect_match(requestString200, "&temporalFilter=om%3Atime%2C2019-01-01")
})
#
# creation temporal filter includes namespaces in KVP (2.0.0) ----
#
test_that("creation temporal filter includes namespaces in KVP (2.0.0)", {
  testsos20 <- SOS_Test(name = "twozero", version = sos200_version)
  period <- sosCreateTimePeriod(sos = testsos20,
                                begin = parsedate::parse_iso_8601("2019-01-01"),
                                end = parsedate::parse_iso_8601("2019-01-02"))
  getobs <- SosGetObservation_2.0.0(service = sosService, version = testsos20@version,
                                    temporalFilter =  list(period), valueReferenceTemporalFilter = "om:time")
  request <- encodeRequestKVP(getobs, testsos20)

  expect_match(request, "&temporalFilter=om%3Atime%2C2019-01-01")
  expect_match(request, "&namespaces=xmlns%28om%2Chttp%3A%2F%2Fwww.opengis.net%2Fom%2F2.0%29")
})
#
# multiple temporal filters give warning in KVP (2.0.0) ----
#
test_that("multiple temporal filters give warning in KVP (2.0.0)", {
  testsos20 <- SOS_Test(name = "twozero", version = sos200_version)
  period <- sosCreateTimePeriod(sos = testsos20,
                                begin = parsedate::parse_iso_8601("2019-01-01"),
                                end = parsedate::parse_iso_8601("2019-01-02"))
  getobs <- SosGetObservation_2.0.0(service = sosService, version = testsos20@version,
                                    temporalFilter =  list(period, period), valueReferenceTemporalFilter = "om:time")
  expect_warning(request <- encodeRequestKVP(getobs, testsos20),
                 "Discarding(.*)KVP")
})
#
# feature of interest in KVP (2.0.0) ----
#
test_that("feature of interest in KVP (2.0.0)", {
  testsos <- SOS_Test(name = "getobspox", version = sos200_version)
  getobs <- SosGetObservation_2.0.0(service = "ser",
                                    version = "2.0.0",
                                    featureOfInterest = list("http://wfs.example.org?request=getFeature&amp;featureid=b
uilding1"))
  request <- encodeRequestKVP(obj = getobs, sos = testsos)
  expect_match(request, "featureOfInterest=http%3A%2F%2Fwfs.example.org%3Frequest%3DgetFeature%26amp%3Bfeatureid%3Db%0Auilding1")
})

feat1 <- GmlFeatureProperty(feature = SamsSamplingFeature(id = "id",
                                                          identifier = "http://feature/1",
                                                          name = "feat1",
                                                          type = "typeF",
                                                          sampledFeature = "sample",
                                                          shape = SamsShape(GmlPoint(GmlDirectPositionLatLon(1, 2)))))
#
# multiple feature of interest with mixed characters and objects in KVP (2.0.0) ----
#
test_that("multiple feature of interest with mixed characters and objects in KVP (2.0.0)", {
  testsos <- SOS_Test(name = "getobspox", version = sos200_version)
  getobs <- SosGetObservation_2.0.0(service = "ser",
                                    version = "2.0.0",
                                    featureOfInterest = list("http://feature/0",
                                                             feat1))
  request <- encodeRequestKVP(obj = getobs, sos = testsos)
  expect_match(request, "featureOfInterest=http%3A%2F%2Ffeature%2F0,http%3A%2F%2Ffeature%2F1")
})
#
# observed property/ies in KVP (2.0.0) ----
#
test_that("observed property/ies in KVP (2.0.0)", {
  testsos <- SOS_Test(name = "getobspox", version = sos200_version)
  getobs <- SosGetObservation_2.0.0(service = "ser",
                                    version = "2.0.0",
                                    observedProperty = list("http://sweet.jpl.nasa.gov/2.0/atmoThermo.owl#EffectiveTemp
erature"))
  request <- encodeRequestKVP(obj = getobs, sos = testsos)
  expect_match(request, "observedProperty=http%3A%2F%2Fsweet.jpl.nasa.gov%2F2.0%2FatmoThermo.owl%23EffectiveTemp%0Aerature")

  getobs <- SosGetObservation_2.0.0(service = "ser",
                                    version = "2.0.0",
                                    observedProperty = list("http://sweet.jpl.nasa.gov/2.0/atmoThermo.owl#EffectiveTemp
erature", "prop2", "prop3"))
  request <- encodeRequestKVP(obj = getobs, sos = testsos)
  expect_match(request, "observedProperty=http%3A%2F%2Fsweet.jpl.nasa.gov%2F2.0%2FatmoThermo.owl%23EffectiveTemp%0Aerature,prop2,prop3")
})
#
# spatial filter in KVP (2.0.0) ----
#
test_that("spatial filter in KVP (2.0.0)", {
  testsos <- SOS_Test(name = "getobspox", version = sos200_version)
  getobs <- SosGetObservation_2.0.0(service = "ser",
                                    version = "2.0.0",
                                    spatialFilter = SamsShape(point = GmlPoint(pos = GmlDirectPosition(pos = "1 2"))))
  request <- encodeRequestKVP(obj = getobs, sos = testsos)
  # xmlns(sams,http://www.opengis.net/samplingSpatial/2.0)
  expect_match(request, "namespaces=xmlns%28sams%2Chttp%3A%2F%2Fwww.opengis.net%2FsamplingSpatial%2F2.0%29")

  skip("SpatialFilter for KVP 2.0.0 not implemented yet")
  # om:featureOfInterest/*/sams:shape,22.32,11.2,32.32,22.2,urn:ogc:def:crs:EPSG::4326
  expect_match(request, "spatialFilter=om%3AfeatureOfInterest%2F*%2Fsams%3Ashape%2C22.32%2C11.2%2C32.32%2C22.2%2Curn%3Aogc%3Adef%3Acrs%3AEPSG%3A%3A4326")
})
#
# multiple offering IDs with KVP (2.0.0) ----
#
test_that("multiple offering IDs with KVP (2.0.0)", {
  testsos <- SOS_Test(name = "getobspox", version = sos200_version)
  getobs <- SosGetObservation_2.0.0(service = "ser",
                                    version = "2.0.0",
                                    offering = list("http://off1a", "off2:b", "off3c"),
                                    observedProperty = list("p1", "p2"))
  request <- encodeRequestKVP(obj = getobs, sos = testsos)
  expect_match(request, "offering=http%3A%2F%2Foff1a,off2%3Ab,off3c")
})

createsos <- SOS_Test(name = "getobspox", version = sos200_version)
off2a <- SosObservationOffering_2.0.0(id = "off2a",
                                      name = "testoff",
                                      resultTime = sosCreateTimePeriod(sos = createsos, begin = parsedate::parse_iso_8601("2019-01-01"), end = parsedate::parse_iso_8601("2019-01-02")),
                                      phenomenonTime = sosCreateTimePeriod(sos = createsos, begin = parsedate::parse_iso_8601("2019-01-01"), end = parsedate::parse_iso_8601("2019-01-02")),
                                      procedure = "procA",
                                      observableProperty = list("obsA"),
                                      observationType = list("measurement"),
                                      procedureDescriptionFormat = list("pdf"),
                                      featureOfInterestType = list("foiA"),
                                      responseFormat = list("rf"))
off2b <- SosObservationOffering_2.0.0(id = "off2b",
                                      name = "testoff",
                                      resultTime = sosCreateTimePeriod(sos = createsos, begin = parsedate::parse_iso_8601("2019-01-01"), end = parsedate::parse_iso_8601("2019-01-02")),
                                      phenomenonTime = sosCreateTimePeriod(sos = createsos, begin = parsedate::parse_iso_8601("2019-01-01"), end = parsedate::parse_iso_8601("2019-01-02")),
                                      procedure = "procB",
                                      observableProperty = list("obsB"),
                                      observationType = list("measurement"),
                                      procedureDescriptionFormat = list("pdf"),
                                      featureOfInterestType = list("foiB"),
                                      responseFormat = list("rf"))
#
# multiple offerings (mixed IDs and objects) with KVP (2.0.0) ----
#
test_that("multiple offerings (mixed IDs and objects) with KVP (2.0.0)", {
  testsos <- SOS_Test(name = "getobspox", version = sos200_version)
  getobs <- SosGetObservation_2.0.0(service = "ser",
                                    version = "2.0.0",
                                    offering = list("http://off1a",
                                                    off2a,
                                                    off2b,
                                                    "off3"),
                                    observedProperty = list("p1", "p2"),
                                    responseFormat = "fmt")
  request <- encodeRequestKVP(obj = getobs, sos = testsos)
  expect_match(request, "offering=http%3A%2F%2Foff1a,off2a,off2b,off3")
})
#
# responseFormat with KVP (2.0.0) ----
#
test_that("responseFormat with KVP (2.0.0)", {
  testsos <- SOS_Test(name = "getobspox", version = sos200_version)
  getobs <- SosGetObservation_2.0.0(service = "ser",
                                    version = "2.0.0",
                                    offering = list("http://off1a"),
                                    responseFormat = "http://www.opengis.net/om/2.0")
  request <- encodeRequestKVP(obj = getobs, sos = testsos)
  expect_match(request, "responseFormat=http%3A%2F%2Fwww.opengis.net%2Fom%2F2.0")
})

context("GetObservation: POX encoding")
#
# minimal ----
#
test_that("minimal", {
  testsos <- SOS_Test(name = "getobspox")
  getobs <- SosGetObservation(service = "ser",
                              version = "ver",
                              offering = "off",
                              observedProperty = list("p1", "p2"),
                              responseFormat = "fmt")
  request <- encodeRequestXML(obj = getobs, sos = testsos)
  expect_match(toString(request), 'service="ser"')
  expect_match(toString(request), ' version="1.0.0"')
  expect_match(toString(request), "<sos:offering>off</sos:offering>")
  expect_match(toString(request), "<sos:observedProperty>p1</sos:observedProperty>")
  expect_match(toString(request), "<sos:observedProperty>p2</sos:observedProperty>")
  expect_match(toString(request), "<sos:responseFormat>fmt</sos:responseFormat>")
  expect_equal(stringr::str_count(toString(request), "time"), 0)
})
#
# with event time (SOS 1.0.0) ----
#
test_that("with event time (SOS 1.0.0)", {
  testsos <- SOS_Test(name = "getobspox")
  getobs <- SosGetObservation(service = "ser",
                              version = "ver",
                              offering = "off",
                              observedProperty = list("p1", "p2"),
                              responseFormat = "fmt",
                              eventTime = sosCreateTime(sos = testsos,
                                                        time = "2017-12-19::2017-12-20"))
  request <- encodeRequestXML(obj = getobs, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(request), ">\\s*<", "><")
  expect_match(encodedString, 'service="ser"')
  expect_match(encodedString, ' version="1.0.0"')
  expect_match(encodedString, "<sos:offering>off</sos:offering>")
  expect_match(encodedString, "<sos:observedProperty>p1</sos:observedProperty>")
  expect_match(encodedString, "<sos:observedProperty>p2</sos:observedProperty>")
  expect_match(encodedString, "<sos:responseFormat>fmt</sos:responseFormat>")
  expect_match(encodedString, "<ogc:TM_During")
  expect_match(encodedString, "<ogc:PropertyName>om:samplingTime</ogc:PropertyName>")
  expect_match(encodedString, "2017-12-20T00:00:00\\+00:00</gml:endPosition>")
  expect_match(encodedString, "</ogc:TM_During></sos:eventTime>")
})
#
# error on multiple offerings for SOS 1.0.0 ----
#
test_that("error on multiple offerings for SOS 1.0.0", {
  testsos <- SOS_Test(name = "getobspox", version = sos100_version)
  expect_error(getObservation(sos = testsos,
                              offering = list("off1", "off2")),
               "unable to find an inherited method")
})
#
# response format with POX (SOS 2.0.0) ----
#
test_that("response format with POX (SOS 2.0.0)", {
  testsos <- SOS_Test(name = "getobspox", version = sos200_version)
  getobs <- SosGetObservation_2.0.0(service = "ser",
                                    version = "2.0.0",
                                    responseFormat = "http://www.opengis.net/om/2.0")
  request <- encodeRequestXML(obj = getobs, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(request), ">\\s*<", "><")
  expect_match(encodedString, "<sos:responseFormat>http://www.opengis.net/om/2.0</sos:responseFormat>")
})
#
# procedure(s) with POX (SOS 2.0.0) ----
#
test_that("procedure(s) with POX (SOS 2.0.0)", {
  testsos <- SOS_Test(name = "getobspox", version = sos200_version)
  getobs <- SosGetObservation_2.0.0(service = "ser",
                                    version = "2.0.0",
                                    procedure = list("http://www.52north.org/test/procedure/1"))
  request <- encodeRequestXML(obj = getobs, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(request), ">\\s*<", "><")
  expect_match(encodedString, "<sos:procedure>http://www.52north.org/test/procedure/1</sos:procedure></GetObservation>")

  getobs <- SosGetObservation_2.0.0(service = "ser",
                                    version = "2.0.0",
                                    procedure = list("procedure/1",
                                                     "procedure/2"))
  request <- encodeRequestXML(obj = getobs, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(request), ">\\s*<", "><")
  expect_match(encodedString, "<sos:procedure>procedure/1</sos:procedure><sos:procedure>procedure/2</sos:procedure>")
})
#
# feature(s) of interest with POX (SOS 2.0.0) ----
#
test_that("feature(s) of interest with POX (SOS 2.0.0)", {
  testsos <- SOS_Test(name = "getobspox", version = sos200_version)
  getobs <- SosGetObservation_2.0.0(service = "ser",
                                    version = "2.0.0",
                                    featureOfInterest = list("http://www.52north.org/test/featureOfInterest/1"))
  request <- encodeRequestXML(obj = getobs, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(request), ">\\s*<", "><")
  expect_match(encodedString, "<sos:featureOfInterest>http://www.52north.org/test/featureOfInterest/1</sos:featureOfInterest></GetObservation>")

  getobs <- SosGetObservation_2.0.0(service = "ser",
                                    version = "2.0.0",
                                    featureOfInterest = list("feature/1",
                                                             "feature/2"))
  request <- encodeRequestXML(obj = getobs, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(request), ">\\s*<", "><")
  expect_match(encodedString, "<sos:featureOfInterest>feature/1</sos:featureOfInterest><sos:featureOfInterest>feature/2</sos:featureOfInterest>")
})
#
# feature(s) of interest with mixed features (string and object) for POX (SOS 2.0.0) ----
#
test_that("feature(s) of interest with mixed features (string and object) for POX (SOS 2.0.0)", {
  testsos <- SOS_Test(name = "getobspox", version = sos200_version)
  getobs <- SosGetObservation_2.0.0(service = "ser",
                                    version = "2.0.0",
                                    featureOfInterest = list("feature/0",
                                                             feat1,
                                                             feat1))
  request <- encodeRequestXML(obj = getobs, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(request), ">\\s*<", "><")
  expect_match(encodedString, "<sos:featureOfInterest>feature/0</sos:featureOfInterest><sos:featureOfInterest>http://feature/1</sos:featureOfInterest><sos:featureOfInterest>http://feature/1</sos:featureOfInterest>")
})
#
# temporal filter with POX (SOS 2.0.0) ----
#
test_that("temporal filter with POX (SOS 2.0.0)", {
  testsos <- SOS_Test(name = "getobspox", version = sos200_version)
  getobs <- SosGetObservation_2.0.0(service = "ser",
                                    version = "2.0.0",
                                    temporalFilter = list(sosCreateTimePeriod(sos = testsos,
                                                                              begin = parsedate::parse_iso_8601("2019-02-02"),
                                                                              end = parsedate::parse_iso_8601("2019-03-03"))),
                                    valueReferenceTemporalFilter = "phenomenonTime")
  request <- encodeRequestXML(obj = getobs, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(request), ">\\s*<", "><")
  expect_match(encodedString, "sos20:temporalFilter><fes:During><fes:ValueReference>phenomenonTime</fes:ValueReference>")
  expect_match(encodedString, "2019-02-02T00:00:00\\+00:00</gml:beginPosition><gml:endPosition>2019-03-03T00:00:00\\+00:00</gml:endPosition></gml:TimePeriod>")
})
#
# multiple offering IDs with POX (SOS 2.0.0) ----
#
test_that("multiple offering IDs with POX (SOS 2.0.0)", {
  testsos <- SOS_Test(name = "getobspox", version = sos200_version)
  getobs <- SosGetObservation_2.0.0(service = "ser",
                                    version = "2.0.0",
                                    offering = list("off1", "off2", "off3"),
                                    observedProperty = list("p1", "p2"),
                                    responseFormat = "fmt")
  request <- encodeRequestXML(obj = getobs, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(request), ">\\s*<", "><")
  expect_match(encodedString, "<sos:offering>off1</sos:offering>")
  expect_match(encodedString, "<sos:offering>off2</sos:offering>")
  expect_match(encodedString, "<sos:offering>off3</sos:offering>")
})
#
# multiple offerings with POX (SOS 2.0.0) ----
#
test_that("multiple offerings with POX (SOS 2.0.0)", {
  testsos <- SOS_Test(name = "getobspox", version = sos200_version)
  getobs <- SosGetObservation_2.0.0(service = "ser",
                                    version = "2.0.0",
                                    offering = list(off2a, off2b),
                                    observedProperty = list("p1", "p2"),
                                    responseFormat = "fmt")
  request <- encodeRequestXML(obj = getobs, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(request), ">\\s*<", "><")
  expect_match(encodedString, "<sos:offering>off2a</sos:offering>")
  expect_match(encodedString, "<sos:offering>off2b</sos:offering>")
})
#
# multiple offerings (mixed IDs and ojbects) with POX (SOS 2.0.0) ----
#
test_that("multiple offerings (mixed IDs and ojbects) with POX (SOS 2.0.0)", {
  testsos <- SOS_Test(name = "getobspox", version = sos200_version)
  getobs <- SosGetObservation_2.0.0(service = "ser",
                                    version = "2.0.0",
                                    offering = list("off1",
                                                    off2a,
                                                    off2b,
                                                    "off3"),
                                    observedProperty = list("p1", "p2"),
                                    responseFormat = "fmt")
  request <- encodeRequestXML(obj = getobs, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(request), ">\\s*<", "><")
  expect_match(encodedString, "<sos:offering>off1</sos:offering>")
  expect_match(encodedString, "<sos:offering>off2a</sos:offering>")
  expect_match(encodedString, "<sos:offering>off2b</sos:offering>")
  expect_match(encodedString, "<sos:offering>off3</sos:offering>")
})

context("GetObservation: integration tests\n")
#
# KVP (1.0.0) ----
#
test_that("KVP (1.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service", binding = "KVP")
  off.1 <- sosOfferings(mySOS)[[1]]
  obs.1 <- getObservation(sos = mySOS,
                          offering = off.1,
                          procedure = sosProcedures(off.1)[[1]],
                          observedProperty = sosObservedProperties(off.1)[1],
                          eventTime = sosCreateTime(sos = mySOS,
                                                    time = "2017-12-19::2017-12-20"))
  expect_s4_class(obs.1, "OmObservationCollection")
  expect_length(obs.1, 1)
  expect_s4_class(obs.1[[1]], "OmObservation")
  expect_equal(dim(sosResult(obs.1)), c(72, 2))
})
#
# POX (1.0.0) ----
#
test_that("POX (1.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/pox",
               binding = "POX",
               useDCPs = FALSE)
  off.1 <- sosOfferings(mySOS)[[1]]
  obs.1 <- getObservation(sos = mySOS,
                          offering = off.1,
                          procedure = sosProcedures(off.1)[[1]],
                          observedProperty = sosObservedProperties(off.1)[1],
                          eventTime = sosCreateTime(sos = mySOS,
                                                    time = "2017-12-19::2017-12-20"))

  expect_s4_class(obs.1, "OmObservationCollection")
  expect_length(obs.1, 1)
  expect_s4_class(obs.1[[1]], "OmObservation")
  result <- sosResult(obs.1)
  expect_equal(dim(result), c(71, 2))
  expect_named(result, c("phenomenonTime", "AirTemperature"))
})
#
# POX with spatial filter not matching anything (1.0.0) ----
#
test_that("POX with spatial filter not matching anything (1.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/pox",
               binding = "POX",
               useDCPs = FALSE)
  off.1 <- sosOfferings(mySOS)[[1]]
  request.bbox <- sosCreateBBOX(lowLat = 5.0, lowLon = 1.0,
                                uppLat = 10.0, uppLon = 3.0,
                                srsName = "urn:ogc:def:crs:EPSG::4326")
  request.bbox.foi <- sosCreateFeatureOfInterest(spatialOps = request.bbox)

  expect_warning(obs.sept15.bbox <- getObservation(sos = mySOS,
                                    offering = off.1,
                                    featureOfInterest = request.bbox.foi),
                 "urn:ogc:def:nil:OGC:inapplicable")

  expect_s4_class(obs.sept15.bbox, "OmObservationCollection")
  expect_length(obs.sept15.bbox, 1)
  expect_s4_class(obs.sept15.bbox[[1]], "OmObservationProperty")
  expect_equal(obs.sept15.bbox[[1]]@href, "urn:ogc:def:nil:OGC:inapplicable")
})
#
# CSV parsing works (1.0.0) ----
#
test_that("CSV parsing works (1.0.0)", {
  skip_on_cran()

  ioos <- SOS(url = "https://sdf.ndbc.noaa.gov/sos/server.php",
              useDCPs = FALSE,
              timeFormat = "%Y-%m-%dT%H:%M:%SZ")
  ioos.off <- sosOfferings(ioos)
  obs <- getObservation(sos = ioos,
                        offering = sosName(ioos.off[[100]]),
                        responseFormat = "text/csv",
                        observedProperty = sosObservedProperties(ioos.off[[100]])[1])
  expect_s3_class(obs, "data.frame")
  expect_equal(dim(obs), c(1, 7))
})
#
# KVP with offering object (2.0.0) ----
#
test_that("KVP with offering object (2.0.0)", {
  skip_on_cran()

  sos <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service", version = sos200_version, binding = "KVP")
  obs <- getObservation(sos = sos,
                        offering = sosOfferings(sos)[["wxt520"]],
                        observedProperty = list("AirTemperature"),
                        responseFormat = "http://www.opengis.net/om/2.0",
                        eventTime = sosCreateTime(sos = sos,
                                                  time =  "2018-04-22T17:45:15+02:00/2018-05-29T17:45:15+02:00"),
                        )

  expect_length(obs, 3)
  expect_equal(sapply(obs, class), rep("OmOM_Observation", 3))
  result <- sosResult(obs)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("°C"))
  expect_equal(dim(result), c(3,1))
  expect_equal(sum(result$`°C`), 242.019)

  expect_equal(sosUOM(result), c("°C" = "°C"))
})
#
# POX without offering (2.0.0) ----
#
test_that("POX without offering (2.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service",
               version = sos200_version,
               binding = "POX",
               useDCPs = FALSE)
  period <- sosCreateTimePeriod(sos = mySOS,
                                begin = parsedate::parse_iso_8601("2017-12-19 00:00:00"),
                                end = parsedate::parse_iso_8601("2017-12-19 01:00:00"))
  obs.1 <- getObservation(sos = mySOS,
                          #inspect = TRUE,
                          eventTime = list(period))

  expect_true(class(obs.1) != "OwsExceptionReport")
  expect_length(obs.1, 76)
  for (obs in obs.1) {
    expect_s4_class(obs, "OmOM_Observation")
  }

  result <- sosResult(obs.1[[1]])
  expect_equal(dim(result), c(1, 1))
  expect_named(result, c("degC"))

  allObservedProperties <- unique(sapply(lapply(obs.1, sosResult), names))
  expect_true(all(c("deg", "degC", "mm") %in% allObservedProperties))
})
#
# POX with observed property (2.0.0) ----
#
test_that("POX with observed property (2.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service",
               version = sos200_version,
               binding = "POX",
               useDCPs = FALSE)
  period <- sosCreateTimePeriod(sos = mySOS,
                                begin = parsedate::parse_iso_8601("2017-12-19 12:00:00"),
                                end = parsedate::parse_iso_8601("2017-12-19 13:00:00"))
  wxt520 <- sosOfferings(mySOS)[["wxt520"]]
  obs.1 <- getObservation(sos = mySOS,
                          eventTime = list(period),
                          observedProperty = list("Humidity")
                          #,inspect=TRUE
                          )

  expect_true(class(obs.1) != "OwsExceptionReport")
  expect_length(obs.1, 8)
  for (obs in obs.1) {
    expect_s4_class(obs, "OmOM_Observation")
  }
  result <- sosResult(obs.1)
  expect_equal(dim(result), c(8, 1))
  expect_named(result, c("%"))
})
#
# POX with offering object in list and not in list (2.0.0) ----
#
test_that("POX with offering object in list and not in list (2.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service",
               version = sos200_version,
               binding = "POX",
               useDCPs = FALSE)
  period <- sosCreateTimePeriod(sos = mySOS,
                                begin = parsedate::parse_iso_8601("2017-12-19 12:00:00"),
                                end = parsedate::parse_iso_8601("2017-12-19 12:12:00"))
  obs.1 <- getObservation(sos = mySOS,
                          eventTime = list(period),
                          offering = sosOfferings(mySOS)["wxt520"])

  expect_true(class(obs.1) != "OwsExceptionReport")
  expect_length(obs.1, 18)
  for (obs in obs.1) {
    expect_s4_class(obs, "OmOM_Observation")
  }

  obs.2 <- getObservation(sos = mySOS,
                          eventTime = list(period),
                          offering = sosOfferings(mySOS)[["wxt520"]])
  expect_length(obs.2, 18)

  expect_equal(obs.2[[1]]@result, obs.1[[1]]@result)
  expect_equal(obs.2[[2]]@result, obs.1[[2]]@result)
})
#
# FOI retrieval can be disabled (tested with POX, 2.0.0) ----
#
test_that("FOI retrieval can be disabled (tested with POX, 2.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service",
               version = sos200_version,
               binding = "POX",
               useDCPs = FALSE)
  period <- sosCreateTimePeriod(sos = mySOS,
                                begin = parsedate::parse_iso_8601("2017-12-20 09:00:00"),
                                end = parsedate::parse_iso_8601("2017-12-20 10:00:00"))

  withFoi <- getObservation(sos = mySOS,
                            eventTime = list(period),
                            retrieveFOI = TRUE)

  # with FOI, we have no hrefs <-- why?
  expect_equal(unique(sapply(sosFeaturesOfInterest(withFoi), function(x) {class(x)})), "GmlFeatureProperty")
  expect_equal(unique(sapply(sosFeaturesOfInterest(withFoi), function(x) {slot(x, "href")})), NA_character_)
  expect_equal(unique(sapply(sosFeaturesOfInterest(withFoi), function(x) {class(slot(x, "feature"))})), "SamsSamplingFeature")

  noFoi <- getObservation(sos = mySOS,
                          eventTime = list(period),
                          retrieveFOI = FALSE)

  # without FOI retrieval only hrefs
  expect_equal(unique(sapply(sosFeaturesOfInterest(noFoi), function(x) {class(x)})), "GmlFeatureProperty")
  expect_equal(unique(sapply(sosFeaturesOfInterest(noFoi), function(x) {slot(x, "href")})), c("Vaisala-WXT520", "http://52north.org/fac/internal/it-is/srv-01", "ELV-WS2500"))
  expect_equal(unique(sapply(sosFeaturesOfInterest(noFoi), function(x) {slot(x, "feature")})), list(NULL))
})
