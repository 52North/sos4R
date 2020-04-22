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
# Created: 2019-05-03                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
context("DescribeSensor: request encoding")

testsos <- SOS_Test(name = "testsensor", version = sos100_version)
axiomCaps <- parseSosCapabilities(xml2::read_xml(x = "../responses/Capabilities_52N-SOS_Axiom.xml"), testsos)
testsos@capabilities <- axiomCaps

test_that("output format for POX (1.0.0)", {
  describeSensorOp <- sosOperation(testsos, sosDescribeSensorName)
  of <- describeSensorOp@parameters[["outputFormat"]][[1]]
  ds <- SosDescribeSensor(service = testsos@name, version = testsos@version, procedure = NA_character_, outputFormat = of)
  expect_equal(ds@outputFormat, "text/xml; subtype=\"sensorML/1.0.1/profiles/ioos_sos/1.0\"")
  xml <- encodeRequestXML(obj = ds, sos = testsos)
  expect_match(toString(xml), "text/xml; subtype=&quot;sensorML/1.0.1/profiles/ioos_sos/1.0&quot;")
})

test_that("procedure for POX (1.0.0)", {
  ds <- SosDescribeSensor(service = testsos@name, version = testsos@version, procedure = "best_procedure", outputFormat = "a_format")
  xml <- encodeRequestXML(obj = ds, sos = testsos)
  expect_match(toString(xml), "<procedure>best_procedure</procedure>")
  expect_match(toString(xml), "outputFormat=\"a_format\"")
})

test_that("output format is encoded correctly for KVP (1.0.0)", {
    describeSensorOp <- sosOperation(testsos, sosDescribeSensorName)
    of <- describeSensorOp@parameters[["outputFormat"]][[1]]

    ds <- SosDescribeSensor(service = testsos@name, version = testsos@version,
                            procedure = sosProcedures(testsos)[[1]],
                            outputFormat = of)
    url <- encodeRequestKVP(ds, testsos)
    expect_match(toString(url), "text%2Fxml%3B%20subtype%3D%22sensorML%2F1.0.1%2Fprofiles%2Fioos_sos%2F1.0%22")

    # test different quotation and escaping variants, and spaces
    ds <- SosDescribeSensor(service = testsos@name, version = testsos@version,
                            procedure = sosProcedures(testsos)[[1]],
                            outputFormat = 'text/xml;subtype="sensorML/1.0.1"')
    url <- encodeRequestKVP(ds, testsos)
    expect_match(toString(url), "text%2Fxml%3Bsubtype%3D%22sensorML%2F1.0.1%22")

    ds <- SosDescribeSensor(service = testsos@name, version = testsos@version,
                            procedure = sosProcedures(testsos)[[1]],
                            outputFormat = "text/xml;subtype=\"sensorML/1.0.1\"")
    url <- encodeRequestKVP(ds, testsos)
    expect_match(toString(url), "text%2Fxml%3Bsubtype%3D%22sensorML%2F1.0.1%22")

    ds <- SosDescribeSensor(service = testsos@name, version = testsos@version,
                            procedure = sosProcedures(testsos)[[1]],
                            outputFormat = "text/xml; subtype=\"sensorML/1.0.1\"")
    url <- encodeRequestKVP(ds, testsos)
    expect_match(toString(url), "text%2Fxml%3B%20subtype%3D%22sensorML%2F1.0.1%22")
})

test_that("errors if procedureDescriptionFormat, validTime, or outputFormat are used with wrong service version", {
  expect_error(SosDescribeSensor(service = "dstest", version = sos100_version,
                          procedure = "better_procedure",
                          procedureDescriptionFormat = "http://procedure/format"),
               "procedureDescriptionFormat option not supported")
  expect_error(SosDescribeSensor(service = "dstest", version = sos200_version,
                                 procedure = "better_procedure",
                                 outputFormat = "http://procedure/format"),
               "outputFormat option not supported")
  expect_error(SosDescribeSensor(service = "dstest", version = sos200_version,
                                 procedure = "better_procedure",
                                 validTime = GmlTimeInstant(GmlTimePosition(parsedate::parse_iso_8601("2019-01-01")))),
               "procedureDescriptionFormat missing")
})

test_that("errors if procedureDescriptionFormat or outputFormat are missing for the used service version", {
  expect_error(SosDescribeSensor(service = "dstest", version = sos100_version,
                                 procedure = "better_procedure"),
               "outputFormat missing")
  expect_error(SosDescribeSensor(service = "dstest", version = sos200_version,
                                 procedure = "better_procedure"),
               "procedureDescriptionFormat missing")
})

test_that("procedure for KVP (2.0.0)", {
  sos <- SOS_Test(name = "sensor20", version = sos200_version)
  ds <- SosDescribeSensor(service = sos@name, version = sos@version,
                          procedure = "better_procedure",
                          procedureDescriptionFormat = sosDefaultDescribeSensorOutputFormat)
  kvp <- encodeRequestKVP(obj = ds, sos = sos)
  expect_match(kvp, "service=SOS&version=2.0.0")
  expect_match(kvp, "request=DescribeSensor")
  expect_match(kvp, "procedure=better_procedure")
  expect_match(kvp, "procedureDescriptionFormat=text%2Fxml%3Bsubtype%3D%22sensorML%2F1.0.1%22")
})

test_that("procedure description format for KVP (2.0.0)", {
  sos <- SOS_Test(name = "sensor20", version = sos200_version)
  ds <- SosDescribeSensor(service = sos@name, version = sos@version,
                          procedure = "better_procedure",
                          procedureDescriptionFormat = "http://procedure/format")
  kvp <- encodeRequestKVP(obj = ds, sos = sos)
  expect_match(kvp, "service=SOS&version=2.0.0")
  expect_match(kvp, "request=DescribeSensor")
  expect_match(kvp, "procedureDescriptionFormat=http%3A%2F%2Fprocedure%2Fformat")
})

library("webmockr")
webmockr::enable("httr")
webmockr::httr_mock()

test_that("validTime instant for KVP (2.0.0)", {
  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://example.com/sos-utils?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
    webmockr::wi_th(
      headers = list("Accept" = "application/xml")
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/Capabilities_200_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(url = "http://example.com/sos-utils", binding = "KVP", version = sos200_version)
  ds <- SosDescribeSensor(service = sosService, version = sos@version,
                          procedure = "b",
                          procedureDescriptionFormat = "f",
                          validTime = sosCreateTimeInstant(sos = sos, time = parsedate::parse_iso_8601("2019-01-01")))

  kvp <- encodeRequestKVP(obj = ds, sos = sos)
  expect_match(kvp, "validTime=2019-01-01T00%3A00%3A00")
})

test_that("validTime period for KVP (2.0.0)", {
  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://example.com/sos-utils?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
    webmockr::wi_th(
      headers = list("Accept" = "application/xml")
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/Capabilities_200_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(url = "http://example.com/sos-utils", binding = "KVP", version = sos200_version)
  ds <- SosDescribeSensor(service = sosService, version = sos@version,
                          procedure = "b",
                          procedureDescriptionFormat = "f",
                          validTime = sosCreateTimePeriod(sos = sos,
                                                          begin = parsedate::parse_iso_8601("2019-01-01"),
                                                          end = parsedate::parse_iso_8601("2019-02-02 02:22:00")))

  kvp <- encodeRequestKVP(obj = ds, sos = sos)
  expect_match(kvp, "validTime=2019-01-01T00%3A00%3A00%2B00%3A00%2F2019-02-02T02%3A22%3A00%2B00%3A00")
})

test_that("procedure and description format for POX (2.0.0)", {
  sos <- SOS_Test(name = "sensor20", version = sos200_version)
  ds <- SosDescribeSensor(service = sosService, version = sos@version, procedure = "great_procedure", procedureDescriptionFormat = "format")
  xml <- encodeRequestXML(obj = ds, sos = sos)
  expect_match(toString(xml), "great_procedure</swes:procedure>")
  expect_match(toString(xml), "format</swes:procedureDescriptionFormat>")
})

test_that("validTime instant for POX (2.0.0)", {
  sos <- SOS_Test(name = "sensor20", version = sos200_version)
  ds <- SosDescribeSensor(service = sosService, version = sos@version, procedure = "great_procedure", procedureDescriptionFormat = "format",
                          validTime = sosCreateTimeInstant(sos = sos, time = parsedate::parse_iso_8601("2019-05-05 05:05:05")))
  xml <- encodeRequestXML(obj = ds, sos = sos)
  encodedString <- stringr::str_replace_all(toString(xml), ">\\s*<", "><")
  expect_match(encodedString, "2019-05-05T05:05:05\\+00:00</gml:timePosition></gml:TimeInstant></swes:validTime>")
})

test_that("validTime period for POX (2.0.0)", {
  sos <- SOS_Test(name = "sensor20", version = sos200_version)
  ds <- SosDescribeSensor(service = sosService, version = sos@version, procedure = "great_procedure", procedureDescriptionFormat = "format",
                          validTime = sosCreateTimePeriod(sos = sos,
                                                          begin = parsedate::parse_iso_8601("2019-04-04 04:04:04"),
                                                          end = parsedate::parse_iso_8601("2019-05-05 05:05:05")))
  xml <- encodeRequestXML(obj = ds, sos = sos)
  encodedString <- stringr::str_replace_all(toString(xml), ">\\s*<", "><")
  expect_match(encodedString, "2019-04-04T04:04:04\\+00:00</gml:beginPosition>")
  expect_match(encodedString, "2019-05-05T05:05:05\\+00:00</gml:endPosition></gml:TimePeriod></swes:validTime>")
})

webmockr::disable("httr")

context("DescribeSensor: parsing sensor description")

sensorDescription <- xml2::read_xml(x = "../responses/SensorDescription_Bochum.xml")

test_that("sensor id is parsed", {
  sml <- parseSensorML(obj = sensorDescription, sos = testsos)
  expect_equal(sml@id, "http://www.52north.org/test/procedure/6")
})

test_that("full XML is saved", {
  sml <- parseSensorML(obj = sensorDescription, sos = testsos)
  expect_s3_class(sml@xml, "xml_document")
  expect_equal(xml2::xml_name(sml@xml, ns = testsos@namespaces), smlSensorMLName)
})

test_that("sensor name is parsed", {
  sml <- parseSensorML(obj = sensorDescription, sos = testsos)
  expect_equal(sml@name, "Hochschule Bochum")
})

test_that("sensor description is parsed", {
  sml <- parseSensorML(obj = sensorDescription, sos = testsos)
  expect_equal(sml@description, "A test sensor in Bochum")
})

test_that("sensor position is parsed", {
  sml <- parseSensorML(obj = sensorDescription, sos = testsos)
  expect_named(sml@coords, c("y", "x", "z"))
  expect_equal(sml@coords[["y"]], 51.447722)
  expect_equal(sml@coords[["x"]], 7.270806)
  expect_equal(sml@coords[["z"]], 52.0)
})

test_that("sensor boundedBy", {
  sml <- parseSensorML(obj = xml2::xml_root(sensorDescription), sos = testsos)
  expect_equal(dim(sosBoundedBy(sml)), c(2, 2))
  expect_equal(dimnames(sosBoundedBy(sml)), list(c("coords.lon", "coords.lat"), c("min", "max")))
})

test_that("sp bbox can be created from boundedBy", {
  sml <- parseSensorML(obj = xml2::xml_root(sensorDescription), sos = testsos)
  expect_match(toString(sp::bbox(sosBoundedBy(sml))), "7.270806, 7.370806, 51.447722, 51.557722")
})

test_that("plain SensorML description has no validTime", {
  sml <- parseSensorML(obj = sensorDescription, sos = testsos)
  expect_null(sosTime(sml))
})

context("DescribeSensor: parsing DescribeSensorResponse")

sensorResponse <- xml2::read_xml(x = "../responses/DescribeSensorResponse_ws2500.xml")

test_that("sensor id and name are parsed", {
  sos <- SOS_Test(name = "sensor20", version = sos200_version)
  sml <- parseDescribeSensorResponse(obj = sensorResponse, sos = sos)
  expect_equal(sosId(sml), "ws2500")
  expect_equal(sml@name, "52n-elv-ws2500")
})

test_that("the XML is the full response", {
  sos <- SOS_Test(name = "sensor20", version = sos200_version)
  sml <- parseDescribeSensorResponse(obj = sensorResponse, sos = sos)
  expect_s3_class(sml@xml, "xml_document")
  expect_equal(xml2::xml_name(sml@xml), swesDescribeSensorResponseName)
})

test_that("validTime is parsed from response", {
  sos <- SOS_Test(name = "sensor20", version = sos200_version)
  sml <- parseDescribeSensorResponse(obj = sensorResponse, sos = sos)
  validTime <- sosTime(sml)
  expect_true((validTime$begin - sml@validTime@beginPosition@time) == 0)
  expect_equal(toString(validTime$begin), toString(sosConvertTime("2015-05-18T13:42:42.393Z", sos = sos)))
  expect_true(is.na(validTime$end))
})

context("DescribeSensor: integration tests\n")

test_that("there is a warning if procedure is not in capabilities (1.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/pox")

  expect_warning({mySensor <- describeSensor(sos = mySOS,
                             procedure = "wrong_id",
                             outputFormat = 'text/xml; subtype="sensorML/1.0.1"')},
                 "(Requested procedure(s) not listed|The value 'wrong_id' of the parameter 'procedure' is invalid)")

  expect_s4_class(mySensor, "OwsExceptionReport")
})

test_that("there is a warning if outputFormat is not in capabilities (1.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/pox")

  expect_warning({mySensor <- describeSensor(sos = mySOS,
                                             procedure = sosProcedures(obj = mySOS)[[1]][[1]],
                                             outputFormat = "some;format")},
                 "outputFormat has to be one of.*some;format")

  expect_s4_class(mySensor, "OwsExceptionReport")
})

test_that("there is a warning if procedure is not in capabilities (2.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/kvp",
               binding = "KVP",
               version = sos200_version)

  expect_warning({mySensor <- describeSensor(sos = mySOS,
                                             procedure = "wrong_id",
                                             outputFormat = 'text/xml; subtype="sensorML/1.0.1"')},
                 "(Requested procedure(s) not listed|The value 'wrong_id' of the parameter 'procedure' is invalid)")

  expect_s4_class(mySensor, "OwsExceptionReport")
})

test_that("there is a warning if outputFormat is not in capabilities (2.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/kvp",
               binding = "KVP",
               version = sos200_version)

  expect_warning({mySensor <- describeSensor(sos = mySOS,
                                             procedure = sosProcedures(obj = mySOS)[[1]][[1]],
                                             outputFormat = "some;format")},
                 "(outputFormat has to be one of.*some;format|The value 'some;format' of the parameter 'procedureDescriptionFormat' is invalid)")

  expect_s4_class(mySensor, "OwsExceptionReport")
})

test_that("sensor description can be requested with POX (1.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/pox",
               binding = "POX", dcpFilter = list("POX" = "/pox"))

  mySensor <- describeSensor(sos = mySOS,
                             procedure = sosProcedures(obj = mySOS)[[1]][[1]],
                             outputFormat = 'text/xml; subtype="sensorML/1.0.1"')

  expect_equal(sosName(mySensor), "adxl345")
  expect_equal(sosId(mySensor), "adxl345")
  expect_s3_class(mySensor@xml, "xml_document")
  expect_named(sosCoordinates(mySensor), c("x", "y", "z"))
  expect_length(sosCoordinates(mySensor), 3)
  expect_match(toString(sp::bbox(sosBoundedBy(mySensor))), "7.5, 7.5, 52, 52")
})

test_that("sensor description can be saved to file during request with POX (1.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/pox",
               binding = "POX", dcpFilter = list("POX" = "/pox"))

  tempfile <- tempfile(fileext = ".xml")
  expect_output(
    mySensor <- describeSensor(sos = mySOS,
                             procedure = sosProcedures(obj = mySOS)[[1]][[1]],
                             outputFormat = 'text/xml; subtype="sensorML/1.0.1"',
                             saveOriginal = tempfile),
    paste0("document saved:(.*)", tempfile))

  expect_true(file.exists(tempfile))
  sensorXmlFromFile <- xml2::read_xml(tempfile)
  expect_equal(xml2::xml_name(sensorXmlFromFile), "SensorML")
  file.remove(tempfile)
  expect_false(file.exists(tempfile))
})

test_that("sensor description can be requested with KVP (2.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/kvp",
               version = sos200_version,
               binding = "KVP", useDCPs = FALSE)

  mySensor <- describeSensor(sos = mySOS,
                             procedure = sosProcedures(obj = mySOS)[[1]][[1]],
                             outputFormat = 'text/xml; subtype="sensorML/1.0.1"')

  expect_equal(sosName(mySensor), "adxl345")
  expect_equal(sosId(mySensor), "adxl345")
  expect_s3_class(mySensor@xml, "xml_document")
})

test_that("multiple sensor descriptions can be requested with KVP (2.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/kvp",
               version = sos200_version,
               binding = "KVP", useDCPs = FALSE)

  mySensors <- describeSensor(sos = mySOS,
                             procedure = c("adxl345", "apu"),
                             outputFormat = 'text/xml; subtype="sensorML/1.0.1"')

  expect_length(mySensors, 2)
  expect_true(is.list(mySensors))
  expect_equal(sosId(mySensors[[1]]), "adxl345")
  expect_equal(sosId(mySensors[[2]]), "apu")
})

test_that("sensor description can be requested with POX (2.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/pox",
               version = sos200_version,
               binding = "POX", dcpFilter = list("POX" = "/pox"))

  mySensor <- describeSensor(sos = mySOS,
                             procedure = sosProcedures(obj = mySOS)[[1]][[1]],
                             outputFormat = 'http://www.opengis.net/sensorML/1.0.1')

  expect_equal(sosName(mySensor), "adxl345")
  expect_equal(sosId(mySensor), "adxl345")
  expect_s3_class(mySensor@xml, "xml_document")
  expect_false(is.null(sosTime(mySensor)))
  expect_true(is.na(sosTime(mySensor)$end))
})

test_that("sensor description can be saved to file during request with POX (2.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/pox",
               version = sos200_version,
               binding = "POX", dcpFilter = list("POX" = "/pox"))

  tempfile <- tempfile(fileext = ".xml")
  expect_output(
    mySensor <- describeSensor(sos = mySOS,
                               procedure = sosProcedures(obj = mySOS)[[1]][[1]],
                               outputFormat = "http://www.opengis.net/sensorML/1.0.1",
                               saveOriginal = tempfile),
    paste0("document saved:(.*)", tempfile))

  expect_true(file.exists(tempfile))
  sensorXmlFromFile <- xml2::read_xml(tempfile)
  expect_equal(xml2::xml_name(sensorXmlFromFile), swesDescribeSensorResponseName)
  file.remove(tempfile)
  expect_false(file.exists(tempfile))
})

context("DescribeSensor: SensorML 2.0 Test (see https://github.com/52North/sos4R/issues/135)")

test_that("sensor description can be requested in SensorML 2.0 manually", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/pox",
               version = sos200_version, useDCPs = FALSE,
               parsers = SosParsingFunctions("DescribeSensorResponse" = function(obj, sos, verbose) {
                 #passthrough
                 return(obj)
               }))
  mySOS@namespaces[["sml"]] <- "http://www.opengis.net/sensorml/2.0"

  mySensorXml <- describeSensor(sos = mySOS,
                             procedure = sosProcedures(obj = mySOS)[[1]][[1]],
                             outputFormat = "http://www.opengis.net/sensorml/2.0")

  expect_equal(xml2::xml_name(mySensorXml), swesDescribeSensorResponseName)
  expect_match(toString(mySensorXml), "<gml:identifier codeSpace=\"uniqueID\">adxl345</gml:identifier>")
})
