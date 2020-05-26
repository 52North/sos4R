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
# Created: 2019-03-26                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
context("parsing: SOS Capabilities 2.0.0")

testsos <- SOS_Test(name = "testcaps", version = sos200_version)
xmlDoc <- xml2::read_xml(x = "../responses/Capabilities_200_Example.xml", options = SosDefaultParsingOptions())
sos200Caps <- parseSosCapabilities(obj = xmlDoc, sos = testsos)
testsos@capabilities <- sos200Caps
#
# test identification abstract and title from ows service identification ----
#
test_that("identification snippet", {
  ident <- parseOwsServiceIdentification(obj = xml2::xml_child(xmlDoc, owsServiceIdentificationName),
                                         sos = testsos)
  expect_match(sosTitle(ident), "Wupperverband SOS")
  expect_match(sosAbstract(ident), "Wupperverband(.*)Catchment Area")
})
#
# test identification abstract and title from capabilities ----
#
test_that("identification from capabilities", {
  expect_match(sosTitle(testsos), "Wupperverband SOS")
  expect_match(sosAbstract(testsos), "Wupperverband(.*)Catchment Area")
})
#
# test keywords ----
#
test_that("keywords", {
  expect_length(sosServiceIdentification(testsos)@keywords, 3)
})
#
# test fees ----
#
test_that("fees", {
  expect_equal(sosServiceIdentification(testsos)@fees, "NONE")
})
#
# test access constraints ----
#
test_that("access constraints", {
  expect_match(sosServiceIdentification(testsos)@accessConstraints, "http://fluggs.wupperverband.de")
})
#
# test profiles ----
#
test_that("profiles", {
  expect_equal(sosServiceIdentification(testsos)@profile[11], "http://www.opengis.net/spec/SOS/2.0/conf/core")
  expect_length(sosServiceIdentification(testsos)@profile, 34)
})
#
# test content: offerings ----
#
test_that("content: offerings", {
  expect_length(sosOfferings(testsos), 8)
  expect_equal(sosName(sosOfferings(testsos)[[2]]), "Zeitreihen für Prozedur Einzelwert")
})
#
# test content: offering IDs ----
#
test_that("content: offering IDs", {
  expect_length(sosOfferingIds(testsos), 8)
  expect_equal(sosOfferingIds(testsos)[[2]], "Zeitreihen_Einzelwert")
})

context("parsing: SOS Capabilities 2.0.0 swes:offering")

swes_offering <- '<?xml version="1.0" encoding="UTF-8"?>
<swes:offering xmlns:sos="http://www.opengis.net/sos/2.0"
xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink"
xmlns:swes="http://www.opengis.net/swes/2.0" xmlns:gml="http://www.opengis.net/gml/3.2"
xsi:schemaLocation="http://www.opengis.net/swes/2.0 http://schemas.opengis.net/swes/2.0/swes.xsd http://www.opengis.net/gml/3.2 http://schemas.opengis.net/gml/3.2.1/gml.xsd">
<sos:ObservationOffering>
<swes:identifier>ws2500</swes:identifier>
<swes:procedure>ws2500</swes:procedure>
<swes:procedureDescriptionFormat>http://www.opengis.net/sensorML/1.0.1</swes:procedureDescriptionFormat>
<swes:procedureDescriptionFormat>http://www.opengis.net/waterml/2.0/observationProcess</swes:procedureDescriptionFormat>
<swes:observableProperty>AirTemperature</swes:observableProperty>
<swes:observableProperty>AthmosphericPressure</swes:observableProperty>
<swes:observableProperty>Dewpoint</swes:observableProperty>
<swes:observableProperty>Humidity</swes:observableProperty>
<swes:observableProperty>Luminance</swes:observableProperty>
<swes:observableProperty>RainfallAccumulated</swes:observableProperty>
<swes:observableProperty>Sunshine</swes:observableProperty>
<swes:observableProperty>WindDirection</swes:observableProperty>
<swes:observableProperty>WindSpeed</swes:observableProperty>
<swes:observableProperty>Windchill</swes:observableProperty>
<swes:relatedFeature>
<swes:FeatureRelationship>
<swes:target xlink:href="ELV-WS2500" />
</swes:FeatureRelationship>
</swes:relatedFeature>
<sos:observedArea>
<gml:Envelope srsName="http://www.opengis.net/def/crs/EPSG/0/4326">
<gml:lowerCorner>51.934814453125 7.652428150177</gml:lowerCorner>
<gml:upperCorner>51.934814453125 7.652428150177002</gml:upperCorner>
</gml:Envelope>
</sos:observedArea>
<sos:phenomenonTime>
<gml:TimePeriod gml:id="phenomenonTime_1">
<gml:beginPosition>2015-05-18T08:35:00.000Z</gml:beginPosition>
<gml:endPosition>2015-12-02T12:00:00.000Z</gml:endPosition>
</gml:TimePeriod>
</sos:phenomenonTime>
<sos:resultTime>
<gml:TimePeriod gml:id="resultTime_1">
<gml:beginPosition>2015-05-18T13:35:00.000Z</gml:beginPosition>
<gml:endPosition>2015-12-02T12:00:00.000Z</gml:endPosition>
</gml:TimePeriod>
</sos:resultTime>
<sos:responseFormat>application/json</sos:responseFormat>
<sos:responseFormat>http://dd.eionet.europa.eu/schemaset/id2011850eu-1.0</sos:responseFormat>
<sos:responseFormat>http://www.opengis.net/om/2.0</sos:responseFormat>
<sos:responseFormat>http://www.opengis.net/waterml-dr/2.0</sos:responseFormat>
<sos:responseFormat>http://www.opengis.net/waterml/2.0</sos:responseFormat>
<sos:observationType>http://www.opengis.net/def/observationType/OGC-OM/2.0/OM_Measurement</sos:observationType>
<sos:featureOfInterestType>http://www.opengis.net/def/samplingFeatureType/OGC-OM/2.0/SF_SamplingPoint</sos:featureOfInterestType>
<sos:featureOfInterestType>http://www.opengis.net/def/samplingFeatureType/OGC-OM/2.0/SF_SamplingPolygon</sos:featureOfInterestType>
</sos:ObservationOffering>
</swes:offering>'
#
# test offering is parsed correctly ----
#
test_that("offering is parsed correctly", {
  offering <- parseSosObservationOffering_200(obj = xml2::read_xml(x = swes_offering), sos = testsos)
  expect_equal(offering@id, "ws2500")
  expect_equal(offering@name, NA_character_)
  expect_length(sosObservableProperties(offering), 10)
  expect_length(sosProcedures(offering), 1)
  expect_length(sosResponseFormats(offering), 5)
  expect_equal(sosResponseFormats(offering)[[4]], "http://www.opengis.net/waterml-dr/2.0")
  expect_length(offering@featureOfInterestType, 2)
  expect_equal(offering@observationType[[1]], "http://www.opengis.net/def/observationType/OGC-OM/2.0/OM_Measurement")
  expect_match(toString(offering@resultTime), "--> GmlTimePosition \\[ time: 2015-12-02")
  expect_match(toString(offering@phenomenonTime), "--> GmlTimePosition \\[ time: 2015-12-02")
  expect_length(sosBoundedBy(offering), 3)
  expect_named(sosBoundedBy(offering), c("srsName", "lowerCorner", "upperCorner"))
  expect_equal(sosBoundedBy(offering)$lowerCorner, "51.934814453125 7.652428150177")
  expect_equal(sosBoundedBy(offering)$srsName, "http://www.opengis.net/def/crs/EPSG/0/4326")
})

context("capabilities: NIWA 2.0 SOS")
# Capabilities taken from https://climate-sos.niwa.co.nz/?service=SOS&version=2.0.0&request=GetCapabilities

niwa <- SOS_Test(name = "testniwa")
niwa@capabilities <- parseSosCapabilities200(obj = xml2::read_xml("../responses/climate-sos.niwa.co.nz.xml"), sos = testsos)
#
# test provider is parsed correctly ----
#
test_that("provider is parsed correctly", {
  expect_equal(sosServiceProvider(niwa)@providerName, "NIWA")
})


context("parsing: OwsOperation 2.0.0 with Content-Type")

operationXml200 <- '<ows:Operation name="DescribeSensor" xmlns:sos="http://www.opengis.net/sos/1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ows="http://www.opengis.net/ows/1.1" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" version="1.0.0">
			<ows:DCP>
				<ows:HTTP>
					<ows:Get xlink:href="http://ioossos.axiomalaska.com/52n-sos-ioos-dev/sos/kvp?">
						<ows:Constraint name="Content-Type">
							<ows:AllowedValues>
								<ows:Value>application/x-kvp</ows:Value>
							</ows:AllowedValues>
						</ows:Constraint>
					</ows:Get>
					<ows:Post xlink:href="http://ioossos.axiomalaska.com/52n-sos-ioos-dev/sos/pox">
						<ows:Constraint name="Content-Type">
							<ows:AllowedValues>
								<ows:Value>application/xml</ows:Value>
								<ows:Value>text/xml</ows:Value>
							</ows:AllowedValues>
						</ows:Constraint>
					</ows:Post>
					<ows:Post xlink:href="http://ioossos.axiomalaska.com/52n-sos-ioos-dev/sos/soap">
						<ows:Constraint name="Content-Type">
							<ows:AllowedValues>
								<ows:Value>application/soap+xml</ows:Value>
							</ows:AllowedValues>
						</ows:Constraint>
					</ows:Post>
				</ows:HTTP>
			</ows:DCP>
			<ows:Parameter name="outputFormat">
				<ows:AllowedValues>
					<ows:Value>text/xml; subtype="sensorML/1.0.1/profiles/ioos_sos/1.0"</ows:Value>
				</ows:AllowedValues>
			</ows:Parameter>
		</ows:Operation>'
#
# test name parsing operation::name ----
#
test_that("name parsing of operations", {
  doc <- xml2::read_xml(x = operationXml200)
  operation <- parseOwsOperation(obj = doc, sos = testsos)
  expect_equal(sosName(operation), "DescribeSensor")
})
#
# test parsing operation::DCP ----
#
test_that("DCP", {
  doc <- xml2::read_xml(x = operationXml200)
  operation <- parseOwsOperation(obj = doc, sos = testsos)
  expect_length(operation@DCPs, 4)
  expect_named(operation@DCPs, c("ows:Get", "ows:Post", "ows:Post", "ows:Post"))
  expect_equal(operation@DCPs[[3]], list("ows:Post", "text/xml", "http://ioossos.axiomalaska.com/52n-sos-ioos-dev/sos/pox"))
})

#
# test name parsing operation::parameter::name ----
#
test_that("parameter names", {
  doc <- xml2::read_xml(x = operationXml200)
  operation <- parseOwsOperation(obj = doc, sos = testsos)
  expect_named(operation@parameters, c("outputFormat"))
})

#
# test name parsing operation::parameter::value ----
#
test_that("parameter values", {
  doc <- xml2::read_xml(x = operationXml200)
  operation <- parseOwsOperation(obj = doc, sos = testsos)
  expect_length(operation@parameters[["outputFormat"]], 1)
  expect_equal(operation@parameters[["outputFormat"]], list('text/xml; subtype="sensorML/1.0.1/profiles/ioos_sos/1.0"'))
})
#
# test summary of SOS ----
#
test_that("summary(SOS_200) returns no error", {
  webmockr::enable("httr")
  webmockr::httr_mock()
  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/kvp?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
    webmockr::wi_th(
      headers = list("Accept" = "application/xml")
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/Capabilities_200_for-summary.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  summaryTestSos <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/kvp",
                        version = sos200_version, binding = "KVP", useDCPs = FALSE)
  summary <- summary(summaryTestSos)

  expect_length(summary, 10)
  expect_equal(summary$class[[1]], "SOS_2.0.0")
  expect_equal(summary$version, "2.0.0")
  expect_equal(summary$url, "http://sensorweb.demo.52north.org/sensorwebtestbed/service/kvp")
  expect_equal(summary$binding, "KVP")
  expect_equal(summary$title, "52N SOS")
  expect_equal(summary$abstract, "52North Sensor Observation Service - Data Access for the Sensor Web")
  # TODO test time elemenent in summary
  #expect_equal(summary$time, "TBD")
  expect_equal(summary$offeringCount, 4)
  expect_equal(summary$procedureCount, 4)
  expect_equal(summary$observablePropCount, 43)
  expect_true(inherits(summary, "summary.SOS_versioned"))

  webmockr::stub_registry_clear()
  webmockr::disable("httr")
})
