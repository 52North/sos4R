context("parsing: SOS Capabilities 2.0.0")

testsos <- SOS_Test(name = "testcaps", version = sos200_version
                    #, verboseOutput = TRUE
                    )
xmlDoc <- xml2::read_xml(x = "../responses/Capabilities_200_Example.xml", options = SosDefaultParsingOptions())
sos200Caps <- parseSosCapabilities(obj = xmlDoc, sos = testsos)
testsos@capabilities <- sos200Caps

test_that("identification snippet", {
  ident <- parseOwsServiceIdentification(obj = xml2::xml_child(xmlDoc, owsServiceIdentificationName),
                                         namespaces = SosAllNamespaces(sos200_version))
  expect_match(sosTitle(ident), "Wupperverband SOS")
  expect_match(sosAbstract(ident), "Wupperverband(.*)Catchment Area")
})

test_that("identification from capabilities", {
  expect_match(sosTitle(testsos), "Wupperverband SOS")
  expect_match(sosAbstract(testsos), "Wupperverband(.*)Catchment Area")
})

test_that("keywords", {
  expect_length(sosServiceIdentification(testsos)@keywords, 3)
})

test_that("fees", {
  expect_equal(sosServiceIdentification(testsos)@fees, "NONE")
})

test_that("access constraints", {
  expect_match(sosServiceIdentification(testsos)@accessConstraints, "http://fluggs.wupperverband.de")
})

test_that("profiles", {
  expect_equal(sosServiceIdentification(testsos)@profile[11], "http://www.opengis.net/spec/SOS/2.0/conf/core")
  expect_length(sosServiceIdentification(testsos)@profile, 34)
})

test_that("content: offerings", {
  expect_length(sosOfferings(testsos), 8)
  expect_equal(sosName(sosOfferings(testsos)[[2]]), "Zeitreihen für Prozedur Einzelwert")
})

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

test_that("offering is parsed correctly", {
  obs <- parseSosObservationOffering_200(obj = xml2::read_xml(x = swes_offering), sos = testsos)
  expect_equal(obs@id, "ws2500")
  expect_equal(obs@name, NA_character_)
  expect_length(sosObservableProperties(obs), 10)
  expect_length(sosProcedures(obs), 1)
  expect_length(sosResponseFormats(obs), 5)
  expect_equal(sosResponseFormats(obs)[[4]], "http://www.opengis.net/waterml-dr/2.0")
  expect_length(obs@featureOfInterestType, 2)
  expect_equal(obs@observationType[[1]], "http://www.opengis.net/def/observationType/OGC-OM/2.0/OM_Measurement")
  expect_match(toString(obs@resultTime), "--> GmlTimePosition \\[ time: 2015-12-02")
  expect_match(toString(obs@phenomenonTime), "--> GmlTimePosition \\[ time: 2015-12-02")
})

context("capabilities: NIWA 2.0 SOS")
# Capabilities taken from https://climate-sos.niwa.co.nz/?service=SOS&version=2.0.0&request=GetCapabilities

niwa <- SOS_Test(name = "testniwa")
niwa@capabilities <- parseSosCapabilities200(obj = xml2::read_xml("../responses/climate-sos.niwa.co.nz.xml"), sos = testsos)

test_that("provider is parsed correctly", {
  expect_equal(sosServiceProvider(niwa)@providerName, "NIWA")
})
