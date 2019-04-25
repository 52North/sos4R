context("parsing: SF SpatialSampling")
library(webmockr)
library(httr)
library(stringr)
webmockr::enable("httr")
webmockr::httr_mock()
sams1 <- '<sams:SF_SpatialSamplingFeature gml:id="ssf_88204F34D0B94590AA1EDE21577C9B5D907F4BAD"
    xmlns:gml="http://www.opengis.net/gml/3.2"
    xmlns:sf="http://www.opengis.net/sampling/2.0"
    xmlns:sams="http://www.opengis.net/samplingSpatial/2.0"
    xmlns:xlink="http://www.w3.org/1999/xlink">
  <gml:identifier codeSpace="http://www.opengis.net/def/nil/OGC/0/unknown">foi-1</gml:identifier>
  <gml:name codeSpace="http://www.opengis.net/def/nil/OGC/0/unknown">foi one</gml:name>
  <sf:type xlink:href="http://www.opengis.net/def/samplingFeatureType/OGC-OM/2.0/SF_SamplingPoint"/>
  <sf:sampledFeature xlink:href="http://www.52north.org/test/featureOfInterest/world"/>
  <sams:shape>
    <gml:Point gml:id="gml-id-p1">
      <gml:pos srsName="http://www.opengis.net/def/crs/EPSG/0/4326">51.883906 7.727958</gml:pos>
    </gml:Point>
  </sams:shape>
</sams:SF_SpatialSamplingFeature>'
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

testsos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")

test_that("id is parsed", {
  sams.doc <- xml2::xml_root(x = xml2::read_xml(x = sams1))
  samsSF <- parseSams200SamplingFeature(obj = sams.doc, sos = testsos)
  expect_equal(samsSF@id, "ssf_88204F34D0B94590AA1EDE21577C9B5D907F4BAD")
})

test_that("identifier is parsed", {
  sams.doc <- xml2::xml_root(x = xml2::read_xml(x = sams1))
  samsSF <- parseSams200SamplingFeature(obj = sams.doc, sos = testsos)
  expect_equal(samsSF@identifier, "foi-1")
})

test_that("name is parsed", {
  sams.doc <- xml2::xml_root(x = xml2::read_xml(x = sams1))
  samsSF <- parseSams200SamplingFeature(obj = sams.doc, sos = testsos)
  expect_equal(samsSF@name, "foi one")
})

test_that("type is parsed", {
  sams.doc <- xml2::xml_root(x = xml2::read_xml(x = sams1))
  samsSF <- parseSams200SamplingFeature(obj = sams.doc, sos = testsos)
  expect_equal(samsSF@type, "http://www.opengis.net/def/samplingFeatureType/OGC-OM/2.0/SF_SamplingPoint")
})

test_that("sampled feature is parsed", {
  sams.doc <- xml2::xml_root(x = xml2::read_xml(x = sams1))
  samsSF <- parseSams200SamplingFeature(obj = sams.doc, sos = testsos)
  expect_equal(samsSF@sampledFeature, "http://www.52north.org/test/featureOfInterest/world")
})

test_that("shape is parsed", {
  sams.doc <- xml2::xml_root(x = xml2::read_xml(x = sams1))
  samsSF <- parseSams200SamplingFeature(obj = sams.doc, sos = testsos)

  expect_s4_class(samsSF@shape, "SamsShape")
  expect_equal(sosCoordinates(samsSF), sosCoordinates(samsSF@shape))
  coords <- sosCoordinates(samsSF)
  expect_named(coords, c("lat", "lon", "SRS"))
  expect_lte(coords$lat - 51.88391, 0.000000000001)
  expect_lte(coords$lon - 7.727958, 0.000000000001)
  expect_equal(as.character(coords$SRS), "http://www.opengis.net/def/crs/EPSG/0/4326")
})

webmockr::disable("httr")
