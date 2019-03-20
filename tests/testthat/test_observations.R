context("parsing: observation collection")

parseXmlSnippet <- function(obj) {
  doc <- xml2::read_xml(x = obj, options = SosDefaultParsingOptions())
  docRoot <- xml2::xml_root(x = doc)
  return(docRoot)
}

testsos <- SOS_Test(name = "testobscoll")

test_that("bounded by is parsed from collection", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml", options = SosDefaultParsingOptions())

  collection <- parseObservationCollection(obj = doc, sos = testsos)
  expect_false(is.null(collection@boundedBy))
  expect_named(collection@boundedBy, c("srsName", "lowerCorner", "upperCorner"))
  expect_equal(collection@boundedBy$srsName, "http://www.opengis.net/def/crs/EPSG/0/4326")
})

test_that("all members are parsed from collection", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml", options = SosDefaultParsingOptions())
  collection <- parseObservationCollection(obj = doc, sos = testsos)
  expect_equal(length(collection), 2)
  expect_equal(length(collection@members), 2)
})

context("parsing: observation")

test_that("id parsed from first observation, but not from second", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml", options = SosDefaultParsingOptions())

  collection <- parseObservationCollection(obj = doc, sos = testsos)
  expect_equal(collection@members[[1]]@id, "1234")
  expect_true(is.na(collection@members[[2]]@id))
})

context("parsing: DataArray")

test_that("parse encoding", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml", options = SosDefaultParsingOptions())
  encodingXml <- xml2::xml_find_first(x = doc, xpath = "//swe:encoding", ns = SosAllNamespaces())

  encoding <- parseEncoding(obj = encodingXml, sos = testsos)
  expect_s4_class(encoding, "SweTextBlock")
  expect_equal(encoding@tokenSeparator, ",")
  expect_equal(encoding@decimalSeparator, ".")
  expect_equal(encoding@blockSeparator, "@@")
})

test_that("parse time field", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml", options = SosDefaultParsingOptions())
  fieldsXml <- xml2::xml_find_all(x = doc, xpath = "//swe:field", ns = SosAllNamespaces())

  timeField <- parseField(obj = fieldsXml[[1]], sos = testsos)
  expect_equal(timeField[[.sosParseFieldName]], "time")
  expect_equal(timeField[[".sosParseFieldDefinition"]], "urn:ogc:property:time:iso8601")
})

test_that("parse quantity field", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml", options = SosDefaultParsingOptions())
  fieldsXml <- xml2::xml_find_all(x = doc, xpath = "//swe:field", ns = SosAllNamespaces())

  quantityField <- parseField(obj = fieldsXml[[3]], sos = testsos)
})

test_that("parse category field", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml", options = SosDefaultParsingOptions())
  fieldsXml <- xml2::xml_find_all(x = doc, xpath = "//swe:field", ns = SosAllNamespaces())

  categoryField <- parseField(obj = fieldsXml[[6]], sos = testsos)

})

test_that("parse data array (i.e. parse the values using encoding and fields", {
  doc <- xml2::read_xml(x = "../responses/sosObservationCollection1.xml", options = SosDefaultParsingOptions())
  dataArrayXml <- xml2::xml_find_first(x = doc, xpath = "//swe:DataArray", ns = SosAllNamespaces())

  dataArray <- parseDataArray(obj = dataArrayXml, sos = testsos)

  expect_equal(collection@members[[1]]@id, "1234")
  expect_true(is.na(collection@members[[2]]@id))
})

context("parsing: composite phenomenon")

compositePhenomenon <- '<swe:CompositePhenomenon xmlns:gml="http://www.opengis.net/gml"
xmlns:swe="http://www.opengis.net/swe/1.0.1" xmlns:xlink="http://www.w3.org/1999/xlink"
gml:id="WaterQuality" dimension="4">
<gml:name>WaterQuality</gml:name>
<swe:component xlink:href="urn:ogc:def:property:OGC-SWE:1:ID"/>
<swe:component xlink:href="urn:ogc:def:property:OGC-SWE:2:ID"/>
</swe:CompositePhenomenon>'

testsos <- SOS_Test(name = "testcompphen")
axiomCaps <- parseSosCapabilities(xml2::read_xml(x = "../responses/Capabilities_52N-SOS_Axiom.xml"), testsos)
testsos@capabilities <- axiomCaps

test_that("composite phenomenon name is parsed from snippet", {
  doc <- parseXmlSnippet(compositePhenomenon)
  phen <- parseCompositePhenomenon(obj = doc) #, verbose = TRUE)
  expect_equal(phen@name, "WaterQuality")
})

test_that("composite phenomenon id is parsed from snippet", {
  doc <- parseXmlSnippet(compositePhenomenon)
  phen <- parseCompositePhenomenon(obj = doc)
  expect_equal(phen@id, "WaterQuality")
})

test_that("composite phenomenon dimension is parsed from snippet", {
  doc <- parseXmlSnippet(compositePhenomenon)
  phen <- parseCompositePhenomenon(obj = doc)
  expect_equal(phen@dimension, 4)
})

test_that("composite phenomenon components are parsed from snippet", {
  doc <- parseXmlSnippet(compositePhenomenon)
  phen <- parseCompositePhenomenon(obj = doc)
  expect_equal(length(phen@components), 2)
  expect_equal(phen@components[[2]]@href, "urn:ogc:def:property:OGC-SWE:2:ID")
})

context("parsing: time")

samplingTimeInstantXml <- '<om:samplingTime xmlns:om="http://www.opengis.net/om/1.0" xmlns:gml="http://www.opengis.net/gml">
    <gml:TimeInstant>
      <gml:timePosition>2005-08-05T12:21:13Z</gml:timePosition>
    </gml:TimeInstant>
  </om:samplingTime>'

test_that("time instant", {
  doc <- parseXmlSnippet(samplingTimeInstantXml)
  samplingTime <- parseTime(obj = doc, format = testsos@timeFormat)
  expect_equal(strftime(samplingTime@timePosition@time, format = "%Y%d"), "200505")
  expect_s3_class(sosTime(samplingTime), "POSIXlt")
})

samplingTimePeriodXml <- '<om:samplingTime xmlns:om="http://www.opengis.net/om/1.0" xmlns:gml="http://www.opengis.net/gml">
    <gml:TimePeriod gml:id="phenomenonTime_1">
				<gml:beginPosition>2015-05-18T08:35:00.000Z</gml:beginPosition>
				<gml:endPosition>2015-12-02T12:00:00.000Z</gml:endPosition>
			</gml:TimePeriod>
  </om:samplingTime>'

test_that("time period", {
  doc <- parseXmlSnippet(samplingTimePeriodXml)
  samplingTime <- parseTime(obj = doc, format = testsos@timeFormat)
  expect_equal(samplingTime@id, "phenomenonTime_1")
  expect_s4_class(samplingTime@beginPosition, "GmlTimePosition")
  expect_s4_class(samplingTime@endPosition, "GmlTimePosition")
})

samplingTimeReferenceXml <- '<om:samplingTime xmlns:om="http://www.opengis.net/om/1.0" xlink:href="#abc" xmlns:xlink="http://www.w3.org/1999/xlink" />'

test_that("time reference", {
  doc <- parseXmlSnippet(samplingTimeReferenceXml)
  samplingTime <- parseTime(obj = doc, format = testsos@timeFormat)
  expect_equal(samplingTime@href, "#abc")
})
