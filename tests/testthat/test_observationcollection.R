context("parsing: observation collection")

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
  expect_s4_class(encoding, "SweTextBlock")
  expect_equal(encoding@tokenSeparator, ",")
  expect_equal(encoding@decimalSeparator, ".")
  expect_equal(encoding@blockSeparator, "@@")
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
