context("parsing: exception reports")

er1 <- "<ows:ExceptionReport xmlns:ows=\"http://www.opengis.net/ows/1.1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" version=\"1.0.0\" xsi:schemaLocation=\"http://schemas.opengis.net/ows/1.1.0/owsExceptionReport.xsd\"><ows:Exception exceptionCode=\"VersionNegotiationFailed\" locator=\"AcceptVersions\"><ows:ExceptionText>The parameter 'AcceptVersions' does not contain the version of this SOS: '1.0.0'</ows:ExceptionText></ows:Exception></ows:ExceptionReport>"
er2 <- "<ows:ExceptionReport xmlns:ows=\"http://www.opengis.net/ows/1.1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" version=\"1.0.0\" lang=\"de-DE\" xsi:schemaLocation=\"http://schemas.opengis.net/ows/1.1.0/owsExceptionReport.xsd\"><ows:Exception exceptionCode=\"VersionNegotiationFailed\" locator=\"AcceptVersions\"><ows:ExceptionText>The parameter 'AcceptVersions' does not contain the version of this SOS: '1.0.0'</ows:ExceptionText></ows:Exception><ows:Exception exceptionCode=\"NoApplicableCode\" locator=\"@home\"><ows:ExceptionText>Just a second exception to make things saver...</ows:ExceptionText></ows:Exception></ows:ExceptionReport>"

test_that("exception is parsed from exception report", {
  er1.doc <- xml2::xml_root(x = xml2::read_xml(x = er1))
  exceptionReport <- parseOwsExceptionReport(obj = er1.doc) #, verbose = TRUE)
  expect_length(exceptionReport@exceptions, 1)
})

test_that("multiple exceptions are parsed from exception report", {
  er2.doc <- xml2::xml_root(x = xml2::read_xml(x = er2))
  exceptionReport <- parseOwsExceptionReport(obj = er2.doc) #, verbose = TRUE)
  expect_length(exceptionReport@exceptions, 2)
})

test_that("version is parsed from exception report", {
  er1.doc <- xml2::xml_root(x = xml2::read_xml(x = er1))
  er2.doc <- xml2::xml_root(x = xml2::read_xml(x = er2))

  exceptionReport <- parseOwsExceptionReport(obj = er1.doc) #, verbose = TRUE)
  expect_equal(exceptionReport@version, "1.0.0")
  exceptionReport <- parseOwsExceptionReport(obj = er2.doc) #, verbose = TRUE)
  expect_equal(exceptionReport@version, "1.0.0")
})

test_that("language is parsed from exception report", {
  er1.doc <- xml2::xml_root(x = xml2::read_xml(x = er1))
  er2.doc <- xml2::xml_root(x = xml2::read_xml(x = er2))
  exceptionReport <- parseOwsExceptionReport(obj = er1.doc) #, verbose = TRUE)
  expect_equal(exceptionReport@lang, NA_character_)
  exceptionReport <- parseOwsExceptionReport(obj = er2.doc) #, verbose = TRUE)
  expect_equal(exceptionReport@lang, "de-DE")
})

test_that("text is parsed from one exception", {
  er1.doc <- xml2::xml_root(x = xml2::read_xml(x = er1))
  exceptionReport <- parseOwsExceptionReport(obj = er1.doc) #, verbose = TRUE)
  expect_match(exceptionReport@exceptions[[1]]@exceptionText, "does not contain the version of this SOS")
})

test_that("code is parsed from one exception", {
  er1.doc <- xml2::xml_root(x = xml2::read_xml(x = er1))
  exceptionReport <- parseOwsExceptionReport(obj = er1.doc) #, verbose = TRUE)
  expect_equal(exceptionReport@exceptions[[1]]@exceptionCode, "VersionNegotiationFailed")
})

test_that("locator is parsed from one exception", {
  er1.doc <- xml2::xml_root(x = xml2::read_xml(x = er1))
  exceptionReport <- parseOwsExceptionReport(obj = er1.doc) #, verbose = TRUE)
  expect_equal(exceptionReport@exceptions[[1]]@locator, "AcceptVersions")
})

test_that("text, code, and locator are parsed from second exception", {
  er2.doc <- xml2::xml_root(x = xml2::read_xml(x = er2))
  exceptionReport <- parseOwsExceptionReport(obj = er2.doc) #, verbose = TRUE)

  expect_match(exceptionReport@exceptions[[2]]@exceptionText, "Just a second exception")
  expect_equal(exceptionReport@exceptions[[2]]@exceptionCode, "NoApplicableCode")
  expect_equal(exceptionReport@exceptions[[2]]@locator, "@home")
})

context("Exception handling: integration test\n")

test_that("wrong parameters return exception to user", {
  skip_on_cran()

  sos <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/pox",
             binding = "POX", useDCPs = FALSE)
  expect_warning(response <- getObservation(sos,
                             offering = sosOfferings(sos)[[1]],
                             observedProperty = list("Bazinga!")), "OwsExceptionReport")
  expect_s4_class(response, "OwsExceptionReport")
  expect_length(response@exceptions, 1)
  expect_equal(response@exceptions[[1]]@exceptionCode, "InvalidParameterValue")
  expect_match(response@exceptions[[1]]@exceptionText, "Bazinga!")
})
