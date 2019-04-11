context("extensions")

test_that("replacing a parsing function (integration test)", {
  skip_on_cran()

  myER <- function(xml, sos, verbose) {
    return("EXCEPTION!!!11")
  }
  myParsers <- SosParsingFunctions("ows:ExceptionReport" = myER)
  cat("\n")
  exceptionParserSOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/pox",
                            parsers = myParsers,
                            binding = "POX", useDCPs = FALSE)
  erroneousResponse <- getObservation(exceptionParserSOS,
                                      #verbose = TRUE,
                                      offering = sosOfferings(exceptionParserSOS)[[1]],
                                      observedProperty = list("Bazinga!"))
  expect_equal(erroneousResponse, "EXCEPTION!!!11")
})

test_that("disabling all parsing functions (integration test)", {
  skip_on_cran()

  disabledParserSOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/pox",
                           parsers = SosDisabledParsers(),
                           binding = "POX", useDCPs = FALSE)
  unparsed <- getObservation(disabledParserSOS,
                             offering = sosOfferings(disabledParserSOS)[[1]],
                             observedProperty = list("Bazinga!"))
  expect_s3_class(unparsed, c("xml_document", "xml_node"))
})

