context("GetObservationById: POX encoding")

test_that("minimal", {
  testsos <- SOS_Test(name = "getobsbyidpox")
  getobs <- SosGetObservationById(service = "ser",
                                  version = "ver",
                                  observationId = "http://obs/1",
                                  responseFormat = "fmt"
  )
  request <- encodeRequestXML(obj = getobs, sos = testsos)
  expect_match(toString(request), 'service="ser"')
  expect_match(toString(request), ' version="1.0.0"')
  expect_match(toString(request), "<sos:ObservationId>http://obs/1</sos:ObservationId>")
  expect_match(toString(request), "<sos:responseFormat>fmt</sos:responseFormat>")
})

context("GetObservationById: integration test\n")

test_that("KVP (SOS 2.0.0 - not implemented)", {
  skip_on_cran()

  expect_warning(
    mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/kvp",
               binding = "KVP", useDCPs = FALSE,
               version = sos200_version)
  )
  expect_error(
    getObservationById(sos = mySOS, observationId = "http://www.52north.org/test/observation/1"),
    "not implemented")
})

test_that("KVP (SOS 1.0.0 - not supported)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/kvp", binding = "KVP")
  expect_error(getObservationById(sos = mySOS, observationId = "http://www.52north.org/test/observation/1"), "not supported")
})

test_that("POX (SOS 1.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/pox",
               binding = "POX", useDCPs = FALSE)
  obs <- getObservationById(sos = mySOS, observationId = "http://www.52north.org/test/observation/1",
                            #verbose = TRUE,
                            #inspect = TRUE
                            )
  data <- sosResult(obs)
  expect_s4_class(obs, "OmObservationCollection")
  expect_length(obs, 1)

  expect_s4_class(obs[[1]], "OmObservation")
  expect_s3_class(sosResult(obs), "data.frame")
  expect_equal(sosResult(obs)[1,1], 1)
})
