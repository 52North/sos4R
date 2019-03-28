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

test_that("KVP", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service", binding = "KVP")
  expect_error(getObservationById(sos = mySOS, observationId = "http://www.52north.org/test/observation/1"), "not supported")
})

test_that("POX", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/pox",
               binding = "POX",
               useDCPs = FALSE)
  obs <- getObservationById(sos = mySOS, observationId = "http://www.52north.org/test/observation/1",
                            #verbose = TRUE,
                            #inspect = TRUE
                            )
  data <- sosResult(obs)

  # http:sensorweb.demo.52north.org/52n-sos-webapp/client?method=POST&accept=application%2Fxml&contentType=application%2Fxml&url=http%3A%2F%2Fsensorweb.demo.52north.org%2F52n-sos-webapp%2Fservice&request=%3C%3Fxml%20version%3D%221.0%22%20encoding%3D%22UTF-8%22%3F%3E%0A%3CGetObservationById%20service%3D%22SOS%22%20version%3D%221.0.0%22%20xmlns%3D%22http%3A%2F%2Fwww.opengis.net%2Fsos%2F1.0%22%20xmlns%3Axsi%3D%22http%3A%2F%2Fwww.w3.org%2F2001%2FXMLSchema-instance%22%20xmlns%3Asos%3D%22http%3A%2F%2Fwww.opengis.net%2Fsos%2F1.0%22%3E%20%20%0A%3Csos%3AObservationId%3Ehttp%3A%2F%2Fwww.52north.org%2Ftest%2Fobservation%2F1%3C%2Fsos%3AObservationId%3E%0A%3Csos%3AresponseFormat%3Etext%2Fxml%3Bsubtype%3D%22om%2F1.0.0%22%3C%2Fsos%3AresponseFormat%3E%0A%3C%2FGetObservationById%3E%20%20
  # works in test client...
  expect_s4_class(obs, "OmObservationCollection")
  expect_length(obs, 1)

  skip("FIXME")
  #expect_s4_class(obs[[1]], "OmObservation")
  #expect_equal(sosResult(obs), 1.0)
})
