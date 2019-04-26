context("GetFeatureOfInterest: encoding")

test_that("KVP encoding works (2.0.0)", {
  testsos <- SOS_Test(name = "getfoi", version = sos200_version, binding = "KVP")
  getfoi <- SosGetFeatureOfInterest_2.0.0(service = "service", version = testsos@version,
                                          featureOfInterest = "http://feature/1")
  request <- encodeRequestKVP(obj = getfoi, sos = testsos)
  expect_match(request, 'service=SOS')
  expect_match(request, 'version=2.0.0')
  expect_match(request, 'request=GetFeatureOfInterest')
  expect_match(request, 'featureOfInterest=http%3A%2F%2Ffeature%2F1')
})

test_that("KVP encoding works for multiple FOIs (2.0.0)", {
  testsos <- SOS_Test(name = "getfoi", version = sos200_version, binding = "KVP")
  getfoi <- SosGetFeatureOfInterest_2.0.0(service = "service", version = testsos@version,
                                          featureOfInterest = c("http://feature/1", "http://feature/2", "http://feature/3"))
  request <- encodeRequestKVP(obj = getfoi, sos = testsos)
  expect_match(request, 'featureOfInterest=http%3A%2F%2Ffeature%2F1%2Chttp%3A%2F%2Ffeature%2F2%2Chttp%3A%2F%2Ffeature%2F3')
})

test_that("POX encoding works (2.0.0)", {
  testsos <- SOS_Test(name = "getfoi", version = sos200_version, binding = "POX")
  getfoi <- SosGetFeatureOfInterest_2.0.0(service = "service", version = testsos@version,
                                featureOfInterest = "http://feature/1")
  request <- encodeRequestXML(obj = getfoi, sos = testsos)
  expect_match(toString(request), "<GetFeatureOfInterest")
  expect_match(toString(request), "<sos20:featureOfInterest>http://feature/1</sos20:featureOfInterest>")
})

test_that("POX encoding works with multiple features (2.0.0)", {
  testsos <- SOS_Test(name = "getfoi", version = sos200_version, binding = "POX")
  getfoi <- SosGetFeatureOfInterest_2.0.0(service = "service", version = testsos@version,
                                          featureOfInterest = c("http://feature/100", "http://feature/200", "http://feature/300"))
  request <- encodeRequestXML(obj = getfoi, sos = testsos)
  expect_match(toString(request), "<sos20:featureOfInterest>http://feature/100</sos20:featureOfInterest>")
  expect_match(toString(request), "<sos20:featureOfInterest>http://feature/200</sos20:featureOfInterest>")
  expect_match(toString(request), "<sos20:featureOfInterest>http://feature/300</sos20:featureOfInterest>")
})

context("GetFeatureOfInterest: integration test\n")

test_that("KVP (2.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/kvp",
               binding = "KVP",
               useDCPs = FALSE,
               version = sos200_version)

  foi <- getFeatureOfInterest(sos = mySOS, featureOfInterest = "http://www.52north.org/test/featureOfInterest/1")

  expect_length(foi, 1)
  expect_s4_class(foi[[1]], "GmlFeatureProperty")
  expect_equal(sosName(foi[[1]]@feature), "con terra")
  expect_equal(foi[[1]]@feature@identifier, "http://www.52north.org/test/featureOfInterest/1")
})

test_that("POX (2.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/pox",
               binding = "POX",
               useDCPs = FALSE,
               version = sos200_version)

  foi <- getFeatureOfInterest(sos = mySOS, featureOfInterest = "http://www.52north.org/test/featureOfInterest/1")

  expect_length(foi, 1)
  expect_s4_class(foi[[1]], "GmlFeatureProperty")
  expect_equal(sosName(foi[[1]]@feature), "con terra")
  expect_equal(foi[[1]]@feature@identifier, "http://www.52north.org/test/featureOfInterest/1")
})

