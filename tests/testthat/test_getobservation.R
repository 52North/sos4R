
context("GetObservation")

test_that("creation of request fails if eventTime contains objects of wrong class", {
  testsos <- SOS_Test(name = "testcaps")
  axiomCaps <- parseSosCapabilities(xml2::read_xml(x = "../responses/Capabilities_52N-SOS_Axiom.xml"), testsos)
  testsos@capabilities <- axiomCaps

  eventTimeList <- sosCreateEventTimeList(sosCreateTimePeriod(sos = testsos,
                                                              begin = as.POSIXct(Sys.time() - 3600 * 24),
                                                              end = as.POSIXct(Sys.time())))
  eventTimeList[[2]] <- eventTimeList[[1]]
  eventTimeList[[3]] <- eventTimeList[[1]]
  getobs <- SosGetObservation(service = "test", version = "test", offering = "test", observedProperty = list("test"),
                          responseFormat = "test",
                          eventTime = eventTimeList)
  expect_equal(getobs@eventTime, eventTimeList)
  eventTimeList[[2]] <- list("not right")
  expect_error(SosGetObservation(service = "test", version = "test", offering = "test", observedProperty = list("test"),
                                       responseFormat = "test",
                                       eventTime = eventTimeList), "must extend SosEventTime")
})

test_that("creation temporal filter has a valueReference in SOS 2.0, but not for SOS 1.0.0", {
  testsos100 <- SOS_Test(name = "onezerozero", version = sos100_version)
  testsos20 <- SOS_Test(name = "twozero", version = sos200_version)
  eventTimeList <- sosCreateEventTimeList(sosCreateTimePeriod(sos = testsos100,
                                                              begin = as.POSIXct("2019-01-01"),
                                                              end = as.POSIXct("2019-01-02")))

  request <- SosGetObservation(service = sosService, version = testsos100@version,
                               offering = "off", observedProperty = list("prop"), responseFormat = "fmt",
                               eventTime = eventTimeList, valueReferenceTemporalFilter = "om:time")
  request100 <- encodeRequestKVP(request, testsos100)
  request20 <- encodeRequestKVP(request, testsos20)

  expect_match(request100, "&eventTime=2019-01-01")
  expect_match(request20, "&temporalFilter=om%3Atime%2C2019-01-01")
})

context("GetObservation: POX encoding")

test_that("minimal", {
  testsos <- SOS_Test(name = "getobspox")
  getobs <- SosGetObservation(service = "ser",
                              version = "ver",
                              offering = "off",
                              observedProperty = list("p1", "p2"),
                              responseFormat = "fmt")
  request <- encodeRequestXML(obj = getobs, sos = testsos)
  expect_match(toString(request), 'service="ser"')
  expect_match(toString(request), ' version="1.0.0"')
  expect_match(toString(request), "<sos:offering>off</sos:offering>")
  expect_match(toString(request), "<sos:observedProperty>p1</sos:observedProperty>")
  expect_match(toString(request), "<sos:observedProperty>p2</sos:observedProperty>")
  expect_match(toString(request), "<sos:responseFormat>fmt</sos:responseFormat>")
  expect_equal(stringr::str_count(toString(request), "time"), 0)
})

test_that("with event time", {
  testsos <- SOS_Test(name = "getobspox")
  getobs <- SosGetObservation(service = "ser",
                              version = "ver",
                              offering = "off",
                              observedProperty = list("p1", "p2"),
                              responseFormat = "fmt",
                              eventTime = sosCreateTime(sos = testsos,
                                                        time = "2017-12-19::2017-12-20"))
  request <- encodeRequestXML(obj = getobs, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(request), ">\\s*<", "><")
  expect_match(encodedString, 'service="ser"')
  expect_match(encodedString, ' version="1.0.0"')
  expect_match(encodedString, "<sos:offering>off</sos:offering>")
  expect_match(encodedString, "<sos:observedProperty>p1</sos:observedProperty>")
  expect_match(encodedString, "<sos:observedProperty>p2</sos:observedProperty>")
  expect_match(encodedString, "<sos:responseFormat>fmt</sos:responseFormat>")
  expect_match(encodedString, "<ogc:TM_During")
  expect_match(encodedString, "<ogc:PropertyName>om:samplingTime</ogc:PropertyName>")
  expect_match(encodedString, "2017-12-20T00:00:00</gml:endPosition>")
  expect_match(encodedString, "</ogc:TM_During></sos:eventTime>")
})

context("GetObservation: integration tests\n")

test_that("KVP", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service", binding = "KVP")
  off.1 <- sosOfferings(mySOS)[[1]]
  obs.1 <- getObservation(sos = mySOS,
                          offering = off.1,
                          procedure = sosProcedures(off.1)[[1]],
                          observedProperty = sosObservedProperties(off.1)[1],
                          eventTime = sosCreateTime(sos = mySOS,
                                                    time = "2017-12-19::2017-12-20")
                          #,verbose = TRUE
  )
  expect_s4_class(obs.1, "OmObservationCollection")
  expect_length(obs.1, 1)
  expect_s4_class(obs.1[[1]], "OmObservation")
  expect_equal(dim(sosResult(obs.1)), c(72, 2))
})

test_that("POX", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/pox",
               binding = "POX",
               useDCPs = FALSE)
  off.1 <- sosOfferings(mySOS)[[1]]
  obs.1 <- getObservation(sos = mySOS,
                          offering = off.1,
                          procedure = sosProcedures(off.1)[[1]],
                          observedProperty = sosObservedProperties(off.1)[1],
                          eventTime = sosCreateTime(sos = mySOS,
                                                    time = "2017-12-19::2017-12-20")
                          #,verbose = TRUE,
                          #inspect = TRUE
  )
  expect_s4_class(obs.1, "OmObservationCollection")
  expect_length(obs.1, 1)
  expect_s4_class(obs.1[[1]], "OmObservation")
  result <- sosResult(obs.1)
  expect_equal(dim(result), c(71, 2))
  expect_named(result, c("phenomenonTime", "AirTemperature"))
})

test_that("CSV parsing works", {
  skip_on_cran()

  ioos <- SOS(url = "https://sdf.ndbc.noaa.gov/sos/server.php",
              timeFormat = "%Y-%m-%dT%H:%M:%SZ")
  ioos.off <- sosOfferings(ioos)
  obs <- getObservation(ioos, offering = sosName(ioos.off[[100]]),
                             responseFormat = "text/csv",
                             observedProperty = sosObservedProperties(ioos.off[[100]])[1])
  obs
  expect_s3_class(obs, "data.frame")
  expect_equal(dim(obs), c(1, 7))
})
