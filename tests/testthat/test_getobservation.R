
context("GetObservation")

test_that("creation of request fails if eventTime contains objects of wrong class", {
  testsos <- SOS_Test(name = "testcaps")
  axiomCaps <- parseSosCapabilities(xmlParseDoc("../responses/Capabilities_52N-SOS_Axiom.xml"), testsos)
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
  eventTimeList[[2]] <- sessionInfo()
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

  request <- SosGetObservation(service = sosService, version = testsos100@version, offering = "off", observedProperty = list("prop"), responseFormat = "fmt",
                               eventTime = eventTimeList, valueReferenceTemporalFilter = "om:time")
  request100 <- encodeRequestKVP(request, testsos100)
  request20 <- encodeRequestKVP(request, testsos20)

  expect_match(request100, "&eventTime=2019-01-01")
  expect_match(request20, "&temporalFilter=om%3Atime%2C2019-01-01")
})
