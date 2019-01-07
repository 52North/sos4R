
context("GetObservation")

testsos <- SOS_Test(name = "testcaps")
axiomCaps <- parseSosCapabilities(xmlParseDoc("../responses/Capabilities_52N-SOS_Axiom.xml"), testsos)
testsos@capabilities <- axiomCaps

test_that("creation of request fails if eventTime contains objects of wrong class", {
  eventTimeList <- sosCreateEventTimeList(sosCreateTimePeriod(sos = testsos,
                                                              begin = as.POSIXct(Sys.time() - 3600 * 24),
                                                              end = as.POSIXct(Sys.time())))
  eventTimeList[[2]] <- eventTimeList[[1]]
  eventTimeList[[3]] <- eventTimeList[[1]]
  getobs <- SosGetObservation_2.0.0(service = "test", version = "test", offering = "test", observedProperty = list("test"),
                          responseFormat = "test",
                          eventTime = eventTimeList)
  expect_equal(getobs@eventTime, eventTimeList)
  eventTimeList[[2]] <- sessionInfo()
  expect_error(SosGetObservation_2.0.0(service = "test", version = "test", offering = "test", observedProperty = list("test"),
                                       responseFormat = "test",
                                       eventTime = eventTimeList), "must extend SosEventTime")
})
