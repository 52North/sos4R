context("encoding GML: spatial")

testsos <- SOS_Test(name = "testgml")

test_that("point minimal", {
  point <- GmlPoint(pos = GmlDirectPositionLatLon(10, 20))
  encoded <- encodeXML(obj = point, sos = testsos)
  encodedString <- stringr::str_remove_all(toString(encoded), "\n")
  expect_match(encodedString, "10 20</gml:pos></gml:Point>")
})

test_that("position minimal", {
  position <- GmlDirectPosition(pos = "1 2")
  encoded <- encodeXML(obj = position, sos = testsos)
  expect_match(toString(encoded), "<gml:pos")
  expect_match(toString(encoded), "1 2</gml:pos>")
})

test_that("position full", {
  position <- GmlDirectPosition(pos = "1 2", srsName = "the_srs", srsDimension = as.integer(3),
                                axisLabels = c("one two"), uomLabels = c("m deg"))
  encoded <- encodeXML(obj = position, sos = testsos)
  encodedString <- stringr::str_remove_all(toString(encoded), "\n")
  expect_match(encodedString, 'srsName="the_srs"')
  expect_match(encodedString, 'srsDimension="3"')
  expect_match(encodedString, 'axisLabels="one two"')
  expect_match(encodedString, 'uomLabels="m deg"')
})

test_that("envelope", {
  env <- GmlEnvelope(lowerCorner = GmlDirectPositionLatLon(1, 2),
                     upperCorner = GmlDirectPositionLatLon(8, 9))
  encoded <- encodeXML(obj = env, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(encoded), ">\\s*<", "><")

  expect_match(encodedString, '<gml:Envelope')
  expect_match(encodedString, '1 2</gml:lowerCorner>')
  expect_match(encodedString, '8 9</gml:upperCorner>')
})

context("encoding GML: temporal")

testsos <- SOS_Test(name = "testgml")

test_that("time position minimal", {
  tpos <- GmlTimePosition(time = as.POSIXct("2019-01-01"))

  encoded <- encodeXML(obj = tpos, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(encoded), ">\\s*<", "><")

  expect_match(encodedString, '2019-01-01T00:00:00</gml:timePosition>')
})

test_that("time position all", {
  tpos <- GmlTimePosition(time = as.POSIXct("2019-01-01"),
                          frame = "the_frame",
                          calendarEraName = "the_era",
                          indeterminatePosition = "yes")

  encoded <- encodeXML(obj = tpos, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(encoded), ">\\s*<", "><")

  expect_match(encodedString, 'frame="the_frame"')
  expect_match(encodedString, 'calendarEraName="the_era"')
  expect_match(encodedString, 'indeterminatePosition="yes"')
  expect_match(encodedString, '2019-01-01T00:00:00</gml:timePosition>')
})

test_that("time instant", {
  instant <- GmlTimeInstant(timePosition = GmlTimePosition(time = as.POSIXct("2019-01-01")))
  encoded <- encodeXML(obj = instant, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(encoded), ">\\s*<", "><")

  expect_match(encodedString, '<gml:TimeInstant')
  expect_match(encodedString, '2019-01-01T00:00:00</gml:timePosition></gml:TimeInstant>')
})

test_that("time period", {
  period <- sosCreateTimePeriod(sos = testsos, begin = as.POSIXct("2019-01-01"), end = as.POSIXct("2019-02-03"))
  encoded <- encodeXML(obj = period, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(encoded), ">\\s*<", "><")

  expect_match(encodedString, '2019-01-01T00:00:00</gml:beginPosition>')
  expect_match(encodedString, '2019-02-03T00:00:00</gml:endPosition></gml:TimePeriod>')
})

test_that("time interval", {
  interval <- sosCreateTimePeriod(sos = testsos, begin = as.POSIXct("2019-01-01"), end = as.POSIXct("2019-02-03"),
                                  timeInterval = GmlTimeInterval(interval = "everyother", unit = "hr", radix = as.integer(17), factor = as.integer(2)))
  encoded <- encodeXML(obj = interval, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(encoded), ">\\s*<", "><")

  expect_match(encodedString, '<gml:timeInterval unit="hr" radix="17" factor="2">everyother</gml:timeInterval>')
})
