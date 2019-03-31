context("encode filter: time")

testsos <- SOS_Test(name = "testogc")

test_that("during", {
  during <- TM_During(time = GmlTimePeriod(beginPosition = GmlTimePosition(time = as.POSIXct("2019-01-01")),
                                 endPosition = GmlTimePosition(time = as.POSIXct("2019-01-01"))))
  encoded <- encodeXML(obj = during, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(encoded), ">\\s*<", "><")

  expect_match(encodedString, "<ogc:PropertyName>om:samplingTime</ogc:PropertyName>")
  expect_match(encodedString, "2019-01-01T00:00:00</gml:endPosition></gml:TimePeriod></ogc:TM_During>")
})

test_that("bbox", {
  box <- OgcBBOX(envelope = GmlEnvelope(lowerCorner = GmlDirectPositionLatLon(1, 20),
                                        upperCorner = GmlDirectPositionLatLon(300, 4000)))
  encoded <- encodeXML(obj = box, sos = testsos)
  encodedString <- stringr::str_replace_all(toString(encoded), ">\\s*<", "><")

  expect_match(encodedString, paste0(sosDefaultSpatialOpPropertyName, "</ogc:PropertyName>"))
  expect_match(encodedString, "300 4000</gml:upperCorner></gml:Envelope></ogc:BBOX>")
})

