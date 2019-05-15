context("parsing: OM 2.0 result")

resultXml <- '<om:result xmlns:om="http://www.opengis.net/om/2.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ns="http://www.opengis.net/gml/3.2" uom="test_unit_9_3" xsi:type="ns:MeasureType">50.28</om:result>'

test_that("result value parsing works", {
  testsos <- SOS_Test(name = "omresult")
  result <- parseResult(obj = xml2::read_xml(x = resultXml), sos = testsos)

  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), c(1,1))
  expect_equal(result[1,1], 50.28)
})

context("parsing: OM_Observation from sos:observation")

observationXml <- '<sos:observation xmlns:sos="http://www.opengis.net/sos/2.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:om="http://www.opengis.net/om/2.0" xmlns:gml="http://www.opengis.net/gml/3.2" xmlns:xlink="http://www.w3.org/1999/xlink">
    <om:OM_Observation gml:id="o_121">
      <gml:identifier codeSpace="http://www.opengis.net/def/nil/OGC/0/unknown">http://www.52north.org/test/observation/1</gml:identifier>
      <om:type xlink:href="http://www.opengis.net/def/observationType/OGC-OM/2.0/OM_Measurement"/>
      <om:phenomenonTime>
        <gml:TimeInstant gml:id="phenomenonTime_121">
          <gml:timePosition>2012-11-19T13:00:00.000Z</gml:timePosition>
        </gml:TimeInstant>
      </om:phenomenonTime>
      <om:resultTime xlink:href="#phenomenonTime_121"/>
      <om:procedure xlink:href="http://www.52north.org/test/procedure/1"/>
      <om:observedProperty xlink:href="http://www.52north.org/test/observableProperty/1"/>
      <om:featureOfInterest xlink:href="http://www.52north.org/test/featureOfInterest/1" xlink:title="con terra"/>
      <om:result xmlns:ns="http://www.opengis.net/gml/3.2" uom="test_unit_1" xsi:type="ns:MeasureType">1.23</om:result>
    </om:OM_Observation>
  </sos:observation>'

testsos <- SOS_Test(name = "omresult", version = sos200_version)

test_that("correct class is returned", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)
  expect_s4_class(observation, "OmOM_Observation")
})

test_that("time parsing works", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)

  times <- sosTime(observation)
  expect_equal(times$resultTime, times$phenomenonTime)
  expect_true(is.list(times))
  expect_s3_class(times[[1]], "POSIXct")
  expect_s3_class(times[[2]], "POSIXct")
  expect_named(times, c("resultTime", "phenomenonTime"))
})

test_that("coordinates are not available without FOI with a warning", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)

  expect_warning(coords <- sosCoordinates(observation),
                 "contains a feature")
  expect_true(is.na(coords))
})


test_that("coordinates are available with retrieved FOI", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/kvp",
               binding = "KVP",
               useDCPs = FALSE,
               version = sos200_version)
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = mySOS, featureCache = list())

  coords <- sosCoordinates(observation)
  expect_named(coords, c("lon", "lat", "SRS"))
  expect_equal(coords[["lon"]], 7.727958)
})

test_that("result parsed from observation", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)
  result <- sosResult(observation)

  expect_equal(dim(result), c(1, 1))
  expect_equal(result[1,1], 1.23)
  expect_named(result, c("test_unit_1"))
})

test_that("observation metadata is in result", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)
  result <- sosResult(observation)

  expect_named(result, c("test_unit_1"))
  skip("Result data.frame not unified yet.")
  expect_named(attributes(result[,1]), c("name", "definition", "unit of measurement"))
  expect_equal(sosUOM(result), c("test_unit_1"))
})

test_that("feature ID parsed from observation", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)
  fid <- sosFeatureIds(observation)

  expect_length(fid, 1)
  expect_equal(fid, "http://www.52north.org/test/featureOfInterest/1")
})

test_that("observed property parsed from observation", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)
  obsProps <- sosObservedProperties(observation)

  expect_length(obsProps, 1)
  expect_equal(obsProps, "http://www.52north.org/test/observableProperty/1")
})

