context("parsing: sampling point")

samplingPointXml <- '<sa:SamplingPoint gml:id="sf_8D0EA55680FEC1646E1A01C441D7220F9BD9F57C" xmlns:gml="http://www.opengis.net/gml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:sa="http://www.opengis.net/sampling/1.0">
<gml:name codeSpace="uniquID">http://www.52north.org/test/featureOfInterest/6</gml:name>
<gml:name codeSpace="http://www.opengis.net/def/nil/OGC/0/unknown">Hochschule Bochum</gml:name>
<sa:sampledFeature xlink:href="http://www.52north.org/test/featureOfInterest/world" xlink:title="Hochschule Bochum"/>
<sa:position>
<gml:Point gml:id="point_sf_8D0EA55680FEC1646E1A01C441D7220F9BD9F57C">
<gml:pos srsName="urn:ogc:def:crs:EPSG::4326">51.447722 7.270806</gml:pos>
</gml:Point>
</sa:position>
</sa:SamplingPoint>'

testsos <- SOS_Test(name = "testsa")

test_that("point is parsed", {
  doc <- xml2::read_xml(x = samplingPointXml)
  point <- parseSamplingPoint(obj = doc, sos = testsos)

  coords <- sosCoordinates(point)
  expect_equal(coords$feature, "sf_8D0EA55680FEC1646E1A01C441D7220F9BD9F57C")
  expect_equal(coords$lat, 51.447722)
  expect_equal(coords$lon, 7.270806)
  expect_equal(as.character(coords$SRS), "urn:ogc:def:crs:EPSG::4326")
})
