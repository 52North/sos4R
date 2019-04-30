context("parsing: WaterML MonitoringPoint")

testsos <- SOS_Test(name = "testwml", version = sos200_version)

# example from OGC 10-126r4, 10.13 Requirements Class: Monitoring Point
mpXml <- '<wml2:MonitoringPoint gml:id="xsd-monitoring-point.example" xmlns:wml2="http://www.opengis.net/waterml/2.0" xmlns:sam="http://www.opengis.net/sampling/2.0" xmlns:sams="http://www.opengis.net/samplingSpatial/2.0" xmlns:gml="http://www.opengis.net/gml/3.2" xmlns:xlink="http://www.w3.org/1999/xlink">
      <gml:description>Nile river at Deddington, South Esk catchment, Tasmania</gml:description>
      <gml:name codeSpace="http://www.csiro.au/">Deddington</gml:name>
      <sam:sampledFeature xlink:href="http://csiro.au/features/rivers/nile" xlink:title="Nile River" />
      <sams:shape>
        <gml:Point gml:id="location_deddington">
          <gml:pos srsName="urn:ogc:def:crs:EPSG::4326">-41.814935 147.568517 </gml:pos>
        </gml:Point>
      </sams:shape>
      <wml2:verticalDatum xlink:href="urn:ogc:def:crs:EPSG::5711" xlink:title="Australian height datum" />
      <wml2:timeZone>
        <wml2:TimeZone>
          <wml2:zoneOffset>+11:00</wml2:zoneOffset>
          <wml2:zoneAbbreviation>AEDT</wml2:zoneAbbreviation>
        </wml2:TimeZone>
      </wml2:timeZone>
    </wml2:MonitoringPoint>'

test_that("reference frame", {
  mp <- parseMonitoringPoint(obj = xml2::read_xml(x = mpXml), sos = testsos)

  expect_equal(sosId(mp), "xsd-monitoring-point.example")
  expect_equal(sosName(mp), c("Deddington"))
  expect_s3_class(mp@verticalDatums, "xml_nodeset")
  expect_length(mp@verticalDatums, 1)
  expect_match(toString(mp@verticalDatums), "Australian height")
  expect_s3_class(mp@timeZone, "xml_node")
  expect_match(toString(mp@timeZone), "AEDT")
  expect_equal(mp@sampledFeatures, c("Nile River" = "http://csiro.au/features/rivers/nile"))
})
