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

test_that("reference frame is parsed correctly", {
  mp <- parseWmlMonitoringPoint(obj = xml2::read_xml(x = mpXml), sos = testsos)

  expect_equal(sosId(mp), "xsd-monitoring-point.example")
  expect_equal(sosName(mp), c("Deddington"))
  expect_s3_class(mp@verticalDatums, "xml_nodeset")
  expect_length(mp@verticalDatums, 1)
  expect_match(toString(mp@verticalDatums), "Australian height")
  expect_s3_class(mp@timeZone, "xml_node")
  expect_match(toString(mp@timeZone), "AEDT")
  expect_equal(mp@sampledFeatures, c("Nile River" = "http://csiro.au/features/rivers/nile"))
})

mtXml <- '<wml2:MeasurementTimeseries gml:id="timeseries.1" xmlns:sos="http://www.opengis.net/sos/2.0" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:om="http://www.opengis.net/om/2.0" xmlns:gml="http://www.opengis.net/gml/3.2" xmlns:wml2="http://www.opengis.net/waterml/2.0">
          <wml2:metadata>
            <wml2:MeasurementTimeseriesMetadata>
              <wml2:temporalExtent xlink:href="#phenomenonTime_1"/>
            </wml2:MeasurementTimeseriesMetadata>
          </wml2:metadata>
          <wml2:defaultPointMetadata>
            <wml2:DefaultTVPMeasurementMetadata>
              <wml2:uom code="m^3/s"/>
              <wml2:interpolationType xlink:href="http://www.opengis.net/def/timeseriesType/WaterML/2.0/continuous" xlink:title="Instantaneous"/>
            </wml2:DefaultTVPMeasurementMetadata>
          </wml2:defaultPointMetadata>
          <wml2:point>
            <wml2:MeasurementTVP>
              <wml2:time>2019-05-17T06:00:00.000+12:00</wml2:time>
              <wml2:value>4.26702452619947</wml2:value>
            </wml2:MeasurementTVP>
          </wml2:point>
        </wml2:MeasurementTimeseries>'

test_that("[TODO] measurement timeseries is parsed correctly", {
  mt <- parseMeasurementTimeseries(obj = xml2::read_xml(x = mtXml), sos = testsos)

  # TODO implement parsing
  expect_s3_class(mt, "xml_document")
})
