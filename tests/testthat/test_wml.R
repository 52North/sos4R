############################################################################## #
# Copyright (C) 2019 by 52°North                                               #
# Initiative for Geospatial Open Source Software GmbH                          #
#                                                                              #
# Contact: Andreas Wytzisk                                                     #
# 52 North Initiative for Geospatial Open Source Software GmbH                 #
# Martin-Luther-King-Weg 24                                                    #
# 48155 Muenster, Germany                                                      #
# info@52north.org                                                             #
#                                                                              #
# This program is free software; you can redistribute and/or modify it under   #
# the terms of the GNU General Public License version 2 as published by the    #
# Free Software Foundation.                                                    #
#                                                                              #
# This program is distributed WITHOUT ANY WARRANTY; even without the implied   #
# WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU #
# General Public License for more details.                                     #
#                                                                              #
# You should have received a copy of the GNU General Public License along with #
# this program (see gpl-2.0.txt). If not, write to the Free Software           #
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or #
# visit the Free Software Foundation web page, http://www.fsf.org.             #
#                                                                              #
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)                          #
# Created: 2019-04-29                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
context("parsing: WaterML MonitoringPoint")

testsos <- SOS_Test(name = "testwml", version = sos200_version)
#
# example from OGC 10-126r4, 10.13 Requirements Class: Monitoring Point
#
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
#
# test that reference frame is parsed correctly ----
#
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
              <wml2:temporalExtent>
                <gml:TimePeriod xmlns:gml="http://www.opengis.net/gml/3.2" gml:id="phenomenonTime_1">
                  <gml:beginPosition>2019-01</gml:beginPosition>
                  <gml:endPosition>2019-06</gml:endPosition>
                </gml:TimePeriod>
              </wml2:temporalExtent>
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
              <wml2:value>4.2</wml2:value>
            </wml2:MeasurementTVP>
          </wml2:point>
          <wml2:point>
            <wml2:MeasurementTVP>
              <wml2:time>2019-05-17T07:00:00.000+12:00</wml2:time>
              <wml2:value>5.4</wml2:value>
            </wml2:MeasurementTVP>
          </wml2:point>
        </wml2:MeasurementTimeseries>'
#
# test that measurement timeseries is parsed correctly ----
#
test_that("measurement timeseries is parsed correctly", {
  mt <- parseWmlMeasurementTimeseries(obj = xml2::read_xml(x = mtXml), sos = testsos)

  expect_s4_class(mt, "WmlMeasurementTimeseries")
  expect_equal(mt@id, "timeseries.1")
  expect_s4_class(mt@metadata, "WmlMeasurementTimeseriesMetadata")
  expect_s4_class(mt@metadata@temporalExtent, "GmlTimePeriod")
  expect_true(mt@metadata@temporalExtent@beginPosition@time == parsedate::parse_iso_8601("2019-01"))
  expect_true(mt@metadata@temporalExtent@endPosition@time == parsedate::parse_iso_8601("2019-06"))
  expect_s4_class(mt@defaultPointMetadata, "WmlDefaultTVPMeasurementMetadata")
  expect_s4_class(mt@defaultPointMetadata@interpolationType, "WmlInterpolationType")
  expect_equal(mt@defaultPointMetadata@uom, "m^3/s")
  expect_equal(mt@defaultPointMetadata@interpolationType@href, "http://www.opengis.net/def/timeseriesType/WaterML/2.0/continuous")
  expect_equal(mt@defaultPointMetadata@interpolationType@title, "Instantaneous")
  expect_length(mt@points, 2)
  expect_s4_class(mt@points[[1]], "WmlMeasurementTVP")
  expect_true(mt@points[[1]]@time == parsedate::parse_iso_8601("2019-05-17T06:00:00.000+12:00"))
  expect_true(mt@points[[1]]@value == 4.2)
  expect_s4_class(mt@points[[2]], "WmlMeasurementTVP")
  expect_true(mt@points[[2]]@time == parsedate::parse_iso_8601("2019-05-17T07:00:00.000+12:00"))
  expect_true(mt@points[[2]]@value == 5.4)
})

test_that("result data can be retrieved from measurement timeseries", {
  mt <- parseWmlMeasurementTimeseries(obj = xml2::read_xml(x = mtXml), sos = testsos)
  result <- sosResult(mt)
  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), c(2, 2))
  expect_true("uom" %in% names(attributes(result)))
})
