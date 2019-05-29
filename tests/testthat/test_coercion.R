############################################################################## #
# Copyright (C) 2019 by 52 North                                               #
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
# Created: 2019-04-30                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
context("Coercion: SML")

testsos <- SOS_Test(name = "testcoercion")

test_that("sensor description can be coerced to Spatial/SpatialPoints/SpatialPointsDataFrame", {
  sensorDescription <- xml2::read_xml(x = "../responses/SensorDescription_Bochum.xml")
  sml <- parseSensorML(obj = sensorDescription, sos = testsos)

  coerced <- as(object = sml, Class = "Spatial")
  expect_true(inherits(coerced, "Spatial"))
  expect_true(inherits(coerced, "SpatialPoints"))
  expect_true(inherits(coerced, "SpatialPointsDataFrame"))
})

test_that("OM observation collection can be coerced to SpatialPolygon (bbox)", {

})

test_that("OM measurement can be coerced to data.frame", {
})

test_that("OM observation can be coerced to data.frame", {
})

offeringXml <- '<sos:ObservationOffering gml:id="Luft" xmlns:sos="http://www.opengis.net/sos/1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ows="http://www.opengis.net/ows/1.1" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:ogc="http://www.opengis.net/ogc" xmlns:gml="http://www.opengis.net/gml">
        <gml:name>Luft</gml:name>
        <gml:boundedBy>
          <gml:Envelope srsName="urn:ogc:def:crs:EPSG::31466">
            <gml:lowerCorner>5659544.05 2574316.031</gml:lowerCorner>
            <gml:upperCorner>5681937.709 2609172.838</gml:upperCorner>
          </gml:Envelope>
        </gml:boundedBy>
        <sos:time>
          <gml:TimePeriod xsi:type="gml:TimePeriodType">
            <gml:beginPosition>2005-12-03T00:00:00.000+01:00</gml:beginPosition>
            <gml:endPosition>2013-09-17T04:40:00.000+02:00</gml:endPosition>
          </gml:TimePeriod>
        </sos:time>
        <sos:procedure xlink:href="Luftfeuchte_Barmen_Wupperverband"/>
        <sos:observedProperty xlink:href="Luftfeuchte"/>
        <sos:featureOfInterest xlink:href="Barmen_Wupperverband"/>
        <sos:responseFormat>text/xml;subtype="om/1.0.0"</sos:responseFormat>
        <sos:resultModel xmlns:ns="http://www.opengis.net/om/1.0">ns:Measurement</sos:resultModel>
        <sos:resultModel xmlns:ns="http://www.opengis.net/om/1.0">ns:Observation</sos:resultModel>
        <sos:responseMode>inline</sos:responseMode>
        <sos:responseMode>resultTemplate</sos:responseMode>
      </sos:ObservationOffering>'

test_that("OM observation offering can be coerced to Spatial/SpatialPolygons", {
  offering <- parseSosObservationOffering(obj = xml2::read_xml(offeringXml), sos = testsos)

  coerced <- as(offering, "Spatial")
  expect_equal(coerced, as(coerced, "SpatialPolygons"))
  expect_equal(bbox(coerced)["x", "min"], 2574316.031)
  expect_equal(bbox(coerced)["y", "max"], 5681937.709)
})

test_that("GML envelope can be coerced to Spatial/SpatialPolygons", {
  env <- GmlEnvelope(lowerCorner = GmlDirectPositionLatLon(lat = 2, lon = 3),
                     upperCorner = GmlDirectPositionLatLon(lat = 10, lon = 20),
                     srsName = "urn:ogc:def:crs:EPSG::4326")

  coerced <- as(env, "Spatial")
  expect_equal(coerced, as(env, "SpatialPolygons"))
  expect_s4_class(coerced, "SpatialPolygons")
  expect_equal(bbox(coerced), matrix(c(3, 2, 20, 10), nrow = 2, dimnames = list(c("x", "y"), c("min", "max"))))
  #mapview(coerced)
})

test_that("GML point can be coerced to Spatial/SpatialPoints", {
  point <- GmlPoint(pos = GmlDirectPositionLatLon(11, 22, srsName = "urn:ogc:def:crs:EPSG::4326"))
  coerced <- as(point, "Spatial")
  expect_equal(coerced, as(point, "SpatialPoints"))
  expect_s4_class(coerced, "SpatialPoints")
  expect_equal(coordinates(coerced), matrix(c(22, 11), nrow = 1, dimnames = list(NULL, c("lon", "lat"))))
})

test_that("GML point property can be coerced to Spatial/SpatialPoints if not an href", {
  pointProp <- GmlPointProperty(point = GmlPoint(pos = GmlDirectPosition(pos = "17 42", srsName = "urn:ogc:def:crs:EPSG::4326")))
  coerced <- as(pointProp, "Spatial")
  expect_equal(coerced, as(pointProp, "SpatialPoints"))
  expect_s4_class(coerced, "SpatialPoints")
  expect_equal(coordinates(coerced), matrix(c(42, 17), nrow = 1, dimnames = list(NULL, c("lon", "lat"))))
})

test_that("GML feature property can be coerced to Spatial/SpatialPoints if not an href (sa:SamplingPoint)", {
  pos <- GmlDirectPositionLatLon(lat = 1, lon = 2, srsName = "http://www.opengis.net/def/crs/EPSG/0/4326")
  prop <- GmlFeatureProperty(feature = SaSamplingPoint(sampledFeatures = list("feature1"),
                                                       position = GmlPointProperty(point = GmlPoint(pos = pos))))
  coerced <- as(prop, "Spatial")
  expect_equal(coerced, as(prop, "SpatialPoints"))
  expect_s4_class(coerced, "SpatialPoints")
  expect_equal(coordinates(coerced), matrix(c(2, 1), nrow = 1, dimnames = list(NULL, c("lon", "lat"))))
})

test_that("GML feature property coercion gives error if FeatureProperty or PointProperty have only reference (href)", {
  prop2 <- GmlFeatureProperty(href = "http://feature1.com")
  expect_error(as(prop2, "Spatial"), "cannot coerce reference property")

  prop1 <- GmlFeatureProperty(feature = SaSamplingPoint(sampledFeatures = list("feature1"),
                                                        position = GmlPointProperty(href = "#point1")))
  expect_error(as(prop1, "Spatial"), "cannot coerce reference property")
})

test_that("GML feature property can be coerced to Spatial/SpatialPoints if not an href (sams:SamplingFeature)", {
  pos <- GmlDirectPositionLatLon(lat = 90, lon = 180, srsName = "http://www.opengis.net/def/crs/EPSG/0/4326")
  prop <- GmlFeatureProperty(feature = SamsSamplingFeature(id = "feat1",
                                                           identifier = "feature 1",
                                                           name = "Feature",
                                                           type = "t",
                                                           sampledFeature = "sample",
                                                           shape = SamsShape(point = GmlPoint(pos = pos))))
  coerced <- as(prop, "Spatial")
  expect_equal(coerced, as(prop, "SpatialPoints"))
  expect_s4_class(coerced, "SpatialPoints")
  expect_equal(coordinates(coerced), matrix(c(180, 90), nrow = 1, dimnames = list(NULL, c("lon", "lat"))))
})

test_that("GML direct position can be coerced to Spatial/SpatialPoints", {
  pos <- GmlDirectPositionLatLon(lat = 10, lon = 20,
                                 srsName = "http://www.opengis.net/def/crs/EPSG/0/4326",
                                 srsDimension = as.integer(2),
                                 axisLabels = c("northing", "easting"))
  coerced <- as(pos, "Spatial")
  expect_equal(coerced, as(pos, "SpatialPoints"))
  expect_s4_class(coerced, "SpatialPoints")
  expect_equal(coordinates(coerced), matrix(c(20, 10), nrow = 1, dimnames = list(NULL, c("lon", "lat"))))
  #mapview(points)
})

test_that("there is an error if GML direct position has no SRS/CRS", {
  pos <- GmlDirectPositionLatLon(10, 20)
  expect_warning(expect_error(as(pos, "Spatial"), "cannot coerce to Spatial class"))
})

test_that("WML monitoring point can be coerced to Spatial/SpatialPolygons", {
  pos <- GmlDirectPositionLatLon(lat = 22, lon = 33, srsName = "http://www.opengis.net/def/crs/EPSG/0/4326")
  mp <- WmlMonitoringPoint(sampledFeatures = c("feature1", "feature2"),
                           id = "mp1",
                           identifier = "monitoring point one",
                           names = c("name"),
                           shape = SamsShape(point = GmlPoint(pos = pos)))
  coerced <- as(mp, "Spatial")
  expect_equal(coerced, as(mp, "SpatialPoints"))
  expect_s4_class(coerced, "SpatialPoints")
  expect_equal(coordinates(coerced), matrix(c(33, 22), nrow = 1, dimnames = list(NULL, c("lon", "lat"))))
})

test_that("SA sampling point can be coerced to Spatial/SpatialPolygons", {
  pos <- GmlDirectPositionLatLon(lat = 179, lon = 89, srsName = "http://www.opengis.net/def/crs/EPSG/0/4326")
  samplingPoint <- SaSamplingPoint(sampledFeatures = list("feature1"),
                                   position = GmlPointProperty(point = GmlPoint(pos = pos)))
  coerced <- as(samplingPoint, "Spatial")
  expect_equal(coerced, as(samplingPoint, "SpatialPoints"))
  expect_s4_class(coerced, "SpatialPoints")
  expect_equal(coordinates(coerced), matrix(c(89, 179), nrow = 1, dimnames = list(NULL, c("lon", "lat"))))
})
