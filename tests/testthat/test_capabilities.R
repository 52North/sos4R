################################################################################
# Copyright (C) 2015 by 52 North                                               #
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
# Created: 2015-01-27                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################
context("parsing: composite phenomenon")
if(!exists("parseXmlSnippet", mode="function")) {
  source("../util.R")
}


.compositePhenomenon <- '<swe:CompositePhenomenon xmlns:gml="http://www.opengis.net/gml" 
xmlns:swe="http://www.opengis.net/swe/1.0.1" xmlns:xlink="http://www.w3.org/1999/xlink"
gml:id="WaterQuality" dimension="4">
<gml:name>WaterQuality</gml:name>
<swe:component xlink:href="urn:ogc:def:property:OGC-SWE:1:ID"/>
<swe:component xlink:href="urn:ogc:def:property:OGC-SWE:2:ID"/>
</swe:CompositePhenomenon>'

test_that("composite phenomenon name is parsed from snippet", {
    .doc <- parseXmlSnippet(.compositePhenomenon)
    .phen <- parseCompositePhenomenon(obj = .doc)
    expect_that(.phen@name, equals("WaterQuality"))
})
test_that("composite phenomenon id is parsed from snippet", {
    .doc <- parseXmlSnippet(.compositePhenomenon)
    .phen <- parseCompositePhenomenon(obj = .doc)
    expect_that(.phen@id, equals("WaterQuality"))
})
test_that("composite phenomenon dimension is parsed from snippet", {
    .doc <- parseXmlSnippet(.compositePhenomenon)
    .phen <- parseCompositePhenomenon(obj = .doc)
    expect_that(.phen@dimension, equals(4))
})
test_that("composite phenomenon components are parsed from snippet", {
    .doc <- parseXmlSnippet(.compositePhenomenon)
    .phen <- parseCompositePhenomenon(obj = .doc)
    expect_that(length(.phen@components), equals(2))
    expect_that(.phen@components[[2]]@href, equals("urn:ogc:def:property:OGC-SWE:2:ID"))
})

context("capabilities: Mapserver")

.compositePhenOffering <- '<sos:ObservationOffering gml:id="Water" 
xmlns:sos="http://www.opengis.net/sos/1.0" xmlns:gml="http://www.opengis.net/gml" 
xmlns:swe="http://www.opengis.net/swe/1.0.1" xmlns:xlink="http://www.w3.org/1999/xlink">
    <sos:observedProperty>
        <swe:CompositePhenomenon gml:id="WaterQuality" dimension="4">
            <gml:name>Water Quality</gml:name>
            <swe:component xlink:href="urn:ogc:def:property:OGC-SWE:1:ID"/>
            <swe:component xlink:href="urn:ogc:def:property:OGC-SWE:1:STN_ID"/>
            <swe:component xlink:href="urn:ogc:def:property:OGC-SWE:1:DATETIME"/>
            <swe:component xlink:href="urn:ogc:def:property:OGC-SWE:1:VALUE"/>
        </swe:CompositePhenomenon>
    </sos:observedProperty>
    <sos:observedProperty>
        <swe:CompositePhenomenon gml:id="AirQuality" dimension="2">
            <gml:name>Air Quality</gml:name>
            <swe:component xlink:href="urn:ogc:def:property:OGC-SWE:1:ID"/>
            <swe:component xlink:href="urn:ogc:def:property:OGC-SWE:1:VALUE"/>
        </swe:CompositePhenomenon>
    </sos:observedProperty>
</sos:ObservationOffering>'

test_that("composite phenomenon are parsed correctly from observedProperty snippet", {
    .doc5 <-  parseXmlSnippet(.compositePhenOffering)
    obs_prop <- parseSosObservedProperty(.doc5[sosObservedPropertyName]) #, verbose = TRUE)
    obs_prop_off1 <- obs_prop[[1]]
    
    expect_equal(length(obs_prop), 2)
    expect_equal(obs_prop, list(observedProperty = "WaterQuality", observedProperty = "AirQuality"))
    #expect_equal(obs_prop[[1]], "urn:ogc:def:property:OGC-SWE:1:STN_ID")
})


# starting at package root: setwd(file.path(getwd(), "tests", "testthat"))
mapserver <- SOS_Test(name = "testcaps")
xmlCaps <- xmlParseDoc("../responses/Capabilities_Mapserver.xml")
parsedCaps <- parseSosCapabilities(obj = xmlCaps, sos = mapserver)
mapserver@capabilities <- parsedCaps

test_that("observed properties are parsed correctly from capabilities", {
    obs_prop <- sosObservedProperties(mapserver)
    obs_prop_off1 <- obs_prop[[1]]
    
    expect_equal(length(obs_prop), 1)
    expect_equal(obs_prop[[1]][[1]], "WaterQuality")
    # or should the components be listed?
    #expect_equal(obs_prop[[1]], "urn:ogc:def:property:OGC-SWE:1:STN_ID")
})

test_that("foi is parsed correctly", {
    foi <- sosFeaturesOfInterest(mapserver)
    expect_equal(foi[["Water"]][[1]], "urn:ogc:def:feature:OGC-SWE:3:transient")
    expect_equal(length(foi), 1)
    expect_equal(length(foi[["Water"]]), 1)
})

test_that("offering id is parsed correctly", {
    off <- sosOfferingIds(mapserver)
    expect_equal(off[[1]], "Water")
    expect_equal(length(off), 1)
})

# TODO add more tests:
#sosFeaturesOfInterest(mapserver)
#sosResultModels(mapserver)
#sosProcedures(mapserver)
#sosAbstract(mapserver)
#sosTitle(mapserver)
#sosGetCRS(mapserver)
#sosTime(mapserver)
#sosFilter_Capabilities(mapserver)
offs <- sosOfferings(mapserver)
#sosName(offs)
#sosBoundedBy(offs)
#sosTime(offs)
#sosProcedures(offs)
#sosProcedures(offs[[1]])


context("capabilities: Axiom")

.axiomOffering <- '<sos:ObservationOffering gml:id="urn_ioos_network_test_all"
xmlns:sos="http://www.opengis.net/sos/1.0" xmlns:xlink="http://www.w3.org/1999/xlink" 
xmlns:gml="http://www.opengis.net/gml">
    <gml:name>urn:ioos:network:test:all</gml:name>
    <gml:boundedBy>
    <gml:Envelope srsName="http://www.opengis.net/def/crs/EPSG/0/4326">
        <gml:lowerCorner>-171.37044143566763 -44.71596347896505</gml:lowerCorner>
        <gml:upperCorner>142.92375463033034 67.972129750194</gml:upperCorner>
        </gml:Envelope>
    </gml:boundedBy>
    <sos:time>
        <gml:TimePeriod xsi:type="gml:TimePeriodType">
            <gml:beginPosition>2015-01-12T23:00:00.000Z</gml:beginPosition>
            <gml:endPosition>2015-01-13T18:00:00.000Z</gml:endPosition>
        </gml:TimePeriod>
    </sos:time>
    <sos:procedure xlink:href="urn:ioos:network:test:all"/>
    <sos:observedProperty xlink:href="http://mmisw.org/ont/cf/parameter/air_temperature"/>
    <sos:observedProperty xlink:href="http://mmisw.org/ont/cf/parameter/sea_water_temperature"/>
    <sos:featureOfInterest xlink:href="urn:ioos:station:test:0"/>
    <sos:featureOfInterest xlink:href="urn:ioos:station:test:0(height-10.0m)"/>
    <sos:featureOfInterest xlink:href="urn:ioos:station:test:0(height-15.0m)"/>
    <sos:responseFormat>application/json</sos:responseFormat>
    <sos:responseFormat>application/x-netcdf</sos:responseFormat>
    <sos:responseFormat>application/zip; subtype=x-netcdf</sos:responseFormat>
    <sos:responseFormat>text/xml; subtype="om/1.0.0"</sos:responseFormat>
    <sos:responseFormat>text/xml; subtype="om/1.0.0/profiles/ioos_sos/1.0"</sos:responseFormat>
    <sos:responseMode>inline</sos:responseMode>
    <sos:responseMode>resultTemplate</sos:responseMode>
</sos:ObservationOffering>'

test_that("offering id is parsed correctly", {
    .doc3 <- parseXmlSnippet(.axiomOffering)
    .obsProp <- parseSosObservedProperty(.doc3[sosObservedPropertyName]) #, verbose = TRUE)
    expect_equal(.obsProp[[1]], "http://mmisw.org/ont/cf/parameter/air_temperature")
    expect_equal(length(.obsProp), 2)
})

context("parsing: SOS Capabilities 2.0.0")

testsos <- SOS_Test(name = "testcaps",version=sos200_version)
sos200Caps <- parseSosCapabilities(xmlParseDoc("../responses/Capabilities_200_Example.xml"), testsos)

context("parsing: SOS Capabilities 2.0.0 swes:offering")

testsos <- SOS_Test(name = "testcaps",version=sos200_version)

test_that("offering is parsed correctly", {
  .obs <- parseSosObservationOffering_200(xmlRoot(xmlParseDoc("../xml-elements/swes-offering1.xml")), testsos)
  expect_equal(.obs@id, "ws2500")
  #TODO test other parameters
})
 
testsos <- SOS_Test(name = "testcaps",version=sos100_version)
axiomCaps <- parseSosCapabilities(xmlParseDoc("../responses/Capabilities_100_Example.xml"), testsos)
