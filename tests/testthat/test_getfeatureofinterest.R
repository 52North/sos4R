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
# Created: 2019-04-26                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
context("GetFeatureOfInterest: encoding")

test_that("KVP encoding works (2.0.0)", {
  testsos <- SOS_Test(name = "getfoi", version = sos200_version, binding = "KVP")
  getfoi <- SosGetFeatureOfInterest_2.0.0(service = "service", version = testsos@version,
                                          featureOfInterest = "http://feature/1")
  request <- encodeRequestKVP(obj = getfoi, sos = testsos)
  expect_match(request, 'service=SOS')
  expect_match(request, 'version=2.0.0')
  expect_match(request, 'request=GetFeatureOfInterest')
  expect_match(request, 'featureOfInterest=http%3A%2F%2Ffeature%2F1')
})

test_that("KVP encoding works for multiple FOIs (2.0.0)", {
  testsos <- SOS_Test(name = "getfoi", version = sos200_version, binding = "KVP")
  getfoi <- SosGetFeatureOfInterest_2.0.0(service = "service", version = testsos@version,
                                          featureOfInterest = c("http://feature/1", "http://feature/2", "http://feature/3"))
  request <- encodeRequestKVP(obj = getfoi, sos = testsos)
  expect_match(request, 'featureOfInterest=http%3A%2F%2Ffeature%2F1%2Chttp%3A%2F%2Ffeature%2F2%2Chttp%3A%2F%2Ffeature%2F3')
})

test_that("POX encoding works (2.0.0)", {
  testsos <- SOS_Test(name = "getfoi", version = sos200_version, binding = "POX")
  getfoi <- SosGetFeatureOfInterest_2.0.0(service = "service", version = testsos@version,
                                featureOfInterest = "http://feature/1")
  request <- encodeRequestXML(obj = getfoi, sos = testsos)
  expect_match(toString(request), "<GetFeatureOfInterest")
  expect_match(toString(request), "<sos20:featureOfInterest>http://feature/1</sos20:featureOfInterest>")
})

test_that("POX encoding works with multiple features (2.0.0)", {
  testsos <- SOS_Test(name = "getfoi", version = sos200_version, binding = "POX")
  getfoi <- SosGetFeatureOfInterest_2.0.0(service = "service", version = testsos@version,
                                          featureOfInterest = c("http://feature/100", "http://feature/200", "http://feature/300"))
  request <- encodeRequestXML(obj = getfoi, sos = testsos)
  expect_match(toString(request), "<sos20:featureOfInterest>http://feature/100</sos20:featureOfInterest>")
  expect_match(toString(request), "<sos20:featureOfInterest>http://feature/200</sos20:featureOfInterest>")
  expect_match(toString(request), "<sos20:featureOfInterest>http://feature/300</sos20:featureOfInterest>")
})

context("GetFeatureOfInterest: feature member parsing")

# source: view-source:http://schemas.opengis.net/sos/2.0/examples/enhancedOperations/GetFOI2_response.xml
responseXml <- '<sos:GetFeatureOfInterestResponse xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:wsa="http://www.w3.org/2005/08/addressing" xmlns:swe="http://www.opengis.net/swe/2.0" xmlns:swes="http://www.opengis.net/swes/2.0" xmlns:ows="http://www.opengis.net/ows/1.1" xmlns:sos="http://www.opengis.net/sos/2.0" xmlns:fes="http://www.opengis.net/fes/2.0" xmlns:gml="http://www.opengis.net/gml/3.2" xmlns:ogc="http://www.opengis.net/ogc" xmlns:sam="http://www.opengis.net/sampling/2.0" xmlns:sams="http://www.opengis.net/samplingSpatial/2.0" xmlns:om="http://www.opengis.net/om/2.0" xmlns:xlink="http://www.w3.org/1999/xlink">
	<sos:featureMember>
		<sams:SF_SpatialSamplingFeature gml:id="st1">
			<gml:identifier codeSpace="">http://www.my_namespace.org/fois/st1</gml:identifier>
			<sam:type xlink:href="http://www.opengis.net/def/samplingFeatureType/OGC-OM/2.0/SF_SamplingPoint"/>
			<sam:sampledFeature xlink:href="http://wfs.example.org?request=getFeature&amp;featureid=Rhine_Sandbank_123"/>
			<sams:shape>
				<gml:Point gml:id="st1p">
					<gml:pos srsName="http://www.opengis.net/def/crs/EPSG/0/4326">50.7167 7.76667</gml:pos>
				</gml:Point>
			</sams:shape>
		</sams:SF_SpatialSamplingFeature>
	</sos:featureMember>
	<sos:featureMember>
		<sams:SF_SpatialSamplingFeature gml:id="st2">
			<gml:identifier codeSpace="">http://www.my_namespace.org/fois/st2</gml:identifier>
			<sam:type xlink:href="http://www.opengis.net/def/samplingFeatureType/OGC-OM/2.0/SF_SamplingPoint"/>
			<sam:sampledFeature xlink:href="http://wfs.example.org?request=getFeature&amp;featureid=Rhine_Sandbank_123"/>
			<sams:shape>
				<gml:Point gml:id="st2p">
					<gml:pos srsName="http://www.opengis.net/def/crs/EPSG/0/4326">51.7167 8.76667</gml:pos>
				</gml:Point>
			</sams:shape>
		</sams:SF_SpatialSamplingFeature>
	</sos:featureMember>
	<sos:featureMember>
	  <!-- exmple from OGC 10-126r4, 10.13 Requirements Class: Monitoring Point -->
		<wml2:MonitoringPoint gml:id="xsd-monitoring-point.example" xmlns:wml2="http://www.opengis.net/waterml/2.0">
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
    </wml2:MonitoringPoint>
	</sos:featureMember>
</sos:GetFeatureOfInterestResponse>'

test_that("All feature members identified and parsed", {
  testsos <- SOS_Test(name = "getfoi", version = sos200_version)

  response <- parseGetFeatureOfInterestResponse(obj = xml2::read_xml(x = responseXml), sos = testsos)

  featureClasses <- sapply(response, function(member) {
    return(class(member@feature))
  })
  expect_equal(featureClasses, c("SamsSamplingFeature", "SamsSamplingFeature", "WmlMonitoringPoint"))
})

context("GetFeatureOfInterest: integration tests\n")

test_that("KVP (2.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/kvp",
               binding = "KVP",
               useDCPs = FALSE,
               version = sos200_version)

  foi <- getFeatureOfInterest(sos = mySOS, featureOfInterest = "http://www.52north.org/test/featureOfInterest/1")

  expect_length(foi, 1)
  expect_s4_class(foi[[1]], "GmlFeatureProperty")
  expect_equal(sosName(foi[[1]]@feature), "con terra")
  expect_equal(foi[[1]]@feature@identifier, "http://www.52north.org/test/featureOfInterest/1")
})

test_that("POX (2.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/pox",
               binding = "POX",
               useDCPs = FALSE,
               version = sos200_version)

  foi <- getFeatureOfInterest(sos = mySOS, featureOfInterest = "http://www.52north.org/test/featureOfInterest/1")

  expect_length(foi, 1)
  expect_s4_class(foi[[1]], "GmlFeatureProperty")
  expect_equal(sosName(foi[[1]]@feature), "con terra")
  expect_equal(foi[[1]]@feature@identifier, "http://www.52north.org/test/featureOfInterest/1")
})

test_that("All features (no filter) with KVP works", {
  skip_on_cran()

  sos <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service",
             version = sos200_version, binding = "KVP")

  features <- getFeatureOfInterest(sos = sos, saveOriginal = TRUE)

  expect_length(features, 6)
  featureClasses <- sapply(features, function(feature) {
    return(class(feature))
  })
  expect_equal(featureClasses, rep("GmlFeatureProperty", 6))

  expect_equal(features[[5]]@href, "http://www.52north.org/test/featureOfInterest/world")
  expect_equal(features[[5]]@feature, NULL)

  names <- sapply(features, function(feature) {
    return(sosName(feature))
  })
  expect_equal(names[[1]], "ELV_WS_2500")
  expect_equal(names[[5]], "http://www.52north.org/test/featureOfInterest/world")
})


