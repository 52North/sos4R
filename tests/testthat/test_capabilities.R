context("capabilities: Mapserver")

parseXmlSnippet <- function(obj) {
  doc <- xml2::read_xml(x = obj, options = SosDefaultParsingOptions())
  docRoot <- xml2::xml_root(x = doc)
  return(docRoot)
}

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

test_that("composite phenomenon offering is parsed correctly from snippet", {
  doc5 <-  parseXmlSnippet(.compositePhenOffering)
  obs_prop <- parseSosObservedProperty(obj = xml2::xml_find_all(x = doc5, xpath = sosObservedPropertyName)) #, verbose = TRUE)

  expect_equal(length(obs_prop), 2)
  expect_equal(obs_prop, list("WaterQuality", "AirQuality"))
})

mapserver <- SOS_Test(name = "testcaps")
xmlCaps <- xml2::read_xml(x = "../responses/Capabilities_Mapserver.xml")
parsedCaps <- parseSosCapabilities(obj = xmlCaps, sos = mapserver)
mapserver@capabilities <- parsedCaps

test_that("observed properties are parsed correctly from capabilities", {
  obs_prop <- sosObservedProperties(mapserver)
  expect_equal(length(obs_prop), 1)
  expect_equal(obs_prop[[1]], "WaterQuality")
  # or should the components be listed?
  #expect_equal(obs_prop[[1]], "urn:ogc:def:property:OGC-SWE:1:STN_ID")
})

test_that("FOI", {
  foi <- sosFeaturesOfInterest(mapserver)
  expect_equal(foi[["Water"]][[1]], "urn:ogc:def:feature:OGC-SWE:3:transient")
  expect_equal(length(foi), 1)
  expect_equal(length(foi[["Water"]]), 1)
})

test_that("offering id", {
  off <- sosOfferingIds(mapserver)
  expect_equal(off[[1]], "Water")
  expect_equal(length(off), 1)
})

test_that("procedures", {
  #sosProcedures(mapserver)
  skip("Test not implemented yet.")
})

test_that("result models", {
  #sosResultModels(mapserver)
  skip("Test not implemented yet.")
})

test_that("abstract", {
  #sosAbstract(mapserver)
  skip("Test not implemented yet.")
})

test_that("title", {
  #sosTitle(mapserver)
  skip("Test not implemented yet.")
})

test_that("CRS", {
  #sosGetCRS(mapserver)
  skip("Test not implemented yet.")
})

test_that("time", {
  #sosTime(mapserver)
  skip("Test not implemented yet.")
})

test_that("offerings", {
  #offs <- sosOfferings(mapserver)
  skip("Test not implemented yet.")
})

test_that("name of offering", {
  #sosName(offs)
  skip("Test not implemented yet.")
})

test_that("bounds of offering", {
  #sosBoundedBy(offs)
  skip("Test not implemented yet.")
})

test_that("time of offering", {
  #sosTime(offs)
  skip("Test not implemented yet.")
})

test_that("procedures of offering", {
  #sosProcedures(offs)
  #sosProcedures(offs[[1]])
  skip("Test not implemented yet.")
})


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
  doc3 <- parseXmlSnippet(.axiomOffering)
  obsProp <- parseSosObservedProperty(doc3[sosObservedPropertyName]) #, verbose = TRUE)
  expect_equal(obsProp[[1]], "http://mmisw.org/ont/cf/parameter/air_temperature")
  expect_equal(length(obsProp), 2)
})

context("parsing: SOS Capabilities 2.0.0")

testsos <- SOS_Test(name = "testcaps",version = sos200_version, verboseOutput = TRUE)
sos200Caps <- parseSosCapabilities(xml2::read_xml(x = "../responses/Capabilities_200_Example.xml"), testsos)

context("parsing: SOS Capabilities 2.0.0 swes:offering")

testsos <- SOS_Test(name = "testcaps",version = sos200_version, verboseOutput = TRUE)

test_that("offering is parsed correctly", {
  obs <- parseSosObservationOffering_200(xml2::xml_root(x = xml2::read_xml(x = "../xml-elements/swes-offering1.xml")), testsos)
  expect_equal(obs@id, "ws2500")
  #TODO test other parameters
})

testsos <- SOS_Test(name = "testcaps",version = sos100_version, verboseOutput = TRUE)
axiomCaps <- parseSosCapabilities(xml2::read_xml(x = "../responses/Capabilities_100_Example.xml"), testsos)


context("parsing: operations metadata")

rangeXml <- '<ows:Range xmlns:ows="http://www.opengis.net/ows/1.1">
<ows:MinimumValue>2005-12-03T00:00:00.000+01:00</ows:MinimumValue>
<ows:MaximumValue>2015-12-13T00:00:00.000+01:00</ows:MaximumValue>
</ows:Range>'

test_that("composite phenomenon name is parsed from snippet", {
  doc <- parseXmlSnippet(rangeXml)
  range <- parseOwsRange(obj = doc)
  expect_equal(range@minimumValue, "2005-12-03T00:00:00.000+01:00")
  expect_equal(range@maximumValue, "2015-12-13T00:00:00.000+01:00")
})

operationXml <- '<ows:Operation name="GetCapabilities" xmlns:ows="http://www.opengis.net/ows/1.1" xmlns:xlink="http://www.w3.org/1999/xlink">
      <ows:DCP>
        <ows:HTTP>
          <ows:Get xlink:href="http://fluggs.wupperverband.de/sos/sos?"/>
          <ows:Post xlink:href="http://fluggs.wupperverband.de/sos/sos"/>
        </ows:HTTP>
      </ows:DCP>
      <ows:Parameter name="updateSequence">
        <ows:AnyValue/>
      </ows:Parameter>
      <ows:Parameter name="AcceptVersions">
        <ows:AllowedValues>
          <ows:Value>1.0.0</ows:Value>
        </ows:AllowedValues>
      </ows:Parameter>
      <ows:Parameter name="Sections">
        <ows:AllowedValues>
          <ows:Value>ServiceIdentification</ows:Value>
          <ows:Value>ServiceProvider</ows:Value>
          <ows:Value>OperationsMetadata</ows:Value>
          <ows:Value>Filter_Capabilities</ows:Value>
          <ows:Value>Contents</ows:Value>
          <ows:Value>All</ows:Value>
        </ows:AllowedValues>
      </ows:Parameter>
      <ows:Parameter name="AcceptFormats">
        <ows:AllowedValues>
          <ows:Value>text/xml</ows:Value>
          <ows:Value>application/zip</ows:Value>
        </ows:AllowedValues>
      </ows:Parameter>
    </ows:Operation>'

test_that("name from operation", {
  doc <- parseXmlSnippet(operationXml)
  operation <- parseOwsOperation(obj = doc)
  expect_equal(sosName(operation), "GetCapabilities")
})

test_that("DCPs are parsed from operation", {
  doc <- parseXmlSnippet(operationXml)
  operation <- parseOwsOperation(obj = doc)
  skip("Test not implemented yet.")
})

context("parsing: filter capabilities")

filterCapsXml <- '<sos:Filter_Capabilities xmlns:sos="http://www.opengis.net/sos/1.0" xmlns:ogc="http://www.opengis.net/ogc">
    <ogc:Spatial_Capabilities>
      <ogc:GeometryOperands>
      <ogc:GeometryOperand>gml:Envelope</ogc:GeometryOperand>
      <ogc:GeometryOperand>gml:Point</ogc:GeometryOperand>
      <ogc:GeometryOperand>gml:LineString</ogc:GeometryOperand>
      <ogc:GeometryOperand>gml:Polygon</ogc:GeometryOperand>
      </ogc:GeometryOperands>
      <ogc:SpatialOperators>
      <ogc:SpatialOperator name="BBOX">
      <ogc:GeometryOperands>
      <ogc:GeometryOperand>gml:Envelope</ogc:GeometryOperand>
      </ogc:GeometryOperands>
      </ogc:SpatialOperator>
      <ogc:SpatialOperator name="Contains">
      <ogc:GeometryOperands>
      <ogc:GeometryOperand>gml:Point</ogc:GeometryOperand>
      <ogc:GeometryOperand>gml:LineString</ogc:GeometryOperand>
      <ogc:GeometryOperand>gml:Polygon</ogc:GeometryOperand>
      </ogc:GeometryOperands>
      </ogc:SpatialOperator>
      <ogc:SpatialOperator name="Intersects">
      <ogc:GeometryOperands>
      <ogc:GeometryOperand>gml:Point</ogc:GeometryOperand>
      <ogc:GeometryOperand>gml:LineString</ogc:GeometryOperand>
      <ogc:GeometryOperand>gml:Polygon</ogc:GeometryOperand>
      </ogc:GeometryOperands>
      </ogc:SpatialOperator>
      <ogc:SpatialOperator name="Overlaps">
      <ogc:GeometryOperands>
      <ogc:GeometryOperand>gml:Point</ogc:GeometryOperand>
      <ogc:GeometryOperand>gml:LineString</ogc:GeometryOperand>
      <ogc:GeometryOperand>gml:Polygon</ogc:GeometryOperand>
      </ogc:GeometryOperands>
      </ogc:SpatialOperator>
      </ogc:SpatialOperators>
      </ogc:Spatial_Capabilities>
    <ogc:Temporal_Capabilities>
      <ogc:TemporalOperands>
      <ogc:TemporalOperand>gml:TimePeriod</ogc:TemporalOperand>
      <ogc:TemporalOperand>gml:TimeInstant</ogc:TemporalOperand>
      </ogc:TemporalOperands>
      <ogc:TemporalOperators>
      <ogc:TemporalOperator name="TM_During">
      <ogc:TemporalOperands>
      <ogc:TemporalOperand>gml:TimePeriod</ogc:TemporalOperand>
      </ogc:TemporalOperands>
      </ogc:TemporalOperator>
      <ogc:TemporalOperator name="TM_Equals">
      <ogc:TemporalOperands>
      <ogc:TemporalOperand>gml:TimeInstant</ogc:TemporalOperand>
      </ogc:TemporalOperands>
      </ogc:TemporalOperator>
      <ogc:TemporalOperator name="TM_After">
      <ogc:TemporalOperands>
      <ogc:TemporalOperand>gml:TimeInstant</ogc:TemporalOperand>
      </ogc:TemporalOperands>
      </ogc:TemporalOperator>
      <ogc:TemporalOperator name="TM_Before">
      <ogc:TemporalOperands>
      <ogc:TemporalOperand>gml:TimeInstant</ogc:TemporalOperand>
      </ogc:TemporalOperands>
      </ogc:TemporalOperator>
      </ogc:TemporalOperators>
      </ogc:Temporal_Capabilities>
    <ogc:Scalar_Capabilities>
      <ogc:ComparisonOperators>
      <ogc:ComparisonOperator>Between</ogc:ComparisonOperator>
      <ogc:ComparisonOperator>EqualTo</ogc:ComparisonOperator>
      <ogc:ComparisonOperator>NotEqualTo</ogc:ComparisonOperator>
      <ogc:ComparisonOperator>LessThan</ogc:ComparisonOperator>
      <ogc:ComparisonOperator>LessThanEqualTo</ogc:ComparisonOperator>
      <ogc:ComparisonOperator>GreaterThan</ogc:ComparisonOperator>
      <ogc:ComparisonOperator>GreaterThanEqualTo</ogc:ComparisonOperator>
      <ogc:ComparisonOperator>Like</ogc:ComparisonOperator>
      </ogc:ComparisonOperators>
    </ogc:Scalar_Capabilities>
    <ogc:Id_Capabilities>
      <ogc:FID/>
      <ogc:EID/>
    </ogc:Id_Capabilities>
</sos:Filter_Capabilities>'

testsos <- SOS_Test(name = "testfilter")

test_that("spatial capabilities", {
  doc <- parseXmlSnippet(filterCapsXml)
  filterCaps <- parseSosFilter_Capabilities(obj = doc, sos = testsos)
  expect_equal(length(filterCaps@spatial$`ogc:GeometryOperands`), 4)
  expect_equal(filterCaps@spatial$`ogc:GeometryOperands`[[3]], "gml:LineString")
  expect_equal(length(filterCaps@spatial$`ogc:SpatialOperators`), 4)
  expect_equal(filterCaps@spatial$`ogc:SpatialOperators`[[1]], "BBOX")
})

test_that("temporal capabilities", {
  doc <- parseXmlSnippet(filterCapsXml)
  filterCaps <- parseSosFilter_Capabilities(obj = doc, sos = testsos)
  expect_equal(length(filterCaps@temporal), 2)
  expect_named(filterCaps@temporal, c("ogc:TemporalOperands", "ogc:TemporalOperators"))
  expect_equal(length(filterCaps@temporal$`ogc:TemporalOperands`), 2)
  expect_equal(length(filterCaps@temporal$`ogc:TemporalOperators`), 4)
})

test_that("scalar capabilities", {
  doc <- parseXmlSnippet(filterCapsXml)
  filterCaps <- parseSosFilter_Capabilities(obj = doc, sos = testsos)
  expect_equal(length(filterCaps@scalar), 8)
  expect_equal(filterCaps@scalar[[2]], "EqualTo")
})

test_that("id capabilities", {
  doc <- parseXmlSnippet(filterCapsXml)
  filterCaps <- parseSosFilter_Capabilities(obj = doc, sos = testsos)
  expect_equal(filterCaps@id, list("ogc:FID", "ogc:EID"))
})


context("parsing: service identification")

serviceIdentXml <- '<ows:ServiceIdentification xmlns:ows="http://www.opengis.net/ows/1.1">
    <ows:Title>SOStitle</ows:Title>
    <ows:Abstract>SOSabstract</ows:Abstract>
    <ows:Keywords>
      <ows:Keyword>water level, gauge height, precipitation, water quality, air temperature, air pressure, runoff</ows:Keyword>
    </ows:Keywords>
    <ows:ServiceType codeSpace="http://opengeospatial.net">OGC:SOS</ows:ServiceType>
    <ows:ServiceTypeVersion>1.0.0</ows:ServiceTypeVersion>
    <ows:Fees>NONE</ows:Fees>
    <ows:AccessConstraints>http://open.data/sos.pdf</ows:AccessConstraints>
  </ows:ServiceIdentification>'

test_that("title and abstract", {
  doc <- parseXmlSnippet(serviceIdentXml)
  serviceIdent <- parseOwsServiceIdentification(obj = doc)
  expect_equal(serviceIdent@title, "SOStitle")
  expect_equal(serviceIdent@abstract, "SOSabstract")
})

test_that("keywords", {
  doc <- parseXmlSnippet(serviceIdentXml)
  serviceIdent <- parseOwsServiceIdentification(obj = doc)
  expect_equal(length(serviceIdent@keywords), 1)
  expect_match(serviceIdent@keywords, "air temperature, air pressure")
})

test_that("fees", {
  doc <- parseXmlSnippet(serviceIdentXml)
  serviceIdent <- parseOwsServiceIdentification(obj = doc)
  expect_equal(serviceIdent@fees, "NONE")
})

test_that("access constraints", {
  doc <- parseXmlSnippet(serviceIdentXml)
  serviceIdent <- parseOwsServiceIdentification(obj = doc)
  expect_match(serviceIdent@accessConstraints, "http://open.data/sos.pdf")
})

context("parsing: service provider")

serviceProviderXml <- '<ows:ServiceProvider xmlns:ows="http://www.opengis.net/ows/1.1" xmlns:xlink="http://www.w3.org/1999/xlink">
    <ows:ProviderName>the_name</ows:ProviderName>
    <ows:ProviderSite xlink:href="http://www.de"/>
    <ows:ServiceContact>
      <ows:IndividualName>TBA</ows:IndividualName>
      <ows:PositionName>TBA</ows:PositionName>
      <ows:ContactInfo>
        <ows:Phone>
          <ows:Voice>123456</ows:Voice>
        </ows:Phone>
        <ows:Address>
          <ows:DeliveryPoint>Str. 100</ows:DeliveryPoint>
          <ows:City>Münster</ows:City>
          <ows:AdministrativeArea>North Rhine-Westphalia</ows:AdministrativeArea>
          <ows:PostalCode>48149</ows:PostalCode>
          <ows:Country>Germany</ows:Country>
          <ows:ElectronicMailAddress>mail@mail.mail</ows:ElectronicMailAddress>
        </ows:Address>
      </ows:ContactInfo>
      <ows:Role/>
    </ows:ServiceContact>
  </ows:ServiceProvider>'

test_that("name and site", {
  doc <- parseXmlSnippet(serviceProviderXml)
  serviceProv <- parseOwsServiceProvider(obj = doc)
  expect_match(serviceProv@providerName, "the_name")
  expect_match(serviceProv@providerSite, "http://www.de")
  expect_s3_class(serviceProv@serviceContact, "xml_node")
})
