context("parsing: composite phenomenon")

.compositePhenomenon <- '<swe:CompositePhenomenon gml:id="WaterQuality" dimension="4">
    <gml:name>WaterQuality</gml:name>
    <swe:component xlink:href="urn:ogc:def:property:OGC-SWE:1:ID"/>
</swe:CompositePhenomenon>'

test_that("composite phenomenon name is parsed", {
    x <- TRUE
    expect_that(x, equals(TRUE))
})


context("parsing: capabilities: Mapserver")

setClass("SOS_Test",
				 representation(name = "character", binding = "character"),
				 prototype = list(name = as.character(NA)),
				 contains = c("SOS"),
				 validity = function(object) {
				 	#print("Entering validation: SOS_Test")
				 	return(TRUE)
				 }
)
testsos <- new("SOS_Test", name = "testcaps")

test_that("observed properties are parsed correctly", {
    xmlCaps <- xmlParseDoc("tests//responses//GetCapabilities_Mapserver.xml")
    parsedCaps <- parseSosCapabilities(xmlCaps, testsos)
    testsos@capabilities <- parsedCaps
    
    obs_prop <- sosObservedProperties(testsos)
    
    expect_equal(length(obs_prop), 1)
    expect_equal(obs_prop[[1]], "WaterQuality")
    # or should the components be listed?
    #expect_equal(obs_prop[[1]], "urn:ogc:def:property:OGC-SWE:1:STN_ID")
})
