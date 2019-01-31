
context("DescribeSensor")

testsos <- SOS_Test(name = "testcaps")
axiomCaps <- parseSosCapabilities(xmlParseDoc("../responses/Capabilities_52N-SOS_Axiom.xml"), testsos)
testsos@capabilities <- axiomCaps

test_that("output format is encoded correctly for POX", {
    describeSensorOp <- sosOperation(testsos, sosDescribeSensorName)
    of <- describeSensorOp@parameters[["outputFormat"]][[1]]
    
    ds <- SosDescribeSensor(service = sosService, version = testsos@version, procedure = sosProcedures(testsos)[[1]], outputFormat = of)
    
    expect_equal(ds@outputFormat, "text/xml; subtype=\"sensorML/1.0.1/profiles/ioos_sos/1.0\"")
    
    xml <- encodeRequestXML(ds, testsos)
    expect_match(toString(xml), "text/xml; subtype=&quot;sensorML/1.0.1/profiles/ioos_sos/1.0&quot;")
})

test_that("output format is encoded correctly for KVP", {
    describeSensorOp <- sosOperation(testsos, sosDescribeSensorName)
    of <- describeSensorOp@parameters[["outputFormat"]][[1]]
    
    ds <- SosDescribeSensor(service = sosService, version = testsos@version, procedure = sosProcedures(testsos)[[1]],
                            outputFormat = of)
    url <- encodeRequestKVP(ds, testsos) # fowards to .sosEncodeRequestKVPDescribeSensor_1.0.0
    expect_that(toString(url), matches("text%2Fxml%3B%20subtype%3D%22sensorML%2F1.0.1%2Fprofiles%2Fioos_sos%2F1.0%22"))
    
    # test different quotation variants and spaces
    ds <- SosDescribeSensor(service = sosService, version = testsos@version, procedure = sosProcedures(testsos)[[1]],
                            outputFormat = 'text/xml;subtype="sensorML/1.0.1"')
    url <- encodeRequestKVP(ds, testsos)
    expect_that(toString(url), matches("text%2Fxml%3Bsubtype%3D%22sensorML%2F1.0.1%22"))

    ds <- SosDescribeSensor(service = sosService, version = testsos@version, procedure = sosProcedures(testsos)[[1]],
                        outputFormat = "text/xml;subtype=\"sensorML/1.0.1\"")
    url <- encodeRequestKVP(ds, testsos)
    expect_that(toString(url), matches("text%2Fxml%3Bsubtype%3D%22sensorML%2F1.0.1%22"))

    ds <- SosDescribeSensor(service = sosService, version = testsos@version, procedure = sosProcedures(testsos)[[1]],
                        outputFormat = "text/xml; subtype=\"sensorML/1.0.1\"")
    url <- encodeRequestKVP(ds, testsos)
    expect_that(toString(url), matches("text%2Fxml%3B%20subtype%3D%22sensorML%2F1.0.1%22"))

})