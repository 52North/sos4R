
context("DescribeSensor")

testsos <- SOS_Test(name = "testcaps")
axiomCaps <- parseSosCapabilities(XML::xmlParseDoc(file = "../responses/Capabilities_52N-SOS_Axiom.xml"), testsos)
testsos@capabilities <- axiomCaps

test_that("output format is encoded correctly for POX", {
    describeSensorOp <- sosOperation(testsos, sosDescribeSensorName)
    of <- describeSensorOp@parameters[["outputFormat"]][[1]]

    ds <- SosDescribeSensor(service = testsos@name, version = testsos@version, procedure = sosProcedures(testsos)[[1]], outputFormat = of)

    expect_equal(ds@outputFormat, "text/xml; subtype=\"sensorML/1.0.1/profiles/ioos_sos/1.0\"")

    xml <- encodeRequestXML(obj = ds, sos = testsos)
    expect_match(toString(xml), "text/xml; subtype=&quot;sensorML/1.0.1/profiles/ioos_sos/1.0&quot;")
})

test_that("output format is encoded correctly for KVP", {
    skip("TO BE FIXED")

    describeSensorOp <- sosOperation(testsos, sosDescribeSensorName)
    of <- describeSensorOp@parameters[["outputFormat"]][[1]]

    ds <- SosDescribeSensor(service = testsos@name, version = testsos@version, procedure = sosProcedures(testsos)[[1]],
                            outputFormat = of)
    url <- encodeRequestKVP(ds, testsos) # fowards to .sosEncodeRequestKVPDescribeSensor_1.0.0
    expect_match(toString(url), "text/xml;%20subtype%3D\"sensorML/1.0.1/profiles/ioos_sos/1.0\"")

    # test different quotation variants and spaces
    ds <- SosDescribeSensor(service = testsos@name, version = testsos@version, procedure = sosProcedures(testsos)[[1]],
                            outputFormat = 'text/xml;subtype="sensorML/1.0.1"')
    url <- encodeRequestKVP(ds, testsos)
    expect_match(toString(url), "text/xml;subtype%3D\"sensorML/1.0.1\"")

    ds <- SosDescribeSensor(service = testsos@name, version = testsos@version, procedure = sosProcedures(testsos)[[1]],
                        outputFormat = "text/xml;subtype=\"sensorML/1.0.1\"")
    url <- encodeRequestKVP(ds, testsos)
    expect_match(toString(url), "text/xml;subtype%3D\"sensorML/1.0.1\"")

    ds <- SosDescribeSensor(service = testsos@name, version = testsos@version, procedure = sosProcedures(testsos)[[1]],
                        outputFormat = "text/xml; subtype=\"sensorML/1.0.1\"")
    url <- encodeRequestKVP(ds, testsos)
    expect_match(toString(url), "text/xml;%20subtype%3D\"sensorML/1.0.1\"")

})
