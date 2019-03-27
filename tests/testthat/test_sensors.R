context("DescribeSensor: request encoding")

testsos <- SOS_Test(name = "testcaps")
axiomCaps <- parseSosCapabilities(xml2::read_xml(x = "../responses/Capabilities_52N-SOS_Axiom.xml"), testsos)
testsos@capabilities <- axiomCaps

test_that("output format for POX", {
    describeSensorOp <- sosOperation(testsos, sosDescribeSensorName)
    of <- describeSensorOp@parameters[["outputFormat"]][[1]]
    ds <- SosDescribeSensor(service = testsos@name, version = testsos@version, procedure = NA_character_, outputFormat = of)
    expect_equal(ds@outputFormat, "text/xml; subtype=\"sensorML/1.0.1/profiles/ioos_sos/1.0\"")

    xml <- encodeRequestXML(obj = ds, sos = testsos)
    expect_match(toString(xml), "text/xml; subtype=&quot;sensorML/1.0.1/profiles/ioos_sos/1.0&quot;")
})

test_that("procedure for POX", {
  ds <- SosDescribeSensor(service = testsos@name, version = testsos@version, procedure = "best_procedure", outputFormat = NA_character_)
  xml <- encodeRequestXML(obj = ds, sos = testsos)
  expect_match(toString(xml), "<procedure>best_procedure</procedure>")
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

context("DescribeSensor response parsing")

sensorDescription <- xml2::read_xml(x = "../responses/SensorDescription_Bochum.xml")

test_that("sensor id is parsed", {
  sml <- parseSensorML(obj = xml2::xml_root(sensorDescription), sos = testsos)
  expect_equal(sml@id, "http://www.52north.org/test/procedure/6")
})

test_that("sensor name is parsed", {
  sml <- parseSensorML(obj = xml2::xml_root(sensorDescription), sos = testsos)
  expect_equal(sml@name, "Hochschule Bochum")
})

test_that("sensor description is parsed", {
  sml <- parseSensorML(obj = xml2::xml_root(sensorDescription), sos = testsos)
  expect_equal(sml@description, "A test sensor in Bochum")
})

test_that("sensor position is parsed", {
  sml <- parseSensorML(obj = xml2::xml_root(sensorDescription), sos = testsos)
  expect_named(sml@coords, c("y", "x", "z"))
  expect_equal(sml@coords[["y"]], 51.447722)
  expect_equal(sml@coords[["x"]], 7.270806)
  expect_equal(sml@coords[["z"]], 52.0)
})

test_that("sensor bbox is parsed", {
  skip("TODO: implement test")
})
