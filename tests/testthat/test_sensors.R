context("DescribeSensor: request encoding")

testsos <- SOS_Test(name = "testsensor")
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
    url <- encodeRequestKVP(ds, testsos)
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

context("parsing: sensor description")

sensorDescription <- xml2::read_xml(x = "../responses/SensorDescription_Bochum.xml")

test_that("sensor id is parsed", {
  sml <- parseSensorML(obj = sensorDescription, sos = testsos)
  expect_equal(sml@id, "http://www.52north.org/test/procedure/6")
})

test_that("sensor name is parsed", {
  sml <- parseSensorML(obj = sensorDescription, sos = testsos)
  expect_equal(sml@name, "Hochschule Bochum")
})

test_that("sensor description is parsed", {
  sml <- parseSensorML(obj = sensorDescription, sos = testsos)
  expect_equal(sml@description, "A test sensor in Bochum")
})

test_that("sensor position is parsed", {
  sml <- parseSensorML(obj = sensorDescription, sos = testsos)
  expect_named(sml@coords, c("y", "x", "z"))
  expect_equal(sml@coords[["y"]], 51.447722)
  expect_equal(sml@coords[["x"]], 7.270806)
  expect_equal(sml@coords[["z"]], 52.0)
})

test_that("sensor boundedBy", {
  sml <- parseSensorML(obj = xml2::xml_root(sensorDescription), sos = testsos)
  expect_equal(dim(sosBoundedBy(sml)), c(2, 2))
  expect_equal(dimnames(sosBoundedBy(sml)), list(c("coords.lon", "coords.lat"), c("min", "max")))
})

test_that("sp bbox can be created from boundedBy", {
  sml <- parseSensorML(obj = xml2::xml_root(sensorDescription), sos = testsos)
  expect_match(toString(sp::bbox(sosBoundedBy(sml))), "7.270806, 7.370806, 51.447722, 51.557722")
})

context("DescribeSensor: integration tests\n")

test_that("sensor description can be requested", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/pox",
               binding = "POX", dcpFilter = list("POX" = "/pox"))

  mySensor <- describeSensor(sos = mySOS,
                             procedure = sosProcedures(obj = mySOS)[[1]][[1]],
                             outputFormat = 'text/xml; subtype="sensorML/1.0.1"'
                             #,inspect = TRUE
  )

  expect_equal(sosName(mySensor), "adxl345")
  expect_equal(sosId(mySensor), "adxl345")
  expect_s3_class(mySensor@xml, "xml_document")
  expect_named(sosCoordinates(mySensor), c("x", "y", "z"))
  expect_length(sosCoordinates(mySensor), 3)
  expect_match(toString(sp::bbox(sosBoundedBy(mySensor))), "7.5, 7.5, 52, 52")
})

test_that("sensor description can be saved to file during request", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/pox",
               binding = "POX", dcpFilter = list("POX" = "/pox"))

  tempfile <- tempfile(fileext = ".xml")
  expect_output(
    mySensor <- describeSensor(sos = mySOS,
                             procedure = sosProcedures(obj = mySOS)[[1]][[1]],
                             outputFormat = 'text/xml; subtype="sensorML/1.0.1"',
                             saveOriginal = tempfile),
    paste0("document saved:(.*)", tempfile))

  expect_true(file.exists(tempfile))
  sensorXmlFromFile <- xml2::read_xml(tempfile)
  expect_equal(xml2::xml_name(sensorXmlFromFile), "SensorML")
  file.remove(tempfile)
  expect_false(file.exists(tempfile))
})