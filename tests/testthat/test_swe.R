context("parsing: SWE position")

testsos <- SOS_Test(name = "testswe")

positionString <- '<swe:Position fixed="false" referenceFrame="urn:ogc:def:crs:EPSG::4326" xmlns:swe="http://www.opengis.net/swe/1.0.1">
	<swe:location>
		<swe:Vector>
			<swe:coordinate name="northing">
				<swe:Quantity axisID="y">
					<swe:uom code="degree"/>
					<swe:value>51.447722</swe:value>
				</swe:Quantity>
			</swe:coordinate>
			<swe:coordinate name="easting">
				<swe:Quantity axisID="x">
					<swe:uom code="degree"/>
					<swe:value>7.270806</swe:value>
				</swe:Quantity>
			</swe:coordinate>
			<swe:coordinate name="altitude">
				<swe:Quantity axisID="z">
					<swe:uom code="m"/>
					<swe:value>52.0</swe:value>
				</swe:Quantity>
			</swe:coordinate>
		</swe:Vector>
	</swe:location>
</swe:Position>'

test_that("reference frame", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = positionString))
  position <- parseSwePosition(obj = doc, sos = testsos)
  expect_equal(attr(position, "referenceFrame"), "urn:ogc:def:crs:EPSG::4326")
})

test_that("all coordinates", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = positionString))
  position <- parseSwePosition(obj = doc, sos = testsos)
  expect_length(position, 3)
})

test_that("coordinate names are the axisID", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = positionString))
  vector <- parseSweVector(obj = xml2::xml_find_first(doc, "//swe:Vector", ns = SosAllNamespaces()), sos = testsos)
  expect_length(vector, 3)
  expect_named(vector, c("y", "x", "z"))
})

context("parsing: SWE position")

coordinateString <- '<swe:coordinate name="altitude" xmlns:swe="http://www.opengis.net/swe/1.0.1">
	<swe:Quantity axisID="z">
		<swe:uom code="m"/>
		<swe:value>52.42</swe:value>
	</swe:Quantity>
</swe:coordinate>'

test_that("name is parsed", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = coordinateString))
  coordinate <- parseSweCoordinate(obj = doc, sos = testsos)
  expect_equal(coordinate$name, "altitude")
})

test_that("axis ID is parsed", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = coordinateString))
  coordinate <- parseSweCoordinate(obj = doc, sos = testsos)
  expect_equal(coordinate$axisID, "z")
})

test_that("UOM code is parsed", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = coordinateString))
  coordinate <- parseSweCoordinate(obj = doc, sos = testsos)
  expect_equal(coordinate$uomCode, "m")
})

test_that("value is parsed", {
  doc <- xml2::xml_root(x = xml2::read_xml(x = coordinateString))
  coordinate <- parseSweCoordinate(obj = doc, sos = testsos)
  expect_equal(coordinate$value, 52.42)
})
