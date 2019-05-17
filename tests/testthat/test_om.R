context("parsing: OM 2.0 result")

resultXml <- '<om:result xmlns:om="http://www.opengis.net/om/2.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ns="http://www.opengis.net/gml/3.2" uom="test_unit_9_3" xsi:type="ns:MeasureType">50.28</om:result>'

test_that("result value parsing works", {
  testsos <- SOS_Test(name = "omresult")
  result <- parseResult(obj = xml2::read_xml(x = resultXml), sos = testsos)

  expect_s3_class(result, "data.frame")
  expect_equal(dim(result), c(1,1))
  expect_equal(result[1,1], 50.28)
})

context("parsing: OM_Observation from sos:observation")

observationXml <- '<sos:observation xmlns:sos="http://www.opengis.net/sos/2.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:om="http://www.opengis.net/om/2.0" xmlns:gml="http://www.opengis.net/gml/3.2" xmlns:xlink="http://www.w3.org/1999/xlink">
    <om:OM_Observation gml:id="o_121">
      <gml:identifier codeSpace="http://www.opengis.net/def/nil/OGC/0/unknown">http://www.52north.org/test/observation/1</gml:identifier>
      <om:type xlink:href="http://www.opengis.net/def/observationType/OGC-OM/2.0/OM_Measurement"/>
      <om:phenomenonTime>
        <gml:TimeInstant gml:id="phenomenonTime_121">
          <gml:timePosition>2012-11-19T13:00:00.000Z</gml:timePosition>
        </gml:TimeInstant>
      </om:phenomenonTime>
      <om:resultTime xlink:href="#phenomenonTime_121"/>
      <om:procedure xlink:href="http://www.52north.org/test/procedure/1"/>
      <om:observedProperty xlink:href="http://www.52north.org/test/observableProperty/1"/>
      <om:featureOfInterest xlink:href="http://www.52north.org/test/featureOfInterest/1" xlink:title="con terra"/>
      <om:result xmlns:ns="http://www.opengis.net/gml/3.2" uom="test_unit_1" xsi:type="ns:MeasureType">1.23</om:result>
    </om:OM_Observation>
  </sos:observation>'

testsos <- SOS_Test(name = "omresult", version = sos200_version)

test_that("correct class is returned", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)
  expect_s4_class(observation, "OmOM_Observation")
})

test_that("time parsing works", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)

  times <- sosTime(observation)
  expect_equal(times$resultTime, times$phenomenonTime)
  expect_true(is.list(times))
  expect_s3_class(times[[1]], "POSIXct")
  expect_s3_class(times[[2]], "POSIXct")
  expect_named(times, c("resultTime", "phenomenonTime"))
})

test_that("coordinates are not available without FOI with a warning", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)

  expect_warning(coords <- sosCoordinates(observation),
                 "contains a feature")
  expect_true(is.na(coords))
})

test_that("coordinates are available with retrieved FOI", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/52n-sos-webapp/service/kvp",
               binding = "KVP",
               useDCPs = FALSE,
               version = sos200_version)
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = mySOS, featureCache = list())

  coords <- sosCoordinates(observation)
  expect_named(coords, c("lon", "lat", "SRS"))
  expect_equal(coords[["lon"]], 7.727958)
})

test_that("result parsed from observation", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)
  result <- sosResult(observation)

  expect_equal(dim(result), c(1, 1))
  expect_equal(result[1,1], 1.23)
  expect_named(result, c("test_unit_1"))
})

test_that("observation metadata is in result", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)
  result <- sosResult(observation)

  expect_named(result, c("test_unit_1"))
  skip("Result data.frame not unified yet.")
  expect_named(attributes(result[,1]), c("name", "definition", "unit of measurement"))
  expect_equal(sosUOM(result), c("test_unit_1"))
})

test_that("feature ID parsed from observation", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)
  fid <- sosFeatureIds(observation)

  expect_length(fid, 1)
  expect_equal(fid, "http://www.52north.org/test/featureOfInterest/1")
})

test_that("observed property parsed from observation", {
  observation <- parseObservation_2.0(obj = xml2::read_xml(x = observationXml), sos = testsos, featureCache = list(), retrieveFOI = FALSE)
  obsProps <- sosObservedProperties(observation)

  expect_length(obsProps, 1)
  expect_equal(obsProps, "http://www.52north.org/test/observableProperty/1")
})

context("OM 2.0: parse observation with swe20:DataArray")

observationXml20 <- '<om:OM_Observation xmlns:om="http://www.opengis.net/om/2.0" xmlns:gml="http://www.opengis.net/gml/3.2" gml:id="o_19">
      <om:type xlink:href="http://www.opengis.net/def/observationType/OGC-OM/2.0/OM_Measurement"/>
      <om:phenomenonTime>
        <gml:TimePeriod xmlns:gml="http://www.opengis.net/gml/3.2" gml:id="phenomenonTime_19">
          <gml:beginPosition>2000-01-01T13:45:00.000+13:00</gml:beginPosition>
          <gml:endPosition>2000-01-02T13:00:00.000+13:00</gml:endPosition>
        </gml:TimePeriod>
      </om:phenomenonTime>
      <om:resultTime>
        <gml:TimeInstant xmlns:gml="http://www.opengis.net/gml/3.2" gml:id="ti_a24f792082e3114273665b1b1e0645c99da9a8207d427445a23b689fee63ca71">
          <gml:timePosition>2000-01-02T13:00:00.000+13:00</gml:timePosition>
        </gml:TimeInstant>
      </om:resultTime>
      <om:procedure xlink:href="Hydrometric_Station"/>
      <om:parameter>
        <om:NamedValue xmlns:om="http://www.opengis.net/om/2.0" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
          <om:name xlink:href="offering"/>
          <om:value xmlns:xs="http://www.w3.org/2001/XMLSchema" xsi:type="xs:string">QR.Master@58902</om:value>
        </om:NamedValue>
      </om:parameter>
      <om:observedProperty xlink:href="Discharge" xlink:title="Discharge"/>
      <om:featureOfInterest xmlns:xlink="http://www.w3.org/1999/xlink" xlink:href="#mp_a2f42077f7b87d26d7bf42352a619a2903b5e16754376eeb216a813530b49e12"/>
      <om:result>
        <swe:DataArray xmlns:swe="http://www.opengis.net/swe/2.0" xmlns:xlink="http://www.w3.org/1999/xlink">
          <swe:elementCount>
            <swe:Count>
              <swe:value>75</swe:value>
            </swe:Count>
          </swe:elementCount>
          <swe:elementType name="Components">
            <swe:DataRecord xmlns:ns="http://www.opengis.net/swe/2.0">
              <ns:field name="phenomenonTime">
                <ns:Time definition="http://www.opengis.net/def/property/OGC/0/PhenomenonTime">
                  <ns:uom xlink:href="http://www.opengis.net/def/uom/ISO-8601/0/Gregorian"/>
                </ns:Time>
              </ns:field>
              <ns:field name="Discharge">
                <ns:Quantity definition="Discharge">
                  <ns:uom code="m^3/s"/>
                </ns:Quantity>
              </ns:field>
            </swe:DataRecord>
          </swe:elementType>
          <swe:encoding>
            <swe:TextEncoding xmlns:ns="http://www.opengis.net/swe/2.0" blockSeparator="@@" decimalSeparator="." tokenSeparator=","/>
          </swe:encoding>
          <swe:values>2000-01-01T13:45:00.000+13:00,6.3519049167120602561453779344446957111358642578125@@2000-01-01T14:15:00.000+13:00,6.57704226105250011613634342211298644542694091796875@@2000-01-01T15:15:00.000+13:00,7.18113035097575025389460279257036745548248291015625@@2000-01-01T15:45:00.000+13:00,7.59075712403778002368426314205862581729888916015625@@2000-01-01T16:00:00.000+13:00,7.88848130589331031359279222670011222362518310546875@@2000-01-01T16:15:00.000+13:00,23.278163964113101513930814689956605434417724609375@@2000-01-01T16:30:00.000+13:00,51.29666874426440159595586010254919528961181640625@@2000-01-01T16:45:00.000+13:00,60.73919477926460075423165108077228069305419921875@@2000-01-01T17:00:00.000+13:00,63.32053778936529653265097294934093952178955078125@@2000-01-01T17:15:00.000+13:00,62.980048852571002271361066959798336029052734375@@2000-01-01T17:30:00.000+13:00,62.41510085883459879596557584591209888458251953125@@2000-01-01T17:45:00.000+13:00,61.5177559518705976415731129236519336700439453125@@2000-01-01T18:00:00.000+13:00,61.96541867946410064860174315981566905975341796875@@2000-01-01T18:15:00.000+13:00,63.77629964009940266578269074670970439910888671875@@2000-01-01T18:30:00.000+13:00,66.2024961829140039526464533992111682891845703125@@2000-01-01T18:45:00.000+13:00,67.3776772414025941770887584425508975982666015625@@2000-01-01T19:00:00.000+13:00,69.0445835736451982711514574475586414337158203125@@2000-01-01T19:15:00.000+13:00,69.2847828364240996279477258212864398956298828125@@2000-01-01T19:30:00.000+13:00,68.446352124105402481291093863546848297119140625@@2000-01-01T19:45:00.000+13:00,66.7884799873288983462771284393966197967529296875@@2000-01-01T20:00:00.000+13:00,64.693927835606899634512956254184246063232421875@@2000-01-01T20:15:00.000+13:00,62.980048852571002271361066959798336029052734375@@2000-01-01T20:30:00.000+13:00,61.5177559518705976415731129236519336700439453125@@2000-01-01T20:45:00.000+13:00,59.297902305248499033041298389434814453125@@2000-01-01T21:00:00.000+13:00,57.44504786575799926140462048351764678955078125@@2000-01-01T21:15:00.000+13:00,56.15920790254819650044737500138580799102783203125@@2000-01-01T21:45:00.000+13:00,54.1603468157311027653122437186539173126220703125@@2000-01-01T22:15:00.000+13:00,53.12643934345229723703596391715109348297119140625@@2000-01-01T22:30:00.000+13:00,52.1049537826254010042248410172760486602783203125@@2000-01-01T22:45:00.000+13:00,51.29666874426440159595586010254919528961181640625@@2000-01-01T23:00:00.000+13:00,50.2973911345249007354141212999820709228515625@@2000-01-01T23:15:00.000+13:00,49.70866141409189964406323269940912723541259765625@@2000-01-01T23:30:00.000+13:00,48.84095310263540312689656275324523448944091796875@@2000-01-01T23:45:00.000+13:00,47.040045629464799503693939186632633209228515625@@2000-01-02T00:00:00.000+13:00,46.57306148546680191202540299855172634124755859375@@2000-01-02T00:30:00.000+13:00,44.82471139767110202001276775263249874114990234375@@2000-01-02T00:45:00.000+13:00,43.65222383616990242671818123199045658111572265625@@2000-01-02T01:00:00.000+13:00,43.02882788567109884070305270142853260040283203125@@2000-01-02T01:30:00.000+13:00,41.45118533759119827664108015596866607666015625@@2000-01-02T01:45:00.000+13:00,40.50048995191109924007832887582480907440185546875@@2000-01-02T02:15:00.000+13:00,39.3893683185464027474154136143624782562255859375@@2000-01-02T02:30:00.000+13:00,39.05134583751699750564512214623391628265380859375@@2000-01-02T02:45:00.000+13:00,39.05134583751699750564512214623391628265380859375@@2000-01-02T03:00:00.000+13:00,39.5590596205154980680163134820759296417236328125@@2000-01-02T03:30:00.000+13:00,41.36460539274529679687475436367094516754150390625@@2000-01-02T03:45:00.000+13:00,42.23545093301360253690290846861898899078369140625@@2000-01-02T04:15:00.000+13:00,43.2063752722039993159341975115239620208740234375@@2000-01-02T04:30:00.000+13:00,43.02882788567109884070305270142853260040283203125@@2000-01-02T04:45:00.000+13:00,42.4988955537319981203836505301296710968017578125@@2000-01-02T05:00:00.000+13:00,42.323153246862801779570872895419597625732421875@@2000-01-02T05:15:00.000+13:00,41.6246812455222965354550979100167751312255859375@@2000-01-02T05:30:00.000+13:00,41.36460539274529679687475436367094516754150390625@@2000-01-02T05:45:00.000+13:00,41.27813738003550270150299184024333953857421875@@2000-01-02T06:00:00.000+13:00,42.0603837080110025681278784759342670440673828125@@2000-01-02T06:15:00.000+13:00,42.14786110516690342819856596179306507110595703125@@2000-01-02T06:30:00.000+13:00,42.41096810200939870583169977180659770965576171875@@2000-01-02T06:45:00.000+13:00,41.79862554732250146116712130606174468994140625@@2000-01-02T07:15:00.000+13:00,40.24236967931739883397312951274216175079345703125@@2000-01-02T08:30:00.000+13:00,37.14273066494789787839181371964514255523681640625@@2000-01-02T08:45:00.000+13:00,36.57361711252939784344562212936580181121826171875@@2000-01-02T09:00:00.000+13:00,36.25086036512829679168135044164955615997314453125@@2000-01-02T09:15:00.000+13:00,35.77005728422650321363107650540769100189208984375@@2000-01-02T09:30:00.000+13:00,35.451737632835403246645000763237476348876953125@@2000-01-02T09:45:00.000+13:00,34.89893036714399698894339962862432003021240234375@@2000-01-02T10:00:00.000+13:00,34.82039871278539777676996891386806964874267578125@@2000-01-02T10:15:00.000+13:00,34.3515171197853987905546091496944427490234375@@2000-01-02T10:30:00.000+13:00,34.04112335964399704835159354843199253082275390625@@2000-01-02T10:45:00.000+13:00,33.9637986096691975035355426371097564697265625@@2000-01-02T11:00:00.000+13:00,34.04112335964399704835159354843199253082275390625@@2000-01-02T11:15:00.000+13:00,33.5021444298516968274270766414701938629150390625@@2000-01-02T11:45:00.000+13:00,33.044413883369799123101984150707721710205078125@@2000-01-02T12:00:00.000+13:00,32.66413005778360201247778604738414287567138671875@@2000-01-02T12:30:00.000+13:00,32.89224673871029835936496965587139129638671875@@2000-01-02T12:45:00.000+13:00,33.27278952403759859635101747699081897735595703125@@2000-01-02T13:00:00.000+13:00,33.8865832142979996888243476860225200653076171875</swe:values>
        </swe:DataArray>
      </om:result>
    </om:OM_Observation>'

test_that("SWE 2.0 data array can be parsed", {
  skip("NOT IMPLEMENTED")
})

context("OM: accessors")

test_that("coordinates from om:ObservationProperty returns data.frame with NAs", {
  property <- OmObservationProperty(href = "http://obs/prop/1")
  sos <- SOS_Test(name = "testgml")
  expect_warning(coords <- sosCoordinates(property),
                  "No coordinates")
  expect_named(coords, c("lon", "lat", "SRS"))
  expect_equal(as.list(coords[1,]), list(lon = NA, lat = NA, SRS = NA))
})
