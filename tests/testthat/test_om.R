#context("parsing: O&M 2.0.0 observation")

#testsos <- SOS_Test(name = "testcaps",version=sos100_version, verboseOutput = TRUE)

testsos <- SOS(url = "http://fluggs.wupperverband.de/sos2/sos", version = "2.0.0", binding = "KVP")

offs <- sosOfferings(testsos)

observation <- getObservation(sos = testsos, offering = sosOfferingIds(testsos)[[1]], responseFormat = "http://www.opengis.net/om/2.0")

testsos <- SOS_Test(name = "testcaps",version=sos200_version, verboseOutput = TRUE)

testsos <- SOS(url = "http://fluggs.wupperverband.de/sos2/sos", version = "2.0.0", binding = "KVP")

obs2 <- parseGetObservationResponse(xmlRoot(xmlParseDoc("../responses/GetObservationResponse.xml")), testsos)

print(length(obs2))