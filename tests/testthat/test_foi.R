#context("parsing: O&M 2.0.0 observation")

#testsos <- SOS_Test(name = "testcaps",version=sos100_version, verboseOutput = TRUE)

testsos <- SOS(url = "http://fluggs.wupperverband.de/sos2/sos", version = "2.0.0", binding = "KVP")

foi <- getFeatureOfInterest(sos = testsos, featureOfInterest = "Panzer-Talsperre_Absperrbauwerk")

testsos <- SOS_Test(name = "testcaps",version=sos200_version, verboseOutput = TRUE)

foi <- parseGetFeatureOfInterestResponse(xmlRoot(xmlParseDoc("../responses/GetFeatureOfInterestResponse.xml")), testsos)


