context("parsing: O&M 2.0.0 observation")

#testsos <- SOS_Test(name = "testcaps",version=sos100_version, verboseOutput = TRUE)

testsos <- SOS(url = "http://fluggs.wupperverband.de/sos2/sos", version = "2.0.0", binding = "KVP")

offs <- sosOfferings(testsos)

getObservation(sos = testsos, offering = sosOfferingIds(testsos)[[1]])

xmlCaps <- xmlParseDoc("../responses/Capabilities_100_Example.xml")

tmpStoredXMLCaps = tempfile()

save(xmlCaps, file = tmpStoredXMLCaps)

cat(tmpStoredXMLCaps)

#parsedCaps <- parseSosCapabilities(obj = xmlCaps, sos = testsos)
#testsos@capabilities <- parsedCaps