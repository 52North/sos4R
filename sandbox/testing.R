################################################################################
# Copyright (C) 2010 by 52 North                                               #
# Initiative for Geospatial Open Source Software GmbH                          #
#                                                                              #
# Contact: Andreas Wytzisk                                                     #
# 52 North Initiative for Geospatial Open Source Software GmbH                 #
# Martin-Luther-King-Weg 24                                                    #
# 48155 Muenster, Germany                                                      #
# info@52north.org                                                             #
#                                                                              #
# This program is free software; you can redistribute and/or modify it under   #
# the terms of the GNU General Public License version 2 as published by the    #
# Free Software Foundation.                                                    #
#                                                                              #
# This program is distributed WITHOUT ANY WARRANTY; even without the implied   #
# WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU #
# General Public License for more details.                                     #
#                                                                              #
# You should have received a copy of the GNU General Public License along with #
# this program (see gpl-2.0.txt). If not, write to the Free Software           #
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or #
# visit the Free Software Foundation web page, http://www.fsf.org.             #
#                                                                              #
# Author: Daniel Nüst (daniel.nuest@uni-muenster.de)                           #
# Created: 2010-06-18                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

################################################################################
source("/home/daniel/Dropbox/2010_SOS4R/workspace/sos4R/sandbox/loadSources.R")


################################################################################
# RCurl

sosUrl = "http://giv-sos.uni-muenster.de:8080/ClimateSOS/sos"
request = "service=SOS&request=GetCapabilities&acceptVersions=1.0.0,2.0.0&sections=OperationsMetadata,ServiceIdentification,ServiceProvider,Filter_Capabilities,Contents&acceptFormats=text/xml"
url = paste(sosUrl, request, sep = "?")
getURL(url, verbose = TRUE)


################################################################################
# GetCapabilities

climatesosUrl = "http://giv-sos.uni-muenster.de:8080/ClimateSOS/sos"
climatesos = SOS(climatesosUrl)
weathersosUrl = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos"
weathersos = SOS(weathersosUrl, verboseOutput = FALSE)

caps = getCapabilities(weathersos)
sosCaps(weathersos)


################################################################################
# DescribeSensor

climatesosUrl = "http://giv-sos.uni-muenster.de:8080/ClimateSOS/sos"
climatesos = SOS(climatesosUrl, verboseOutput = FALSE)
id = "urn:ogc:object:feature:WMOStation:10280"
describeSensor(sos = climatesos, procedure = id)

# !!! describeSensor does not check if using GET, because Capabilities lack that DCP in current SOS!
weathersosUrl = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos"
weathersos = SOS(weathersosUrl, method = "POST", verboseOutput = TRUE)
sensor <- describeSensor(weathersos, sosProcedures(weathersos)[[1]])
sensor <- describeSensor(weathersos, "manniK")
sensor <- describeSensor(weathersos, sosProcedures(weathersos)[[1]], verbose = FALSE)

################################################################################
# GetObservation

# mandatory:
go.service = "SOS"
go.version = "1.0.0"
go.offering = "region_3"
go.observedProperty = c("urn:ogc:def:property:OGC:1.0:temperature", "urn:ogc:def:property:OGC:1.0:windDirection")
go.responseFormat = "text/xml;subtype=&quot;om/1.0.0&quot;"

# creation method
go <- GetObservation(service = go.service, version = go.version, offering = go.offering, observedProperty =  go.observedProperty, responseFormat =  go.responseFormat)
go.xml <- encode(go)

# with service
sos = SOS(climatesos)
localsos = SOS("http://localhost:8080/ClimateSOS-local/sos")
getObservation(localsos, offering = go.offering, observedProperty =  go.observedProperty, responseFormat = go.responseFormat)

# optional:
go.srsName = "urn:ogc:def:crs:EPSG:4326"
go.eventTimeInstant = "2010-04-12T11:00:00Z"
go.eventTimePeriod = "2008-04-10T10:00:00Z/2010-04-12T11:00:00Z"
go.procedure = c("urn:ogc:object:feature:WMOStation:104380",
		"urn:ogc:object:feature:WMOStation:226020",
		"urn:ogc:object:feature:WMOStation:33020")
go.foi = c("foi_id_1", "foi_id_2")
go.result = "<noResult>lala</noResult>" # not implemented for GET, causes exception "The parameter result is not supported by this SOS."
go.resultModel = "om:Observation" # om:CategoryObservation, om:Measurement, om:SpatialObservation, not mentioned by OOSTethys, but implemented in 52N SOS
go.responseMode = "inline" # reslutTemplate, only for usage with GetResult, so not need for know, not mentioned by OOSTethys, but implemented in 52N SOS
go.BBOX = "7.0,52.0,7.4,52.4"
go.BBOX2 = "7.0,52.0,7.4,52.4,urn:ogc:def:crs:EPSG:6.5:4326"

# timeperiod
go <- GetObservation(service = go.service, version = go.version, 
		offering = go.offering, observedProperty =  go.observedProperty,
		responseFormat =  go.responseFormat,
		srsName = go.srsName, eventTime = go.eventTimePeriod,
		procedure = go.procedure, featureOfInterest = go.foi,
		result = go.result,
		resultModel = go.resultModel,
		responseMode = go.responseMode, BBOX = go.BBOX)

go.xml <- encode(go)


################################################################################
# ExceptionReports

weathersos.url = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos"
weathersos = SOS(weathersos.url)

id.correct = "urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93"
id.incorrect = "lala"

er.xmltext <- "<ows:ExceptionReport xmlns:ows=\"http://www.opengis.net/ows/1.1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" version=\"1.0.0\" xsi:schemaLocation=\"http://schemas.opengis.net/ows/1.1.0/owsExceptionReport.xsd\"><ows:Exception exceptionCode=\"VersionNegotiationFailed\" locator=\"AcceptVersions\"><ows:ExceptionText>The parameter 'AcceptVersions' does not contain the version of this SOS: '1.0.0'</ows:ExceptionText></ows:Exception></ows:ExceptionReport>"
er.xmltext2 <- "<ows:ExceptionReport xmlns:ows=\"http://www.opengis.net/ows/1.1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" version=\"1.0.0\" lang=\"de-DE\" xsi:schemaLocation=\"http://schemas.opengis.net/ows/1.1.0/owsExceptionReport.xsd\"><ows:Exception exceptionCode=\"VersionNegotiationFailed\" locator=\"AcceptVersions\"><ows:ExceptionText>The parameter 'AcceptVersions' does not contain the version of this SOS: '1.0.0'</ows:ExceptionText></ows:Exception><ows:Exception exceptionCode=\"NoApplicableCode\" locator=\"@home\"><ows:ExceptionText>Just a second exception to make things saver...</ows:ExceptionText></ows:Exception></ows:ExceptionReport>"
er.doc <- xmlParseDoc(er.xmltext)
er.root <- xmlRoot(er.doc)

er2.doc <- xmlParseDoc(er.xmltext2)
er2.parsed <- parseOwsExceptionReport(er2.doc)
str(er2.parsed)

# unparsable report for wrong getcap:
er.xmltext3 <- "<ows:ExceptionReport xmlns:ows=\"http://www.opengis.net/ows/1.1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" version=\"1.0.0\" xsi:schemaLocation=\"http://schemas.opengis.net/ows/1.1.0/owsExceptionReport.xsd\"><ows:Exception exceptionCode=\"VersionNegotiationFailed\" locator=\"AcceptVersions\"><ows:ExceptionText>The parameter 'AcceptVersions' HAR HAR does not contain the version of this SOS: '1.0.0'</ows:ExceptionText></ows:Exception></ows:ExceptionReport>"
str(parseOwsExceptionReport(xmgetlParseDoc(er.xmltext3)))
# works from here!

debug(parseOwsExceptionReport)
debug(parseOwsException)

# no exception text
er.xmltext4 <- "<ows:ExceptionReport xmlns:ows=\"http://www.opengis.net/ows/1.1\" lang=\"de-DE\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" version=\"1.0.0\" lang=\"de-DE\" xsi:schemaLocation=\"http://schemas.opengis.net/ows/1.1.0/owsExceptionReport.xsd\" />"


################################################################################
# escaping special characters in url
input1 <- "urn:ogc:def:property:OGC:1.0:temperature_#_1"
input2 <- "om:SpatialObservation"
input3 <- "text/xml;subtype=\"OM/1.0.0\""

.kvpEscapeSpecialCharacters(input1)
.kvpEscapeSpecialCharacters(input2)
.kvpEscapeSpecialCharacters(input3)

# use test from above for kvp(...) method!

############################
# problem with "/" character
url = "http://localhost:8080/ClimateSOS-local/sos"
request1 = "service=SOS&request=GetCapabilities&acceptVersions=1.0.0,2.0.0&sections=All&acceptFormats=text/xml"
request2 = "service=SOS&request=GetCapabilities&acceptVersions=1.0.0,2.0.0&sections=All&acceptFormats=text%2Fxml"

getURL(paste(url, request1, sep = "?"))
getURL(paste(url, request2, sep = "?"))


################################################################################
# Parsing the capabilities file...

weathersos = SOS(url = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos")
#caps <- getCapabilities(weathersos, verbose = TRUE)
weathersos@capabilities

# some error
caps <- '...'
xmlCaps <- xmlParseDoc(caps)
debug(parseSosCapabilities)
parsedCaps <- parseSosCapabilities(xmlCaps, weathersos)

opXml <- xmlParseDoc('COPY HERE')
debug(parseOwsOperation)
parseOwsOperation(xmlRoot(opXml))


################################################################################
# accessor functions
weathersos = SOS("http://v-swe.uni-muenster.de:8080/WeatherSOS/sos")
sosUrl(weathersos)
sosMethod(weathersos)
sosVersion(weathersos)
sosCaps(weathersos)

################################################################################
# POST

# manually:
getCapRequest <- '<?xml version="1.0" encoding="UTF-8"?><GetCapabilities xmlns="http://www.opengis.net/sos/1.0" xmlns:ows="http://www.opengis.net/ows/1.1" xmlns:ogc="http://www.opengis.net/ogc" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.opengis.net/sos/1.0 http://schemas.opengis.net/sos/1.0.0/sosGetCapabilities.xsd" service="SOS"><ows:AcceptVersions><ows:Version>1.0.0</ows:Version></ows:AcceptVersions><ows:Sections><ows:Section>OperationsMetadata</ows:Section><ows:Section>ServiceIdentification</ows:Section><ows:Section>ServiceProvider</ows:Section><ows:Section>Filter_Capabilities</ows:Section><ows:Section>Contents</ows:Section></ows:Sections></GetCapabilities>'
# using 'post' for application/x-www-form-urlencoded content
caps.response <- postForm(uri = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos",
		request = getCapRequest,
		style = "POST",
		.encoding = "UTF-8")
caps.doc <- xmlParseDoc(caps.response)
caps <- parseSosCapabilities(caps.doc)

# GetCapabilities
sos = SOS("http://localhost:8080/ClimateSOS-local/sos", method = "POST",
		verboseOutput = TRUE)
sos = SOS("http://v-swe.uni-muenster.de:8080/WeatherSOS/sos", method = "POST",
		verboseOutput = TRUE)
caps = sosCaps(sos)

# DescribeSensor
procedures = sosProcedures(sos)
sensor.10 <- describeSensor(sos = sos, procedure = procedures[[1]])

################################################################################
# testing to call functions from a list
myFunc1 <- function(xml) {
	print("myfunc: ")
	print(xml)
	return(list(xml, "anotherone"))
}
value <- "lala la"
temp <- list(func1 = myFunc1)
# call the function:
result <- temp[["func1"]](value)
result

################################################################################
# Replace a parsing function... and inspect the request and response
myParseSensorML <- function(obj) {
	root <- xmlRoot(obj)
	return(xmlName(root))
}
myER <- function(xml) {
	return("EXCEPTION! RUN!!!!1111")
}
# testing:
myParsers <- SOSParsers("DescribeSensor" = myParseSensorML)
SOSParsers("DescribeSensor" = myParseSensorML, "ExceptionReport" = myER)
SOSParsers("DescribeSensor" = myParseSensorML, include = c("GetObservation", "DescribeSensor"))
SOSParsers("DescribeSensor" = myParseSensorML, exclude = c("GetObservation", "DescribeSensor"))


weathersos = SOS(url = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos",
		method = "POST",
		parsers = SOSParsers("DescribeSensor" = myParseSensorML, "ExceptionReport" = myER),
		verboseOutput = FALSE)
sensor <- describeSensor(weathersos, sosProcedures(weathersos)[[1]], inspect = TRUE)
# WORKS! YEAH!

################################################################################
# Replace an encoding function
myPostEncoding <- function(object, v) {
	# myPostEncoding
	.request <- encodeRequestXML(obj = object, verbose = v)
	# attach comment node to show this is actually used
	.request[[xmlSize(.request)+1]] <- xmlCommentNode("hej hej!")
	return(.request)
}

weathersos = SOS(url = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos",
		method = "POST",
		encoders = SOSEncoders("POST" = myPostEncoding),
		verboseOutput = TRUE)
sensor <- describeSensor(weathersos, sosProcedures(weathersos)[[1]],
		inspect = TRUE)
# works!

################################################################################
# inspecting XML using dummy parsing function
weathersos2 = SOS(url = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos",
		method = "POST",
		parsers = SOSDisabledParsers,
		verboseOutput = FALSE)
sensor2 <- describeSensor(weathersos2, sosProcedures(weathersos2)[[1]])
sensor2 <- describeSensor(weathersos2, "lala")
class(sensor) # from example above
class(sensor2)
# works!

################################################################################
# parsing om:Measurement

meas <- '<?xml version="1.0" encoding="UTF-8"?><om:ObservationCollection xmlns:om="http://www.opengis.net/om/1.0" xmlns:gml="http://www.opengis.net/gml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:sa="http://www.opengis.net/sampling/1.0" gml:id="oc_0" xsi:schemaLocation="http://www.opengis.net/sos/1.0 http://schemas.opengis.net/sos/1.0.0/sosAll.xsd"><om:member><om:Measurement gml:id="o_3376580"><om:samplingTime><gml:TimeInstant xsi:type="gml:TimeInstantType"><gml:timePosition>2010-09-08T09:45:00.000+02:00</gml:timePosition></gml:TimeInstant></om:samplingTime><om:procedure xlink:href="urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93"/><om:observedProperty xlink:href="urn:ogc:def:property:OGC::Temperature"/><om:featureOfInterest><sa:SamplingPoint gml:id="urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93"><gml:name>NOT_SET</gml:name><sa:position><gml:Point><gml:pos srsName="urn:ogc:def:crs:EPSG:4326">51.9412 7.6103</gml:pos></gml:Point></sa:position></sa:SamplingPoint></om:featureOfInterest><om:result uom="Cel">12.9</om:result></om:Measurement></om:member><om:member><om:Measurement gml:id="o_3376580"><om:samplingTime><gml:TimeInstant xsi:type="gml:TimeInstantType"><gml:timePosition>2010-09-08T09:45:00.000+02:00</gml:timePosition></gml:TimeInstant></om:samplingTime><om:procedure xlink:href="urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93"/><om:observedProperty xlink:href="urn:ogc:def:property:OGC::Temperature"/><om:featureOfInterest><sa:SamplingPoint gml:id="urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93"><gml:name>NOT_SET</gml:name><sa:position><gml:Point><gml:pos srsName="urn:ogc:def:crs:EPSG:4326">51.9412 7.6103</gml:pos></gml:Point></sa:position></sa:SamplingPoint></om:featureOfInterest><om:result uom="Cel">12.9</om:result></om:Measurement></om:member></om:ObservationCollection>'
measDoc <- xmlParseDoc(meas)

# for partial testing
tempM <- xmlChildren(xmlRoot(measDoc))[["member"]][["Measurement"]]
parsedM <- parseMeasurement(tempM, SOSParsers(), verbose = T)
tempSP <- xmlChildren(tempM[["featureOfInterest"]])[[1]]
parseSamplingPoint(tempSP)

om <- parseOM(obj = measDoc, parsers = SOSParsers(), verbose = TRUE)
str(om)
# works!

################################################################################
# test handling two of a few not supported observation specializations:
# category observation and geometry observation
obsDoc2 <- xmlParseDoc('<?xml version="1.0" encoding="UTF-8"?><om:ObservationCollection xmlns:om="http://www.opengis.net/om/1.0" xmlns:gml="http://www.opengis.net/gml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:swe="http://www.opengis.net/swe/1.0.1" gml:id="oc_0" xsi:schemaLocation="http://www.opengis.net/sos/1.0 http://schemas.opengis.net/sos/1.0.0/sosAll.xsd"><om:member><om:CategoryObservation></om:CategoryObservation></om:member><om:member><om:GeometryObservation></om:GeometryObservation></om:member></om:ObservationCollection>')
tempObs2 <- parseOM(obsDoc2, parsers = SOSParsers(), verbose = TRUE)
# works!


################################################################################
# creating data frames with time classes
value1 <- list(as.POSIXct("2008-01-01"), as.POSIXct("2009-02-02"),
		as.POSIXct("2010-03-03"))
value2 <- c("lala", "nana", "pooh")
value3 <- c(10.1, 12.4, 17.42)
value4 <- c("10.1", "12.4", "17.42")
value5 <- c(strptime("2010-03-01T12:15:00.000+01:00", sosDefaultTimeParsingFormat),
		strptime("2010-03-02T12:30:00.000+01:00", sosDefaultTimeParsingFormat))
value6 <- c("2010-03-01T12:15:00.000+01:00", "2010-03-02T12:30:00.000+01:00")

df <- data.frame(value1, value2, value3)
df1 <- data.frame(time = value5, name = value2[2:3])
df2 <- data.frame(temperature = value3)
df2[,"lala"] <- values4

timeDF <- data.frame(value5, stringsAsFactors = FALSE)
str(timeDF)
# WORKS, but only if value5 is NOT A LIST

# procedure as in parsing method:
weathersos = SOS(url = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos", method = "POST", verboseOutput = FALSE)
rawTestCurrentValues <- list("2010-03-01T12:15:00.000+01:00",
		"2010-03-02T12:30:00.000+01:00")
testCurrentValues <- lapply(rawTestCurrentValues, sosConvertTime, sos = weathersos)
str(testCurrentValues)
str(data.frame(I(testCurrentValues)))

# looks good... but columnList <- list(testCurrentValues, value2[2:3]) # does not do the trick
cbind(tempTimeDF, tempValuesDF)
str(cbind(tempTimeDF, tempValuesDF))
# why is this not like the following?
str(df1)

str(sosConvertTime(x = rawTestCurrentValues, sos = weathersos))
str(as.double(value4))
# not bad, so just give whole list to conversion function instead of calling lapply
str(sosConvertTime(rawTestCurrentValues, sos = weathersos))

tempTimeDF <- data.frame(sosConvertTime(
				x = list("2010-03-01T12:15:00.000+01:00",
						"2010-03-02T12:30:00.000+01:00"),
				sos = weathersos))
# cannot set name with variable on creation of the data.frame
names(tempTimeDF) <- "time"
tempValuesDF <- data.frame("values" = sosConvertDouble(list("11.1", "12.4")))
tempId = "tempId"
tempData = data.frame(tempId = seq(1:2))
tempData <- cbind(tempId, tempTimeDF, tempValuesDF); str(tempData)
tempData[,!colnames(tempData)%in%tempId]
# YEAH! WORKS!


################################################################################
# parsing om:Observation

obsDoc <- xmlParseDoc('<?xml version="1.0" encoding="UTF-8"?><om:ObservationCollection xmlns:om="http://www.opengis.net/om/1.0" xmlns:gml="http://www.opengis.net/gml" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:swe="http://www.opengis.net/swe/1.0.1" gml:id="oc_0" xsi:schemaLocation="http://www.opengis.net/sos/1.0 http://schemas.opengis.net/sos/1.0.0/sosAll.xsd"><om:member><om:Observation><om:samplingTime><gml:TimePeriod xsi:type="gml:TimePeriodType"><gml:beginPosition>2010-09-08T09:45:00.000+02:00</gml:beginPosition><gml:endPosition>2010-09-08T09:45:00.000+02:00</gml:endPosition></gml:TimePeriod></om:samplingTime><om:procedure xlink:href="urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93"/><om:observedProperty><swe:CompositePhenomenon gml:id="cpid0" dimension="1"><gml:name>resultComponents</gml:name><swe:component xlink:href="urn:ogc:data:time:iso8601"/><swe:component xlink:href="urn:ogc:def:property:OGC::Temperature"/></swe:CompositePhenomenon></om:observedProperty><om:featureOfInterest><gml:FeatureCollection/></om:featureOfInterest><om:result><swe:DataArray><swe:elementCount><swe:Count><swe:value>1</swe:value></swe:Count></swe:elementCount><swe:elementType name="Components"><swe:SimpleDataRecord><swe:field name="Time"><swe:Time definition="urn:ogc:data:time:iso8601"/></swe:field><swe:field name="feature"><swe:Text definition="urn:ogc:data:feature"/></swe:field><swe:field name="urn:ogc:def:property:OGC::Temperature"><swe:Quantity definition="urn:ogc:def:property:OGC::Temperature"><swe:uom code="Cel"/></swe:Quantity></swe:field></swe:SimpleDataRecord></swe:elementType><swe:encoding><swe:TextBlock decimalSeparator="." tokenSeparator="," blockSeparator=";"/></swe:encoding><swe:values>2010-03-01T12:15:00.000+01:00,urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93,73.0;2010-03-01T12:30:00.000+01:00,urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93,68.0;2010-03-01T12:45:00.000+01:00,urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93,69.0;2010-03-01T13:00:00.000+01:00,urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93,65.0;2010-03-01T13:15:00.000+01:00,urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93,61.0;2010-03-01T13:30:00.000+01:00,urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93,56.0;2010-03-01T13:45:00.000+01:00,urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93,60.0;2010-03-01T14:00:00.000+01:00,urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93,57.0;</swe:values></swe:DataArray></om:result></om:Observation></om:member></om:ObservationCollection>')
weathersos = SOS(url = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos", method = "POST", verboseOutput = FALSE)

# testing parts of the observation document:
tempPP <- parsePhenomenonProperty(xmlRoot(obsDoc)[[omMemberName]][[omObservationName]][[omObservedPropertyName]])
tempCP <- parseCompositePhenomenon(xmlRoot(obsDoc)[[omMemberName]][[omObservationName]][[omObservedPropertyName]][[sweCompositePhenomenonName]])
tempTP <- parseSamplingTime(xmlRoot(obsDoc)[[omMemberName]][[omObservationName]][[omSamplingTimeName]])
tempFOI <- parseFOI(xmlRoot(obsDoc)[[omMemberName]][[omObservationName]][[omFeatureOfInterestName]])
tempResult <- parseResult(xmlRoot(obsDoc)[[omMemberName]][[omObservationName]][[omResultName]], sos = weathersos, verbose = TRUE)
tempDA <- parseDataArray(xmlRoot(obsDoc)[[omMemberName]][[omObservationName]][[omResultName]][[sweDataArrayName]], sos = weathersos, verbose = TRUE)
tempFields <- parseElementType(xmlRoot(obsDoc)[[omMemberName]][[omObservationName]][[omResultName]][[sweDataArrayName]][[sweElementTypeName]])
tempEncoding <- parseEncoding(xmlRoot(obsDoc)[[omMemberName]][[omObservationName]][[omResultName]][[sweDataArrayName]][[sweEncodingName]])
tempValues <- parseValues(xmlRoot(obsDoc)[[omMemberName]][[omObservationName]][[omResultName]][[sweDataArrayName]][[sweValuesName]], fields = tempFields, encoding = tempEncoding, sos = weathersos, verbose = TRUE)

tempObs <- parseObservation(xmlRoot(obsDoc)[[omMemberName]][[omObservationName]], sos = weathersos, verbose = TRUE)

# if the observation has a foi with a coordinate, add that to all data rows?

################################################################################
source("/home/daniel/Dokumente/2010_SOS4R/workspace/sos4R/sandbox/loadSources.R")


################################################################################
# GetObservationById

# some ids for weathersos
ids = paste("o_33765", c(80:99), sep = "")
getobsbyid <- GetObservationById("SOS", "1.0.0", ids[1],
		"text/xml;subtype=&quot;om/1.0.0&quot;")
getobsbyid
encodeRequestXML(getobsbyid) # is valid!

getobsbyid <- GetObservationById(service = "SOS", version = "1.0.0",
		observationId = ids[2],
		responseFormat = "text/xml;subtype=&quot;om/1.0.0&quot;",
		resultModel = "om:Measurement", responseMode = "inline",
		srsName = "urn:ogc:def:crs:EPSG:4326")
getobsbyid
encodeRequestXML(getobsbyid) # is valid!

weathersos = SOS(url = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos",
		method = "POST", verboseOutput = FALSE)
ids = paste("o_33765", c(80:99), sep = "")
obs <- getObservationById(sos = weathersos, observationId = ids[[1]],
		responseFormat = sosDefaultGetObsResponseFormat,
		srsName = as.character(NA), resultModel = as.character(NA),
		responseMode = as.character(NA), verbose = TRUE)
# works!

obs <- getObservationById(sos = weathersos, observationId = ids[[1]], verbose = TRUE)
obs <- getObservationById(sos = weathersos, observationId = ids[[1]], resultModel = SosSupportedResultModels()[[2]], verbose = TRUE)
# works!

################################################################################
# temporal operations
weathersos = SOS("http://v-swe.uni-muenster.de:8080/WeatherSOS/sos", method = "POST")

format(as.POSIXct("2010-07-01 12:00"), weathersos@timeFormat)

t1 <- sosCreateTimeInstant(sos = weathersos, time = as.POSIXct("2010-07-01 12:00"))
p1 <- sosCreateTimePeriod(sos = weathersos, begin = as.POSIXct("2010-03-01 12:15"),
		end = as.POSIXct("2010-03-02 12:15"))
# works!

eventTime1 <- sosCreateEventTime(SosSupportedTemporalOperators()[["TM_During"]], p1)
encodeEventTimeXML(eventTime1)
encodeEventTimeKVP(eventTime1)
# TODO hier geht's weiter...

################################################################################
# GetObservations

weathersos = SOS("http://v-swe.uni-muenster.de:8080/WeatherSOS/sos", method = "POST")

# request:
go.offering = sosOfferings(weathersos)[[3]]@id # ATHMOSPHERIC
go.observedProperty = sosObservedProperties(weathersos)[[3]] # temp
#go.responseFormat - leave to default
go.responseMode = sosResponseMode(weathersos)[[2]] # inline
go.eventTime = "2010-07-01T12:00/2010-07-10T12:00" 
go.resultModel = sosResultModels(weathersos)[[3]] # om:Measurement
go.srsName = "urn:ogc:def:crs:EPSG:6.8:4326" # is "AnyValue" in capabilities... contacted Carsten about that

# no event time -> latest
getObservation(sos = weathersos, offering = go.offering,
		observedProperty =  go.observedProperty)

# TODO bbox

# TODO time interval


################################################################################
# SOAP

# TODO ********************************************************* continue here!!


################################################################################
# PegelOnlineSOS
pegelsos <- SOS(url = "http://v-sos.uni-muenster.de:8080/PegelOnlineSOSv2/sos")
# works so far... :-)

# AirQualitySOS

################################################################################
source("/home/daniel/Dokumente/2010_SOS4R/workspace/sos4R/sandbox/loadSources.R")