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
url = paste(sosUrl, request, sep="?")
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
weathersos = SOS(weathersosUrl, method = "POST", verboseOutput = FALSE)
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

weathersos.url = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos"
weathersos = SOS(weathersos.url)
#caps <- getCapabilities(weathersos, verbose = TRUE)
weathersos@capabilities

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
sensor.10 <- describeSensor(sos = sos, procedure = procedures[1], verbose = TRUE)

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
# Replace a parsing function...
myParseSensorML <- function(xml) {
	root <- xmlRoot(xml)
	return(xmlValue(root))
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
		parsers = SOSParsers("DescribeSensor" = myParseSensorML),
		verboseOutput = FALSE)
sensor <- describeSensor(weathersos, sosProcedures(weathersos)[[1]])
# WORKS! YEAH!

################################################################################
# Replace an encoding function
myPostEncoding <- function(object, v) {
	# myPostEncoding
	return(encodeRequestXML(obj = object, verbose = v))
}
	
weathersos = SOS(url = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos",
		method = "POST",
		encoders = SOSEncoders("POST" = myPostEncoding),
		verboseOutput = TRUE)
sensor <- describeSensor(weathersos, sosProcedures(weathersos)[[1]])
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
# SOAP


################################################################################
# GetObservationById
getobsbyid.id <- "lala:123"
getobsbyid <- GetObservationById("SOS", "1.0.0", getobsbyid.id,
		"text/xml;subtype=&quot;om/1.0.0&quot;")
getobsbyid
encode(getobsbyid)
getobsbyid <- GetObservationById(service = "SOS", version = "1.0.0",
		observationId = getobsbyid.id,
		responseFormat = "text/xml;subtype=&quot;om/1.0.0&quot;",
		resultModel = "om:Measurement", responseMode = "inline",
		srsName = "epsg:1234")
getobsbyid
encode(getobsbyid) # is valid!

# TODO ********************************************************* continue here!!


################################################################################
# Parsing observations

sos = SOS("http://v-swe.uni-muenster.de:8080/WeatherSOS/sos", method = "GET")

# request:
go.offering = sosOfferings(sos)[[6]]@id
go.observedProperty = sosObservedProperties(sos)[[6]] # temp

go.responseFormat = sosResponseFormats(sos)[[1]] # temp
go.responseMode = sosResponseMode(sos)[[2]] # inline
go.eventTime = "2010-07-01T12:00/2010-07-10T12:00" 
go.resultModel = sosResultModels(sos)[[3]] # om:Measurement
go.srsName = "urn:ogc:def:crs:EPSG:6.8:4326" # is "AnyValue" in capabilities... contacted Carsten about that

# no event time -> latest
getObservation(sos, offering = go.offering,
		observedProperty =  go.observedProperty,
		responseFormat = go.responseFormat)





################################################################################
source("/home/daniel/Dropbox/2010_SOS4R/workspace/sos4R/sandbox/loadSources.R")

