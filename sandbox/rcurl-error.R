# reproducible example for error with RCurl

# ERROR: Unhandled case for the value of curl_easy_setopt (R type = 19, option 10002)

###############################################################################
# The problem:
library("sos4R")
mySOS <- SOS(url = "http://141.30.100.135:8080/eo2heavenSOS/sos")
mySOS@curlOptions
mySOS@curlHandle

sosCapabilitiesDocumentOriginal(sos = mySOS)
# does NOT work

###############################################################################
# not repeatable if done manually...
getCapRequest <- '<sos:GetCapabilities xsi:schemaLocation="http://www.opengis.net/sos/1.0 http://schemas.opengis.net/sos/1.0.0/sosAll.xsd" service="SOS" xmlns:sos="http://www.opengis.net/sos/1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ows="http://www.opengis.net/ows/1.1" xmlns:ogc="http://www.opengis.net/ogc">
 <ows:AcceptVersions>
  <ows:Version>1.0.0</ows:Version>
 </ows:AcceptVersions>
 <ows:Sections>
  <ows:Section>All</ows:Section>
 </ows:Sections>
 <ows:AcceptFormats>
 <ows:OutputFormat>text/xml</ows:OutputFormat>
 </ows:AcceptFormats>
</sos:GetCapabilities>'

postForm(uri = "http://141.30.100.135:8080/eo2heavenSOS/sos", request = getCapRequest, style = "post",
				 .opts = mySOS@curlOptions, curl = mySOS@curlHandle)
# works!

response <- postForm(uri = "http://141.30.100.135:8080/eo2heavenSOS/sos",
				 request = '<sos:DescribeSensor xsi:schemaLocation="http://www.opengis.net/sos/1.0 http://schemas.opengis.net/sos/1.0.0/sosAll.xsd" service="SOS" outputFormat="text/xml;subtype=&quot;sensorML/1.0.1&quot;" version="1.0.0" xmlns:sos="http://www.opengis.net/sos/1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
 <sos:procedure>http://www.eo2heaven.org/feature/sensor/germany/saxony/meterological_station/PM10-DESN014</sos:procedure>
</sos:DescribeSensor>',
										 style = "POST",
										 .opts = mySOS@curlOptions,
										 curl = mySOS@curlHandle,
				 .encoding = sosDefaultCharacterEncoding)
# works!

###############################################################################
# an example only using RCurl...

.gc <- "http://141.30.100.135:8080/eo2heavenSOS/sos?request=GetCapabilities&service=SOS"
.responseString = getURL(url = .gc)
.responseString
.response <- xml2::read_xml(x = .responseString)
# WORKS

.responseString = getURL(url = .gc,
		.opts = mySOS2@curlOptions,
		curl = mySOS2@curlHandle,
		.encoding = sosDefaultCharacterEncoding)
.response <- xml2::read_xml(x = .responseString)
.response
# works.
