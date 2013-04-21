# reproducible example for error with RCurl

# Fehler: Unhandled case for the value of curl_easy_setopt (R type = 19, option 10002)

library("RCurl")
library("sos4R")

mySOS <- SOS(url = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos")
mySOS@curlOptions
mySOS@curlHandle

response <- postForm(uri = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos",
				 request = '<sos:DescribeSensor xsi:schemaLocation="http://www.opengis.net/sos/1.0 http://schemas.opengis.net/sos/1.0.0/sosAll.xsd" service="SOS" outputFormat="text/xml;subtype=&quot;sensorML/1.0.1&quot;" version="1.0.0" xmlns:sos="http://www.opengis.net/sos/1.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
 <sos:procedure>urn:ogc:object:feature:OSIRIS-HWS:3d3b239f-7696-4864-9d07-15447eae2b93</sos:procedure>
</sos:DescribeSensor>',
										 style = "POST",
										 .opts = mySOS@curlOptions,
										 curl = mySOS@curlHandle,
				 .encoding = sosDefaultCharacterEncoding)
# works!

# TODO create an example only using RCurl...