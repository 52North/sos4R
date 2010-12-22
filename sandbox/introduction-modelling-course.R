# V+Ü Einführung Modellierung dynamischer räumlicher Prozesse, WS 2010/2011
# 
# Author: Daniel Nüst
###############################################################################

# Was ist ein SOS?
# - OGC Standard für Onlinespeicherung von Sensormessungen:
#	http://www.opengeospatial.org/standards/sos
#	- Gute Übersicht: http://de.wikipedia.org/wiki/Sensor_Observation_Service
#	- Begriffe: FOI, Observation, Offering, Phenomenon, Procedure, In-Situ
#	- Verbindungsarten: HTTP GET, HTTP POST, SOAP
#	- Dokumentenkodierung: XML, SensorML, GML, O&M, SweCommon, OwsCommon, ...

# Warum muss ich mich damit beschäftigen?
# - SWSL: http://swsl.uni-muenster.de
# - Standards = Interoperabilität
#	(semantische ~ --> http://musil.uni-muenster.de/)

# Kann ich mit R nicht Daten einfach so runterladen?
library("RCurl")
temp <- getURL("ifgi.uni-muenster.de")
cat(temp)
temp <- getURL("http://giv-sos.uni-muenster.de:8080/52nSOSv3/sos?request=GetCapabilities&version=1.0.0&service=SOS")
print(temp)
cat(temp)
# CSV Datei runterladen und importieren
download.file(url = "http://geography.uoregon.edu/geogr/data/csv/cities.csv",
		destfile = "cities.csv")
cities <- read.csv(paste(getwd(), "/cities.csv", sep = ""))
# Eine einfache Tabelle - kein Problem!

#  10080 000     SVALBARD LUHTHAVN                   78.3n   15.5e    29m  temperature in degrees centigrade (  99.0 is missing)   
#  10080 001  par    jan    feb    mar    apr    may    jun    jul    aug    sep    oct    nov    dec    djf    mam    jja    son    ann    
#  10080 1977   3   99.0   99.0   99.0   99.0   99.0   99.0   99.0   99.0   99.0   -4.1   -6.9   -9.5   99.0   99.0   99.0   99.0   99.0
#  10080 1978   3  -20.7  -20.4  -17.7   -9.9   -3.7    1.7    6.0    5.0   -1.8   -5.8  -10.9  -12.4  -16.9  -10.4    4.2   -6.2   -7.6
#  10080 1979   3  -20.5  -20.6  -16.0  -17.4   -8.0   -0.5   99.0    4.8    1.0   -4.3   99.0  -12.7  -17.8  -13.8   99.0   99.0   99.0
# [...]  10080 2003   3  -18.6  -12.3  -17.3   -9.6   -2.5    2.8    7.0    6.4    0.6   -4.3   -6.7  -19.1  -12.8   -9.8    5.4   -3.5   -6.1
#  10080 2003   3  -18.6  -12.3  -17.3   -9.6   -2.5    2.8    7.0    6.4    0.6   -4.3   -6.7  -19.1  -12.8   -9.8    5.4   -3.5   -6.1
#  10080 2004   3  -17.0  -17.5   -7.7   -4.3   -1.8    3.0    7.6    5.7    1.3   -3.6  -11.5   -6.6  -17.9   -4.6    5.4   -4.6   -4.4
#  10080 991 #yrs    27.    27.    27.    27.    27.    27.    26.    27.    27.    28.    27.    28.    27.    27.    26.    26.    26.
#  10080 992 mean  -14.7  -15.0  -13.6  -10.7   -3.5    2.5    6.3    5.2    0.8   -4.7   -9.1  -12.5  -14.2   -9.3    4.7   -4.4   -5.7
#  10080 993 stdv    4.4    3.8    3.7    3.3    1.3    1.0    0.9    0.9    1.7    3.1    3.8    4.2    3.0    2.1    0.7    2.0    1.3
# Format?!?
# http://dss.ucar.edu/datasets/common/wmssc/format.html

# --> Vorteile vom SOS ausnutzen: Wohldefiniertes Markup, Queries

# Was ist sos4R?
# - Webseite (downloads, news): http://www.nordholmen.net/sos4R
# - Funktionalität
# 	- Core Profile (plug GetObservationById)
#		- GetCapabilities
#		- GetObservation
#		- DescribeSensor
#	- GET und POST
#	- Austauschbarkeit

##### Installation #############################################################
# Package herunterladen
pkgName = "sos4R_0.1-07.tar.gz"
download.file(url = paste(
				"http://www.nordholmen.net/sos4r/download/", pkgName, sep =""),
		destfile = pkgName)
#download.file(url = "http://ifgi.uni-muenster.de/~d_nues01/pub/sos4R_0.1-07.tar.gz",
#		destfile = pkgName)
# Package installieren
# Dependencies werden nicht aufgelöst wenn von Datei installiert wird:
install.packages("RCurl")
install.packages("XML")
install.packages(paste(getwd(), "/", pkgName, sep = ""))

# Package laden
library("sos4R")
# Unterstützte Features (werden hoffentlich in Zukunft mehr)
SosSupportedServiceVersions()
SosSupportedConnectionMethods()
SosSupportedResponseFormats()
SosSupportedResponseModes()
SosSupportedResultModels()
SosSupportedSpatialOperators()
SosSupportedTemporalOperators()

##### Verbindung zu einem SOS erstellen ########################################
sos = SOS(url = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos")

# TIPP: Methoden beginnen mit 'sos...'
# > sos "TAB TAB" in Konsole
# sos "CTRL Space" in StatET
sosUrl(sos)
sosVersion(sos)
sosTimeFormat(sos)

##### Capabilities abfragen ####################################################
# Wurden bereits runtergeladen:
sosCaps(sos)
# Originaldokument:
sosCapabilitiesDocumentOriginal(sos)

# Capabilities Erforschen:
sosContents(sos)
sosFilter_Capabilities(sos)
sosServiceIdentification(sos)
sosServiceProvider(sos) # @serviceContact

sosOfferings(sos)
off1 <- sosOfferings(sos)[[1]]
sosOfferingIds(sos)

sosProcedures(sos)
sosProcedures(off1)

sosObservedProperties(sos)
sosObservedProperties(off1)

sosBoundedBy(off1)
str(sosBoundedBy(off1)) # Nicht so schön ...

sosTime(sos)
sosTime(off1)


##### Sensor Metadaten abfragen ################################################
describeSensor(sos, "lala")
# Warnung! Und: Antwort ist OwsExceptionReport!

# TIPP: verbose option in service operation functions
describeSensor(sos, "lala", verbose = TRUE)

sensor2 <- describeSensor(sos, sosProcedures(off1)[[2]])
sensor2


##### Messungen abfragen #######################################################

sosFeaturesOfInterest()
sosFeatureIds
sosMethod(sos)()
sosCoordinates()
sosId()
sosSrsName()


##### Daten erforschen #########################################################
sosResult()
# Funktionen zur Kombination in einzelnen data.frame

# Nur ein ganz normaler data.frame ...
attributes()



##### Temporäre Ausschnitte ####################################################
#SosSupportedTemporalOperators()

sosCreateEventTimeList()
sosCreateTimeInstant()
sosCreateTimePeriod()

##### Räumliche Ausschnitte ####################################################
#SosSupportedSpatialOperators()

sosCreateBBOX()
sosCreateBBoxMatrix()
sosCreateFeatureOfInterest()

##### Subsetting ###############################################################

sosCreateFeatureOfInterest()

##### Daten -> sp ##############################################################


##### Demos ####################################################################
demo(package = "sos4R")
# Demos laufen lassen:
demo("weathersos")
demo("pegel")
demo("airquality")
demo("marinemeta")


##### Fragen? ##################################################################
# Mailingliste: http://52north.org/resources/mailing-list-and-forums/ (guide!)
# Forum: http://geostatistics.forum.52north.org/
# Kontakt: daniel.nuest@uni-muenster.de

##### Mach mit! ################################################################
# Entwicklerdokumentation und To-Do-Liste:
# https://wiki.52north.org/bin/view/Geostatistics/Sos4R#To_Do_List