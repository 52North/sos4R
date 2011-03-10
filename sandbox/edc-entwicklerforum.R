# ESRI Development Center (EDC) Entwickerforum Time & Space
# Münster, 17. - 18.02.2011
# 
# Author: Daniel Nüst
# sos4R Webseite (downloads, news): http://www.nordholmen.net/sos4r
###############################################################################

# Was muss ich wirklich wissen?
# Begriffe:
#	- offering
#	- procedure
#	- observedProperty
#	- feature of interest

##### Installation #############################################################
# Dependencies installieren (werden nicht aufgelöst wenn von Datei installiert 
# wird):
install.packages("sos4R")
# Package laden
library("sos4R")
# sessionInfo()

################################################################################
# Unterstützte und Standard-Features (werden hoffentlich in Zukunft mehr)
SosSupportedOperations()
SosSupportedServiceVersions()
SosSupportedConnectionMethods()
SosSupportedResponseFormats()
SosSupportedResponseModes()
SosSupportedResultModels()
SosSupportedSpatialOperators()
SosSupportedTemporalOperators()
SosDefaultConnectionMethod()
SosDataFieldConvertingFunctions()
SosDefaults()

##### Verbindung zu einem SOS erstellen ########################################
mySOS = SOS(url = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos")

# TIPP: Methoden beginnen mit 'sos...'
# > sos "TAB TAB" in Konsole
# sos "CTRL Space" in StatET
sosUrl(mySOS)
sosVersion(mySOS)
sosTimeFormat(mySOS)
sosMethod(mySOS)

##### Capabilities #############################################################
# http://v-swe.uni-muenster.de:8080/WeatherSOS/sos?service=SOS&request=GetCapabilities
# Wurden bereits runtergeladen:
sosCaps(mySOS)

##########################
# Capabilities Erforschen:
sosContents(mySOS)
sosFilter_Capabilities(mySOS)
sosServiceIdentification(mySOS)
sosServiceProvider(mySOS) # @serviceContact

##### Zugriffsfunktionen #######################################################
# Wichtige Elemente, da Grundlage für spätere Anfragen, sind über getter bzw. 
# accessor oder Zugriffsfunktionen abrufbar:
sosOfferings(mySOS)
sosOfferings(mySOS)[2:3]
sosOfferingIds(mySOS)

off.temp <- sosOfferings(mySOS)[["ATMOSPHERIC_TEMPERATURE"]]
sosId(off.temp)

sosProcedures(mySOS)
sosProcedures(off.temp)

sosObservedProperties(mySOS)
sosObservedProperties(off.temp)

sosBoundedBy(off.temp) # , convert = TRUE)

sosTime(mySOS)
off.temp.time <- sosTime(off.temp)
str(off.temp.time) # modelliert XML


##### Grafische Ausgabe der Offerings ##########################################
plot(offering)

# TODO hintergrundkarte erstellen


##### Sensor Metadaten abfragen ################################################
sensor2 <- describeSensor(mySOS, sosProcedures(off.temp)[[2]])
sensor2
str(sensor2)
sensor2@xml

sosId(sensor2)
sosCoordinates(sensor2)

#************#
# Aufgabe 02 #
#************#
# Wie ist die bounding box von sensor2?

# Wo ist sensor2?


##### Messungen abfragen #######################################################
# Einfachster Fall: "latest observation" für ein gesamtes offering
obs.temp.latest <- getObservation(sos = mySOS, offering = off.temp,
		latest = TRUE, inspect = TRUE)
# TIPP: "inspect" benutzen um die requests und responses kennen zu lernen!

##### Response erforschen ######################################################
obs.temp.latest
# TIPP: str(...) für Einblick unter die Motorhaube

# ObservationCollection ist auf vielfältige Art indizierbar:
length(obs.temp.latest)
obs.temp.latest[[1]]
obs.temp.latest[2:5]

# Koordinaten, Features und BoundingBox abfragen:
sosCoordinates(obs.temp.latest)
sosCoordinates(obs.temp.latest[[1]])
sosFeatureIds(obs.temp.latest)
sosBoundedBy(obs.temp.latest)

#************#
# Aufgabe 02 #
#************#
# Welche procedures, observedProperties und features sind in den erhaltenen
# Observations zu finden?
# TIPP: Zugriffsfunktionen für SOS (siehe oben) wiederverwenden.

sosObservedProperties(obs.temp.latest)


##### Daten erforschen #########################################################
# sosResult(...) ist die wichtigste Methode
sosResult(obs.temp.latest[[2]])
obs.temp.latest.result <- sosResult(obs.temp.latest[1:2])

# Nur ein ganz normaler data.frame ... Attribute enthalten Metadaten.
attributes(obs.temp.latest.result[["urn:ogc:def:property:OGC::Temperature"]])

# Kombination der results mit den Koordinaten
obs.temp.latest.coords <- sosCoordinates(obs.temp.latest)
obs.temp.latest.data <- merge(x = obs.temp.latest.result,
		y = obs.temp.latest.coords)
obs.temp.latest.data

# Geht viel einfacher!
obs.temp.latest.data <- sosResult(..., coordinates = TRUE)

#************#
# Aufgabe 02 #
#************#
# ...

##### Temporäre Ausschnitte ####################################################
# Erstellen der event time für GetObservation requests:
period.august09 <- sosCreateEventTimeList(
		sosCreateTimePeriod(sos = mySOS,
				begin = as.POSIXct("2009-08-01 00:00"),
				end = as.POSIXct("2009-08-31 12:00")))
#sosCreateTimeInstant()
#sosCreateTimePeriod()
str(period.august09)
#SosSupportedTemporalOperators()

obs.august09 <- getObservation(sos = mySOS,
		offering = off.temp,
		procedure = sosProcedures(off.temp),
		eventTime = period.august09)

obs.temp.august09.result <- sosResult(obs.august09)
summary(obs.temp.august09.result)
str(obs.temp.august09.result)
obs.temp.august09.result[100:103,]

#************#
# Aufgabe 02 #
#************#
# Wie ist die maximale, die minimale, und die Durchschnittstemperatur im
# September 2010? TIPP: ?summary


##### Thematische Ausschnitte ##################################################
sosObservedProperties(mySOS)

# TODO plot einer Stations über einen Monat

# TODO Histogramm

#************#
# Aufgabe 02 #
#************#
# Wie war die maximale Windstärke im September 2010 für alle verfügbaren Stationen? 


##### Räumliche Ausschnitte ####################################################
#SosSupportedSpatialOperators()
sosBoundedBy(off.temp)

request.bbox <- sosCreateBBOX(lowLat = 50.0, lowLon = 7.0,
		uppLat = 52.0, uppLon = 9.0, srsName = "urn:ogc:def:crs:EPSG:4326")
request.bbox.foi <- sosCreateFeatureOfInterest(spatialOps = request.bbox)
request.bbox.foi

obs.august09.bbox <- getObservation(sos = mySOS,
		offering = off.temp,
		featureOfInterest = request.bbox.foi,
		eventTime = period.august09)
obs.august09.bbox
sosCoordinates(obs.august09.bbox)

sosBoundedBy(obs.august09.bbox, convert = TRUE)


##### Air Quality SOS ##########################################################

#************#
# Aufgabe 02 #
#************#
# Erzeuge eine Verbindung zum SOS mit der URL http://giv-uw.uni-muenster.de:8080/AQE/sos

# Wann sind Daten des Offerings NO2 verfügbar?

# Welche Phänomene werden im SOS beobachtet?

# Wie viele procedures/Messstationen gibt es, die NO2-Werte liefern?

# Frage Daten für eine beliebige Woche ab und erzeuge einen data.frame.



##### Daten -> sp ##############################################################

spdf <- SpatioalPointsDataFrame(...)

spdf <- as(result, "SpatialPointsDataFrame")


#************#
# Aufgabe 02 #
#************#
# Wo sind die Messtationen?
plot(spdf)


##### Demos ####################################################################
demo(package = "sos4R")
# Demos laufen lassen (enhalten weiterführende Beispiele mit plots usw.):
demo("weathersos")
demo("pegel")
demo("airquality")


##### Fragen? ##################################################################
vignette("sos4R")
# Mailingliste: http://52north.org/resources/mailing-list-and-forums/
# 				Bitte den Posting-Guide beachten!
# Forum:		http://geostatistics.forum.52north.org/
# Kontakt:		d.nuest@52north.org

