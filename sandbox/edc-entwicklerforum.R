# ESRI Development Center (EDC) Entwickerforum Time & Space
# Münster, 17. - 18.02.2011
# 
# Author: Daniel Nüst
# sos4R Webseite (downloads, news): http://www.nordholmen.net/sos4r
###############################################################################

# SOS: Was muss ich wirklich wissen?
# Begriffe: offering, procedure, observedProperty, feature of interest
# Ablauf:	requestparameter sammeln, encoding, request senden, response parsen/
#	decoding.


##### Installation #############################################################
# Dependencies installieren (werden nicht aufgelöst wenn von Datei installiert 
# wird):
install.packages("sos4R")
# Package laden
library("sos4R")
# sessionInfo()


##### Unterstützte und Standard-Features #######################################
SosSupportedOperations() # jeweils eine entsprechende R Funktion
SosSupportedServiceVersions()
SosSupportedConnectionMethods()
SosSupportedResponseFormats()
SosSupportedResponseModes()
#SosSupportedResultModels()
SosSupportedSpatialOperators()
SosSupportedTemporalOperators()

SosDefaultConnectionMethod()
# SosDataFieldConvertingFunctions()
names(SosDataFieldConvertingFunctions())
SosDefaults()


##### Verbindung zu einem SOS erstellen ########################################
aqe <- SOS(url = "http://giv-uw.uni-muenster.de:8080/AQE/sos")

# TIPP: Methoden beginnen mit 'sos...'
# > sos "TAB TAB" in Konsole
# sos "CTRL Space" in StatET
sosUrl(aqe)
sosVersion(aqe)
sosMethod(aqe)

# Standard R Funktionen werden oft unterstützt:
print(aqe)
summary(aqe)


##### Capabilities #############################################################
# http://v-swe.uni-muenster.de:8080/WeatherSOS/sos?service=SOS&request=GetCapabilities
# Wurden bereits runtergeladen:
sosCaps(aqe)

# TIPP: str() Funktion, siehe ?str, insbesondere mit max.level
str(sosCaps(aqe), max.level = 2)

# Teile der Capabilities:
sosContents(aqe)

#************#
# Aufgabe 02 #
#************#
# Wie können die weiteren Sektionen der Capabilities abgefragt werden?
#
# Die Elemente sind ServiceIdentification, ServiceProvider, OperationsMetadata,
# Filter_Capabilities.

#----- sosFilter_Capabilities(aqe)
#----- sosServiceIdentification(aqe)
#----- sosServiceProvider(aqe)
#----- sosOperationsMetadata(aqe)


##### Zugriffsfunktionen #######################################################
# Weitere wichtige Elemente der Capabilities, sind über getter bzw.  accessor 
# abrufbar, da sie später zum erstellen der Requests benötigt werden:
sosOfferingIds(aqe) # == names(sosOffering(aqe))

sosOfferings(aqe)

# Indexierungsarten (generisch R):
sosOfferings(aqe)[2:3]			# Subliste
sosOfferings(aqe)[[3]]			# einzelnes Element
sosOfferings(aqe)[["PM10"]]	# über die ID 

pm10.off <- sosOfferings(aqe)[["PM10"]]
sosId(pm10.off)

sosProcedures(pm10.off)
sosObservedProperties(pm10.off)
sosBoundedBy(pm10.off, bbox = TRUE)
sosTime(pm10.off)
sosGetCRS(pm10.off)

#************#
# Aufgabe 02 #
#************#
# Welche der Funktionen aus diesem Abschnitt funktionieren auf für SOS-Objekte?
sosProcedures(aqe)

#----- sosTime(aqe) # benutzt GetObservation operation description
#----- sosObservedProperties(aqe)
#----- str(sosObservedProperties(aqe))
#----- sosProcedures(aqe)
#----- sosGetCRS(aqe)
#----- sosId(aqe) # GEHT NICHT! SOS hat keine ID.
#----- sosBoundedBy(aqe) # GEHT NICHT! BoundingBox ist Offering-spezifisch.

# Welches der Offerings hat die meisten Sensoren?

#----- lapply(X = sosProcedures(aqe), FUN = length)

##### Grafische Ausgabe von SOS und Offerings ##################################
plot(aqe) # nicht sehr hilfreich

# Hintergrundkarte erstellen -- muss nicht verstanden werden:
library(maps); library(mapdata); library(maptools)
data(worldHiresMapEnv)
crs <- unique(sosGetCRS(aqe))[[1]]
region <- map.where(database = "world",
		sosCoordinates(pm10.off)) # Finde region basierend auf einem offering.
map <- pruneMap(map(database = "world", region = region,
				plot = FALSE))
map.lines <- map2SpatialLines(map, proj4string = crs)

plot(map.lines, col = "grey50", lwd = 1)
plot(aqe, add = TRUE, lwd = 3)
title(main = paste("Offerings by '", sosTitle(aqe), "'", sep = ""),
		sub = paste(sosAbstract(aqe), " --- ",
				toString(names(sosOfferings(aqe)))))


##### Sensor Metadaten abfragen ################################################
sensor2 <- describeSensor(aqe, sosProcedures(pm10.off)[[2]])
sensor2
sensor2@xml

# Getter:
sosId(sensor2)

#************#
# Aufgabe 02 #
#************#
# Wie ist die Bounding Box von sensor2?

#----- sosBoundedBy(sensor2)

# Wo in Deutschland ist sensor2?

#----- sensor2.coords <- sosCoordinates(sensor2)
#----- plot(sensor2, add = TRUE, pch = 7, cex = 2)

##### Messungen abfragen #######################################################
# TIPP: "inspect" benutzen um die requests und responses kennen zu lernen!

##### Temporäre Ausschnitte ####################################################
# 1 Stunde im Dezember, alle Daten zu viel wegen parallelen Anfragen!
# TIPP: sosCreate... - Funktionen helfen korrekte Strukturen zu erzeugen:
dec2003.1Hrs = sosCreateEventTimeList(sosCreateTimePeriod(sos = aqe,
				begin = as.POSIXct("2003/12/01 08:00"),
				end = as.POSIXct("2003/12/02 09:00")))

# Daten abrufen:
dec2003.obs <- getObservation(sos = aqe, # inspect = TRUE,
		offering = pm10.off, eventTime = dec2003.1Hrs)
warnings()

# PROBLEM: Nicht unterstützte Datenfelder werden abgefragt! Konverter ergänzen:
aqe.converters <- SosDataFieldConvertingFunctions(
		"http://giv-genesis.uni-muenster.de:8080/SOR/REST/phenomenon/OGC/Concentration[PM10]" = sosConvertDouble,
		"http://giv-genesis.uni-muenster.de:8080/SOR/REST/phenomenon/OGC/Concentration[NO2]" = sosConvertDouble,
		"http://giv-genesis.uni-muenster.de:8080/SOR/REST/phenomenon/OGC/Concentration[O3]" = sosConvertDouble,
		"http://www.opengis.net/def/property/OGC/0/SamplingTime" = sosConvertTime,
		"http://www.opengis.net/def/property/OGC/0/FeatureOfInterest" = sosConvertString)

# Neue Verbindung erstellen, dieses mal mit Konvertern:
aqe <- SOS(sosUrl(aqe), dataFieldConverters = aqe.converters)

# Nochmaliges daten abrufen, diesmal alternativ auf Basis der offering ID:
# Daten für ganz Deutschland!
dec2003.obs <- getObservation(sos = aqe, offering = "NO2",
		saveOriginal = TRUE,
		eventTime = dec2003.1Hrs)


##### Response erforschen ######################################################
print(dec2003.obs)

# ObservationCollection ist auf vielfältige Art indizierbar:
length(dec2003.obs)
dec2003.obs[[1]]
dec2003.obs[4:5]

# Koordinaten, Features und BoundingBox abfragen:
sosCoordinates(dec2003.obs[[1]])
sosCoordinates(dec2003.obs)[1:3,]
sosFeatureIds(dec2003.obs)[42:44]
sosBoundedBy(dec2003.obs, bbox = TRUE)

#************#
# Aufgabe 02 #
#************#
# Welche procedures, observedProperties und features sind in den erhaltenen
# Observations zu finden? TIPP: Zugriffsfunktionen!

#----- sosProcedures(dec2003.obs)
#----- sosObservedProperties(dec2003.obs[[1]])


##### Tatsächliche Daten erforschen ############################################
# sosResult(...) ist die wichtigste Methode:
sosResult(dec2003.obs[[1]])
sosResult(dec2003.obs[100:101])
sosResult(dec2003.obs)[100:101,]

dec2003.result <- sosResult(dec2003.obs)

# Nur ein ganz normaler data.frame ... Attribute enthalten Metadaten.
names(dec2003.result)
attributes(dec2003.result[["Concentration[NO2]"]])

# Kombination der results mit den Koordinaten
dec2003.coords <- sosCoordinates(dec2003.obs)[1:5,]
merge(x = dec2003.result, y = dec2003.coords)[1:3,]

# Geht viel einfacher!
dec2003.data <- sosResult(dec2003.obs, coordinates = TRUE)

#************#
# Aufgabe 02 #
#************#
# Was ist der maximale/minimale, und der Durchschnittswert von NO2?

#----- summary(dec2003.data)


##### Thematische Ausschnitte ##################################################

# Ein Phänomen (auch wenn es sowieso nur eines ist) für eine Station, ganzes Jahr:
no2.off <- sosOfferings(aqe)[["NO2"]]
time.2007 = sosCreateEventTimeList(sosCreateTimePeriod(sos = aqe,
				begin = as.POSIXct("2007/01/01"),
				end = as.POSIXct("2007/12/31")))

sosProcedures(no2.off)

# Beachte das Benennungsschema: "DE" "NW"
# Stationsübersicht: http://www.eea.europa.eu/themes/air/airbase/map-stations
# Station auswählen -> Meta Data
myStationID <- "DEBE044"
idx <- grep(pattern = myStationID, x = sosProcedures(pm10.off))
myStation <- sosProcedures(pm10.off)[idx]

obs.myStation.2007 <- getObservation(sos = aqe, offering = pm10.off, # inspect = TRUE,
#		observedProperty = sosObservedProperties(no2.off),
		procedure = myStation,
		eventTime = time.2007)

result.myStation.2005 <- sosResult(obs.myStation.2005)
summary(result.myStation.2005)

# Zeitreihenplot:
plot(data.myStation.2005[["SamplingTime"]], data.denw095.2004[[NO2]], type = "l",
		main = paste("NO2 in", denw095.id, "2004"), sub = denw095,
		xlab = "Time",
		ylab = paste("NO2 (",
				denw095.NO2.attributes[["unit of measurement"]],
				")", sep = ""))
data.denw095.2004.locRegr = loess(data.denw095.2004[[NO2]]~as.numeric(data.denw095.2004[["SamplingTime"]]),
		data.denw095.2004, enp.target = 30)
p = predict(data.denw095.2004.locRegr)
lines(p ~ data.denw095.2004[["SamplingTime"]], col = 'blue',lwd = 4)


# Histogramm:
hist(result.myStation.2005[[""]])

#************#
# Aufgabe 02 #
#************#
# Wie war der maximale Wert von 03 im März 2005 für eine Station in der Nähe 
# deiner Heimatstadt?


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


#************#
# Aufgabe 02 #
#************#
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

