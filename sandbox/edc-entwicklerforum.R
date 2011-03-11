# ESRI Development Center (EDC) Entwickerforum Time & Space
# Münster, 17. - 18.02.2011
# 
# Author: Daniel Nüst
# sos4R Webseite (downloads, news): http://www.nordholmen.net/sos4r
################################################################################

# SOS ##########################################################################
# Was muss ich wirklich wissen?
# Begriffe: offering, procedure, observedProperty, feature of interest
# Ablauf:	requestparameter sammeln, encoding, request senden, response parsen/
#	decoding.

# Workshop #####################################################################
# Skript öffnen in RGui, dann mit Strg + R aktuelle Zeile oder ausgewählten
# Code ausführen.


##### Installation #############################################################
# Dependencies installieren (werden nicht aufgelöst wenn von Datei installiert 
# wird):
#install.packages("sos4R")
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
# Aufgabe 01 #
#************#
# Wie können die weiteren Sektionen der Capabilities abgefragt werden?
# Die Elemente sind ServiceIdentification, ServiceProvider, OperationsMetadata,
# Filter_Capabilities.

#++ sosFilter_Capabilities(aqe)
#++ sosServiceIdentification(aqe)
#++ sosServiceProvider(aqe)
#++ sosOperationsMetadata(aqe)
#++ str(sosOperationsMetadata(aqe))
#++ sosOperation(aqe, "GetObservation")

# Welche Information sind dort zu finden?
# Wie viel kostet die Benutzung diese SOS?
# Wer ist verantwortliche Kontaktperson?


##### Zugriffsfunktionen #######################################################
# Weitere wichtige Elemente der Capabilities, sind über getter bzw.  accessor 
# abrufbar, da sie später zum erstellen der Requests benötigt werden:
sosOfferingIds(aqe) # == names(sosOffering(aqe))

sosOfferings(aqe) # > print(sosOfferings(aqe))

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
# Welche der Funktionen aus diesem Abschnitt funktionieren auch für SOS-Objekte?
# Was gibt es dabei zu beachten?

#++ sosTime(aqe) # benutzt GetObservation operation description
#++ sosObservedProperties(aqe)
#++ str(sosObservedProperties(aqe))
#++ sosProcedures(aqe)
#++ sosGetCRS(aqe)

#++ sosId(aqe) # GEHT NICHT! SOS hat keine ID.
#++ sosBoundedBy(aqe) # GEHT NICHT! BoundingBox ist Offering-spezifisch.

# Welches der Offerings hat die meisten Sensoren?
# TIPP: ?length

#++ lapply(X = sosProcedures(aqe), FUN = length)
#++ lapply(X = sosProcedures(sosOfferings(aqe)), FUN = length)

#++ procedures <- sosProcedures(sosOfferings(aqe))
#++ length(procedures[[1]])
#++ length(procedures[[2]])
#++ length(procedures[[3]])

#++ length(sosProcedures(sosOfferings(aqe)[[1]]))
#++ length(sosProcedures(sosOfferings(aqe)[[2]]))
#++ length(sosProcedures(sosOfferings(aqe)[[3]]))


##### Grafische Ausgabe von SOS und Offerings ##################################
plot(pm10.off) # alleinstehend nicht sehr hilfreich

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
plot(pm10.off, add = TRUE, lwd = 3)
title(main = paste("Offering '", sosId(pm10.off), "' at", sosTitle(aqe),
				sep = ""),
		sub = paste(sosAbstract(aqe), " --- ",
				toString(names(sosOfferings(aqe)))))

#************#
# Aufgabe 03 #
#************#
# Wie können alle Offerings geplottet werden?

#++ plot(map.lines, col = "grey50", lwd = 1)
#++ plot(sosOfferings(aqe)[[1]], add = TRUE, lwd = 3, border = "green")
#++ plot(sosOfferings(aqe)[[2]], add = TRUE, lwd = 3, border = "blue")
#++ plot(sosOfferings(aqe)[[3]], add = TRUE, lwd = 3, border = "red")

#++ plot(map.lines, col = "grey50", lwd = 1)
#++ plot(aqe, add = TRUE, lwd = 2)

# TIPP: ?text

##### Sensor Metadaten abfragen ################################################
sensor2 <- describeSensor(aqe, sosProcedures(pm10.off)[[2]])
sensor2		
sensor2@xml # Viel Information, einzelne Details über Getter abfragbar, dies
			# verlangt aber Konformität mit # SensorML Profile for Discovery

# Getter:
sosId(sensor2)

#************#
# Aufgabe 04 #
#************#
# Wie ist die Bounding Box von sensor2?

#++ sosBoundedBy(sensor2)

# Wo in Deutschland (Koordinaten und/oder Plot) ist 'sensor2'?

#++ sosCoordinates(sensor2)
#++ plot(sensor2, add = TRUE, pch = 7, cex = 2)


##### Messungen abfragen #######################################################
# TIPP: "inspect" benutzen um die requests und responses kennen zu lernen!

##### Temporäre Ausschnitte ####################################################
# 1 Stunde im Dezember, alle Daten zu viel wegen parallelen Anfragen!
# TIPP: sosCreate... - Funktionen helfen korrekte Strukturen zu erzeugen:
dec2003.1Hrs = sosCreateEventTimeList(sosCreateTimePeriod(sos = aqe,
				begin = as.POSIXct("2003/12/01 08:00"),
				end = as.POSIXct("2003/12/02 09:00")))

# Daten abrufen, nur zeitlicher Auschnitt, Gesamtdeutschland:
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
dec2003.obs <- getObservation(sos = aqe, offering = "NO2", # verbose = TRUE,
#		saveOriginal = TRUE,
		eventTime = dec2003.1Hrs)
# Parsing dauert länger als request -> wenig Daten in vielen Observations, hoher
# XML overhead.

# Nur einige Stationen, mit 'inspect' Parameter
dec2003.obs.small <- getObservation(sos = aqe, offering = "NO2",# verbose = TRUE,
		inspect = TRUE,
		procedure = sosProcedures(aqe)[[1]][1:10],
		eventTime = dec2003.1Hrs)

##### Response erforschen ######################################################
print(dec2003.obs.small)
str(dec2003.obs.small, max.level = 5) # limitieren der Strukturtiefe

# ObservationCollection ist auf vielfältige Art indizierbar:
dec2003.obs[[1]]
dec2003.obs[355:length(dec2003.obs)]
dec2003.obs[c(17,42)]

# Koordinaten, Features und BoundingBox abfragen:
sosCoordinates(dec2003.obs[[1]])
sosCoordinates(dec2003.obs[[1]])
sosFeatureIds(dec2003.obs)[42:44]
sosBoundedBy(dec2003.obs, bbox = TRUE)

#************#
# Aufgabe 05 #
#************#
# Welche procedures, observedProperties und features sind in den erhaltenen
# Observations zu finden? TIPP: Zugriffsfunktionen!

#++ sosProcedures(dec2003.obs)
#++ sosObservedProperties(dec2003.obs[[1]])

# Wie können die Koordinaten für die Observations 10 bis 20 abgefragt werden?

sosCoordinates(dec2003.obs[10:20])

# Was ist der Unterschied zwischen sosFeatureIds(dec2003.obs)[42:44] und 
# sosFeatureIds(dec2003.obs[42:44])

#++ Fragt alle feature ids ab und nimmt von dieser zusammengefügten Liste
#++ die Elemente 42 bis 44:
#++ str(sosFeatureIds(dec2003.obs)[42:44]) # List of 3 character
#++ Frage observations 42:44 ab und ruft auf dieser Liste (!) für jedes Element
#++ die Funktion sosFeatureIds(...) auf.
#++ str(sosFeatureIds(dec2003.obs[42:44])) # List of 3 Lists


##### Tatsächliche Daten erforschen ############################################
# sosResult(...) ist die wichtigste Methode:
sosResult(dec2003.obs[[1]])

# Testplot
plot(sosResult(dec2003.obs[[1]]))

# Was ist die Struktur des Results?
class(sosResult(dec2003.obs[[1]]))

?data.frame

# Wie kann ich die Daten verschiedene Stationen zusammenfügen?
sosResult(dec2003.obs[100:101])		# Result der 100. und 101. Observations
sosResult(dec2003.obs)[100:101,]	# Zeile 100, 101 von allen (!) Daten

dec2003.result <- sosResult(dec2003.obs)

# Warum XML parsen, wenn sowieso nur CSV-Werte zur einer "Tabelle" geparst
# werden? Attribute enthalten Metadaten!
names(dec2003.result)
attributes(dec2003.result[["Concentration[NO2]"]])

# Kombination der results mit den Koordinaten
dec2003.coords <- sosCoordinates(dec2003.obs)[1:5,]
merge(x = dec2003.result, y = dec2003.coords)[1:3,]

# Geht viel einfacher!
dec2003.data <- sosResult(dec2003.obs, coordinates = TRUE)

#************#
# Aufgabe 06 #
#************#
# Was sind der maximale/minimale, der Durchschnittswert, der Median und die
# Quantile von NO2 für alle heruntergeladenen Daten?

#++ summary(dec2003.data)
#++ summary(dec2003.data[["Concentration[NO2]"]])

#++ max(dec2003.data[["Concentration[NO2]"]])
#++ min(dec2003.data[["Concentration[NO2]"]])
#++ mean(dec2003.data[["Concentration[NO2]"]])
#++ median(dec2003.data[["Concentration[NO2]"]])
#++ quantile(dec2003.data[["Concentration[NO2]"]])


# Wie ist das Phänomen Concentration[NO2] definiert?

#++ "Amount of nitrogen dioxide (NO2) as a fraction of host medium"
#++ Browser: http://giv-genesis.uni-muenster.de:8080/SOR/REST/phenomenon/OGC/Concentration[NO2]
#++ Nur um zu zeigen dass Erweitern mit Paket XML nicht so schlimm ist:
#++ definition <- getURL("http://giv-genesis.uni-muenster.de:8080/SOR/REST/phenomenon/OGC/Concentration[NO2]")
#++ definition.xml <- xmlParse(description)
#++ getNodeSet(doc = definition.xml, path = "//gml:description/text()")[[1]]


##### Thematische Ausschnitte ##################################################

# Ein Phänomen (auch wenn es sowieso nur eines ist) für eine Station, ein Jahr:
no2.off <- sosOfferings(aqe)[["NO2"]]
time.2007 = sosCreateEventTimeList(sosCreateTimePeriod(sos = aqe,
				begin = as.POSIXct("2007/01/01"),
				end = as.POSIXct("2007/12/31")))

sosProcedures(no2.off)
# Beachte das Benennungsschema: "DE" "NW"
# Stationsübersicht: http://www.eea.europa.eu/themes/air/airbase/map-stations
# Station auswählen -> Meta Data
myStationID <- "DESH024"
idx <- grep(pattern = myStationID, x = sosProcedures(no2.off))
myStation <- sosProcedures(pm10.off)[idx]

# Filtern mit observed property nicht notwendig, da sowieso ein Offering nur 
# ein Phänomen liefert.
obs.myStation.2007 <- getObservation(sos = aqe, offering = no2.off, # inspect = TRUE,
#		observedProperty = sosObservedProperties(no2.off),
		procedure = myStation,
		eventTime = time.2007)

result.myStation.2007 <- sosResult(obs.myStation.2007)
summary(result.myStation.2007)

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
# Aufgabe 07 #
#************#
# Wie war der maximale Wert von 03 im März 2005 für eine Station in der Nähe 
# deiner Heimatstadt?



##### Räumliche Ausschnitte ####################################################
#SosSupportedSpatialOperators()
sosBoundedBy(off.temp)

request.bbox <- sosCreateBBOX(lowLat = 49.84, lowLon = 5.98,
		uppLat = 52.12, uppLon = 10.15, srsName = "urn:ogc:def:crs:EPSG:4326")
request.bbox.foi <- sosCreateFeatureOfInterest(spatialOps = request.bbox)
request.bbox.foi

# Alle PM10 Daten von Januar 2007 in Bounding Box
obs.2007.bbox <- getObservation(sos = aqe, # inspect = TRUE,
		offering = pm10.off,
		featureOfInterest = request.bbox.foi,
		eventTime = sosCreateEventTimeList(sosCreateTimePeriod(sos = aqe,
						begin = as.POSIXct("2007/01/01"),
						end = as.POSIXct("2007/01/31"))))
obs.2007.bbox
sosBoundedBy(obs.2007.bbox, convert = TRUE)

bbox.coords <- sosCoordinates(obs.2007.bbox)


#************#
# Aufgabe 08 #
#************#
# Wann und wo sind Daten des Offerings PM10 verfügbar?

# Wann und wo sind Daten in den abgefragten Observations verfügbar?

# Wie viele Messstationen gibt es in der bounding box, die PM10-Werte liefern?


##### Daten -> sp ##############################################################
result.2007.bbox <- sosResult(obs.2007.bbox)
obs.crs <- sosGetCRS(obs.2007.bbox)

pm10.spdf <- SpatialPointsDataFrame(
		coords = result.2007.bbox[,c("lon", "lat")],
		data = result.2007.bbox[,
				c("SamplingTime", "feature", "Concentration[NO2]")],
		proj4string = obs.crs)
summary(pm10.spdf)

as(obs.2007.bbox[[1]], "SpatialPointsDataFrame")
spdf <- as(obs.2007.bbox, "SpatialPointsDataFrame")


#************#
# Aufgabe 09 #
#************#
# Wo sind die Messtationen?

coordinates(spdf)
plot(map.lines, col = "grey50", lwd = 1)
plot(spdf)

# Frage Daten für eine beliebige Woche ab und erzeuge einen data.frame.


##### Demos ####################################################################
demo(package = "sos4R")
# Demos laufen lassen (enhalten weiterführende Beispiele mit plots usw.):
demo("weathersos")
demo("pegel")
demo("airquality")


##### Fragen? ##################################################################
vignette("sos4R")
sosCheatSheet()
# Mailingliste: http://52north.org/resources/mailing-list-and-forums/
# 				Bitte den Posting-Guide beachten!
# Forum:		http://geostatistics.forum.52north.org/
# Kontakt:		d.nuest@52north.org

