# ESRI Development Center (EDC) Entwickerforum Time & Space
# Münster, 17. - 18.02.2011
# 
# Author: Daniel Nüst
###############################################################################

# Was muss ich wirklich wissen?
# Begriffe:
#	- offering
#	- procedure
#	- observedProperty
#	- feature of interest

# Was ist sos4R?
# - Webseite (downloads, news): http://www.nordholmen.net/sos4r
# - Funktionalität
# 	- Core Profile (plug GetObservationById)
#		- GetCapabilities
#		- GetObservation
#		- DescribeSensor
#	- GET und POST
#	- Austauschbarkeit!

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

#######################################################
# Wichtige Elemente, da Grundlage für spätere Anfragen:
sosOfferings(mySOS)
off.temp <- sosOfferings(mySOS)[["ATMOSPHERIC_TEMPERATURE"]]
sosOfferingIds(mySOS)
names(sosOfferings(mySOS))

sosId(off.temp)
sosOfferings(mySOS)[1:3]

sosProcedures(mySOS)
sosProcedures(off.temp)

sosObservedProperties(mySOS)
sosObservedProperties(off.temp)

sosBoundedBy(off.temp)
str(sosBoundedBy(off.temp)) # Nicht so schön ...

sosTime(mySOS)
off.temp.time <- sosTime(off.temp)
str(off.temp.time) # modelliert XML
# "wirklichen" Startzeitpunkt abfragen
off.temp.time@beginPosition@time
class(off.temp.time@beginPosition@time)

##### Sensor Metadaten abfragen ################################################
describeSensor(sos = mySOS, procedure = "meinTollerSensor")
# Warnung! Und: Antwort ist OwsExceptionReport!

# TIPP: verbose option in service operation functions
describeSensor(sos = mySOS, procedure = "lala", verbose = TRUE)

sensor2 <- describeSensor(mySOS, sosProcedures(off.temp)[[2]])
sensor2
str(sensor2)
sensor2@xml

##### Messungen abfragen #######################################################
# Einfachster Fall: "latest observation" für ein gesamtes offerings
obs.temp.latest <- getObservation(sos = mySOS, offering = off.temp,
		latest = TRUE, inspect = TRUE)
# TIPP: "inspect" benutzen um die requests und responses kennen zu lernen!

##### Response erforschen ######################################################
# print Methoden
obs.temp.latest
# TIPP: str(...) für Einblick unter die Motorhaube

# ObservationCollection behaves like a list in most cases
length(obs.temp.latest)
obs.temp.latest[[1]]
obs.temp.latest[2:5]

# Koordinaten, Features und BoundingBox abfragen
sosCoordinates(obs.temp.latest)
sosCoordinates(obs.temp.latest[[1]])
sosFeatureIds(obs.temp.latest)
sosBoundedBy(obs.temp.latest)


##### Daten erforschen #########################################################
# sosResult(...) ist die wichtigste Methode
sosResult(obs.temp.latest[[2]])
obs.temp.latest.result <- sosResult(obs.temp.latest[1:2])

# Nur ein ganz normaler data.frame ... Attribute enthalten Metadaten. Diese 
# gehen nach dem "merge" verloren!
attributes(obs.temp.latest.result[["urn:ogc:def:property:OGC::Temperature"]])

# Kombination der results mit den Koordinaten
obs.temp.latest.coords <- sosCoordinates(obs.temp.latest)
obs.temp.latest.data <- merge(x = obs.temp.latest.result,
		y = obs.temp.latest.coords)
obs.temp.latest.data

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

##### Räumliche Ausschnitte ####################################################
#SosSupportedSpatialOperators()

sosBoundedBy(off.temp)
#request.bbox <- sosCreateBBOX(lowLat = 10.0, lowLon = 2.0,
#		uppLat = 30.0, uppLon = 5.0, srsName = "urn:ogc:def:crs:EPSG:4326")
request.bbox <- sosCreateBBOX(lowLat = 50.0, lowLon = 7.0,
		uppLat = 52.0, uppLon = 9.0, srsName = "urn:ogc:def:crs:EPSG:4326")
request.bbox.foi <- sosCreateFeatureOfInterest(spatialOps = request.bbox)
request.bbox.foi

obs.august09.bbox <- getObservation(sos = mySOS,
		offering = off.temp,
		featureOfInterest = request.bbox.foi,
		eventTime = period.august09,
		inspect = TRUE)
obs.august09.bbox
sosCoordinates(obs.august09.bbox)

# Beliebige räumliche Filter über FOI sind auch möglich
# -> siehe SOS Spec.
# -> siehe sos4R Code

##### Fortgeschrittenes Filtern ################################################
# Mit feature of interest:
off.temp.fois <- sosFeaturesOfInterest(off.temp)
request.fois <- sosCreateFeatureOfInterest(objectIDs = list(off.temp.fois[[1]]))

obs.august09.fois <- getObservation(sos = mySOS, offering = off.temp,
		featureOfInterest = request.fois,
		eventTime = period.august09)
obs.august09.fois
# Object of class OmObservationCollection with 1 members. 

# Weitere Filter sind derzeit nicht implementiert...

##### Neue Konverter ###########################################################
# GET Verbindung
MBARI <- SOS("http://mmisw.org/oostethys/sos",
		method = SosSupportedConnectionMethods()[["GET"]])
myOff <- sosOfferings(MBARI)[[1]]
myProc <- sosProcedures(MBARI)[[1]]
mbariObs <- getObservation(sos = MBARI, offering = myOff, procedure = myProc,
		inspect = TRUE)
# Warnmeldungen!

?SosDataFieldConvertingFunctions

# So geht es:
myConverters <- SosDataFieldConvertingFunctions(
		# mapping for UOM:
		"C" = sosConvertDouble,
		"S/m" = sosConvertDouble,
		# mapping for definition:
		"http://mmisw.org/ont/cf/parameter/sea_water_salinity" = sosConvertDouble)
MBARI <- SOS("http://mmisw.org/oostethys/sos",
		method = SosSupportedConnectionMethods()[["GET"]],
		dataFieldConverters = myConverters)
myOff <- sosOfferings(MBARI)[[1]]
myProc <- sosProcedures(MBARI)[[1]]
mbariObs <- getObservation(sos = MBARI, offering = myOff, procedure = myProc)

sosResult(mbariObs)

##### Daten -> sp/gstat ########################################################

# Typisch für das Geoinformatikerleben: Anwenden von Software anderer Leute...
# Viel Erfolg!

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

