# Authors: 	Sandy Adriaenssens <adriaenssens@irceline.be>,
#						Olav Peeters <peeters@irceline.be>
# Project: Joaquin (Joint Air Quality Initiative) - http://www.joaquin.eu/
library("sos4R")

# example addapted from the SOS4R manual
# fetch UFP data (90500 - PN1) and inspect the results

## Add converter for particle  numbers:
myConverters <- SosDataFieldConvertingFunctions("90500 - PN1" = sosConvertDouble) 

# Take a SOS from the example list 
# SosExampleServices() 
# or specify the URL of an online SOS server:
sos_irceline = SOS("http://sos.irceline.be/sos", 
									 curlHandle = getCurlHandle(http.version=HTTP_VERSION_1_0), 
									 dataFieldConverters = myConverters)

# List offerings, procedures and observedProperties
names(sosOfferings(sos_irceline))
class(sosProcedures(sos_irceline)$`90500 - PN1`)
class(sosProcedures(sos_irceline)[2])

sosProcedures(sos_irceline)
sosObservedProperties(sos_irceline)
# Create time period (last 30 days)
tPeriod <- sosCreateEventTimeList(
	time = sosCreateTimePeriod(
		sos = sos_irceline,
		begin = Sys.time() - (3600 * 24 * 30),
		end = Sys.time()))
# Request data for all observed properties and procedures of a certain offering
observation <- getObservation(sos = sos_irceline,
			offering = sosOfferings(sos_irceline)[["90500 - PN1"]],
			observedProperty = sosObservedProperties(sos_irceline)[["90500 - PN1"]],
			procedure = sosProcedures(sos_irceline)[["90500 - PN1"]],
			eventTime = tPeriod)


# Inspect result
sosResult(observation)
str(sosResult(observation))

# Inspect attributes of the data fields
attributes(sosResult(observation)[,1])


################################################################################
# example addapted from: http://www.nordholmen.net/sos4r/tag/example/
# fetch UFP data (90500 - PN1) and plot a simple graph 

# if you want to consult the CheatSheet uncomment the following line
#sosCheatSheet()

names(sosOfferings(sos_irceline))
go.offering = sosOfferings(sos_irceline)[["90500 - PN1"]] # UFP
go.observedProperty = sosObservedProperties(sos_irceline)[["90500 - PN1"]] # UFP
go.eventTime3 = sosCreateEventTimeList(sosCreateTimePeriod(sos = sos_irceline, 
											begin = as.POSIXct("2014-01-01 18:00"),
											end = as.POSIXct("2014-01-10 18:00")))


obs3 <- getObservation(sos = sos_irceline,observedProperty = go.observedProperty,
					procedure = sosProcedures(sos_irceline)[["90500 - PN1"]][1], 
					#procedure = list(sosProcedures(sos)[["6924"]]),
					#verbose = TRUE,
					#inspect = TRUE,
					eventTime = tPeriod,
					offering = go.offering)

PN <- sosResult(obs3)
head(PN)

summary(PN$"90500 . PN1")
summary(PN$SamplingTime)
length(PN$"90500 . PN1")
length(PN$SamplingTime)

attributes(PN$SamplingTime)
attributes(PN$"90500 . PN1")

plot(PN$SamplingTime, PN$"90500 . PN1"[1:length(PN$"90500 . PN1")], type = "l",
		 main = "Particle Numbers",
		 xlab = paste0(attributes(PN$SamplingTime)$"name", "\n",
		 							attributes(PN$SamplingTime)$"definition"), # "Time",
		 ylab = paste0("PN in ", 
		 							attributes(PN$"90500 . PN1")$"unit of measurement")) #"PN (#/cm3)")

