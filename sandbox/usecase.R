################################################################################
# weathersos
#library("sos4R")

# create connection to SOS
weathersos = SOS("http://v-swe.uni-muenster.de:8080/WeatherSOS/sos")

# set up request parameters
stationMuenster <- sosProcedures(weathersos)[[1]]
temperatureOffering <- sosOfferings(weathersos)[["ATMOSPHERIC_TEMPERATURE"]]
temperature <- sosObservedProperties(weathersos)[5] # "urn:ogc:def:property:OGC::Temperature"
september <- sosCreateTimePeriod(sos = weathersos,
		begin = as.POSIXct("2010-09-01 00:00"),
		end = as.POSIXct("2010-09-30 00:00"))
# make the request
obsSept <- getObservation(sos = weathersos,
		observedProperty = temperature,
		procedure = stationMuenster,
		eventTime = sosCreateEventTimeList(september),
		offering = temperatureOffering)

# inspect data
summary(sosResult(obsSept))
sosResult(obsSept)[1:2,]
names(sosResult(obsSept))

data <- sosResult(obsSept)
# ejp

# create time series from data and plot
library("xts")
tempSept <- xts(x = data[["urn:ogc:def:property:OGC::Temperature"]],
		order.by = data[["Time"]])
plot(tempSept, main = "Temperature in Münster",
		xlab = "Time", ylab = "Temperature (°C)", major.ticks = "weeks")


################################################################################

