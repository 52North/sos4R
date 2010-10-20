# optional: install the package
#install.packages("sos4R")

# load the sos4R package
#library("sos4R")
#source("/home/daniel/Dropbox/2010_SOS4R/workspace/sos4R/sandbox/loadSources.R")

################################################################################
# pegelonlinesos
# not so nice: not exactly reproducible because data is only stored for 30 days!

# create connection to SOS
pegelsos <- SOS(url = "http://v-sos.uni-muenster.de:8080/PegelOnlineSOSv2/sos")

# what data do I get?
names(sosOfferings(pegelsos))

# set up parameters for request
procedures <- sosProcedures(pegelsos)
procedures <- subset(procedures, procedures %in% grep("*Bake*", procedures, value=TRUE))
# Watch out here because order of elements can change! The correct value is given in the comment.
wasserstand <- sosObservedProperties(pegelsos)[1] # <- Should be "Wasserstand"
wasserstand_roh <- sosOfferings(pegelsos)[[1]] # <- Should be "WASSERSTAND_ROHDATEN"
lastSixtyDays <- sosCreateEventTimeList(time = sosCreateTimePeriod(
	sos = pegelsos,
	begin = Sys.time() - (3600 * 24 * 60),
	end = Sys.time()))

pegelObs <- getObservation(sos = pegelsos, offering = wasserstand_roh,
		observedProperty = wasserstand, procedure = procedures[7],
		eventTime = lastSixtyDays)
data <- sosResult(pegelObs)

# inspect data
summary(data)
data[1:2,]
names(data)
attributes(data[,3])

# clean up data (remove negative values)
data <- subset(data, data[,3]>0)

# create time series from data and plot
library("xts")
bakeA <- xts(x = data[["Wasserstand"]], order.by = data[["Time"]])
plot(bakeA, main = "Water Level at Bake A", xlab = "Time", ylab = "Water Level (cm)", major.ticks = "days")

library("forecast")
# time series plots
tsdisplay(bakeA)
# check the periodicity
periodicity(bakeA)

# fit autoregressive model, selcting complexity by AIC
bakeA.ar <- ar(bakeA)
# forecast and plot
bakeA.ar.fcast <- forecast(bakeA.ar, h = 60 * 48) # 48 hrs
plot(bakeA.ar.fcast)
summary(bakeA.ar.fcast) # overview of forecast
accuracy(bakeA.ar.fcast) # check goodness of fit
# looks interesting! save figure1.png
savePlot(type = "png", filename = "figure1.png")

# TODO add filtered lines

# try out ets
bakeA.ets <- ets(bakeA)
bakeA.ets.fcast <- forecast(bakeA.ets, h = 60 * 24) # 12 hrs
plot(bakeA.ets.fcast)
# not useful

# fit ARMA model by conditional least squares and plot it
bakeA.arma <- arma(bakeA)
plot(bakeA.arma)


################################################################################
# weathersos
#library("sos4R")

# create connection to SOS
weathersos = SOS("http://v-swe.uni-muenster.de:8080/WeatherSOS/sos")

# set up request parameters
stationMuenster <- sosProcedures(weathersos)[1]
temperatureOffering <- sosOfferings(weathersos)[["ATMOSPHERIC_TEMPERATURE"]]
temperature <- sosObservedProperties(weathersos)[5] # "urn:ogc:def:property:OGC::Temperature"
september <- sosCreateEventTimeList(sosCreateTimePeriod(sos = weathersos,
				begin = as.POSIXct("2010-09-01 00:00"),
				end = as.POSIXct("2010-09-30 24:00")))
# make the request
obsSept <- getObservation(sos = weathersos,
		observedProperty = temperature,
		procedure = stationMuenster,
		eventTime = september,
		offering = temperatureOffering)

# inspect data
summary(sosResult(obsSept))
sosResult(obsSept)[1:2,]
names(sosResult(obsSept))
data <- sosResult(obsSept)

# create time series from data and plot
library("xts")
tempSept <- xts(x = data[["urn:ogc:def:property:OGC::Temperature"]],
	order.by = data[["Time"]])
plot(tempSept, main = "Temperature in Münster",
	xlab = "Time", ylab = "Temperature (°C)", major.ticks = "weeks")

# time series plots
tsdisplay(tempSept)
# check the periodicity
periodicity(tempSept)
# create models
tempSeptArima <- Arima(tempSept)
tempSeptETS <- ets(tempSept)
# auto-regressive
tempSeptAR <- ar(tempSept)

# create forecasts
library("forecast")
# based on exponential smoothing
fcastETS <- forecast(tempSeptETS, h = 1000)
summary(fcastETS) # overview of forecast
accuracy(fcastETS) # check goodness of fit
plot(fcastETS)
# based on ARIMA model
fcastArima <- forecast(tempSeptArima)
accuracy(fcastArima) # check goodness of fit
plot(fcastArima)


################################################################################
# one year
# create connection to SOS
weathersos = SOS("http://v-swe.uni-muenster.de:8080/WeatherSOS/sos")
# set up request parameters
stationMuenster <- sosProcedures(weathersos)[1]
temperatureOffering <- sosOfferings(weathersos)[["ATMOSPHERIC_TEMPERATURE"]]
temperature <- sosObservedProperties(weathersos)[5] # "urn:ogc:def:property:OGC::Temperature"
timeperiod2009 <- sosCreateEventTimeList(sosCreateTimePeriod(sos = weathersos,
				begin = as.POSIXct("2009-01-01 00:00"),
				end = as.POSIXct("2009-12-31 00:00")))
# make the request
temp2009 <- getObservation(sos = weathersos,
		observedProperty = temperature,
		procedure = stationMuenster,
		eventTime = timeperiod2009,
		offering = temperatureOffering)

# inspect data
summary(sosResult(temp2009))
sosResult(temp2009)[1:2,]
names(sosResult(temp2009))

# create time series from data and plot
library("xts")
tempSeries2009 <- xts(x = sosResult(temp2009)[["urn:ogc:def:property:OGC::Temperature"]],
	order.by = sosResult(temp2009)[["Time"]])
plot(tempSeries2009, main = "Temperature in Münster", xlab = "Time", ylab = "Temperature (°C)", major.ticks = "months")

# time series plots
tsdisplay(tempSeries2009)

# check the periodicity
periodicity(tempSeries2009)

# create models
temp2009Arima <- Arima(tempSeries2009)
temp2009ETS <- ets(tempSeries2009)
temp2009AR <- ar(tempSeries2009)
# diagnose model
tsdiag(temp2009ETS)

# create forecasts
library("forecast")

# based on exponential smoothing
fcast2009ETS <- forecast(temp2009ETS, h = 300)
summary(fcast2009ETS) # overview of forecast
accuracy(fcast2009ETS) # check goodness of fit
plot(fcast2009ETS)
plot(fcast2009ETS, xlim=c(26000,30500), xlab=) # last part of the year
# Save figure1.png
savePlot(type = "png", filename = "figure2.png")

# autoregressive model
plot(temp2009AR)

# data is apparently not period enough...
stl(tempSeries2009)
#Fehler in stl(tempSept) : 
#  Zeitreihe ist nicht periodisch oder umfasst weniger als zwei Perioden
