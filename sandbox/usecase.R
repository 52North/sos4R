library("sos4R"); library("xts")

# create connection to SOS
weathersos = SOS("http://v-swe.uni-muenster.de:8080/WeatherSOS/sos")

# set up request parameters
station <- sosProcedures(weathersos)[[1]]
temperatureOffering <- sosOfferings(weathersos)[["ATMOSPHERIC_TEMPERATURE"]]
temperature <- sosObservedProperties(temperatureOffering)[1]
september <- sosCreateTimePeriod(sos = weathersos,
		begin = as.POSIXct("2010-09-01 00:00"),
		end = as.POSIXct("2010-09-30 00:00"))
# make the request
obsSept <- getObservation(sos = weathersos, observedProperty = temperature,
		procedure = station, eventTime = sosCreateEventTimeList(september),
		offering = temperatureOffering)
data <- sosResult(obsSept)

# inspect data
summary(data); data[1:2,]; names(data)

# create time series from data and plot
tempSept <- xts(x = data[["urn:ogc:def:property:OGC::Temperature"]], order.by = data[["Time"]])
# calculate regression (polynomial fitting)
temp <- data[["urn:ogc:def:property:OGC::Temperature"]]
time <- as.numeric(data[["Time"]])
x = loess(temp~time,
		na.omit(data),enp.target=10)

# create plot
plot(tempSept, main = "Temperature at Station One",
		xlab = "Time", ylab = paste("Temperature in", attributes(temp)[["unit of measurement"]]),
		major.ticks = "weeks")
lines(data$Time, x$fitted, col = 'red', lwd=3)
savePlot(type = "png", filename = "usecase.png")