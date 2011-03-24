# Copyright (C) 2011 by 52 North Initiative for Geospatial Open Source Software GmbH, Contact: info@52north.org
# This program is free software; you can redistribute and/or modify it under the terms of the GNU General Public License version 2 as published by the Free Software Foundation. This program is distributed WITHOUT ANY WARRANTY; even without the implied WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program (see gpl-2.0.txt). If not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or visit the Free Software Foundation web page, http://www.fsf.org.
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r
library("sos4R")

################################################################################
# WeatherSOS
#
# establish a connection to a SOS instance with default settings
weathersos <- SOS(url = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos")
summary(weathersos)

# explore SOS, plotting
library(maps); library(mapdata); library(maptools); data(worldHiresMapEnv)
crs <- sosGetCRS(weathersos)[[1]]
worldHigh <- pruneMap(map(database = "worldHires",
				region = c("Germany", "Austria"), plot = FALSE))
worldHigh.lines <- map2SpatialLines(worldHigh, proj4string = crs)

plot(worldHigh.lines, col = "grey50")
plot(weathersos, add = TRUE, lwd = 3)
title(main = paste("Offerings by '", sosTitle(weathersos), "'", sep = ""),
		sub = toString(names(sosOfferings(weathersos))))

# get the latest observation (not standard conform!)
off <- sosOfferings(weathersos)[["ATMOSPHERIC_TEMPERATURE"]]
obs <- getObservation(sos = weathersos, offering = off, #verbose = TRUE,
		latest = TRUE)

# show the result for latest observation
sosResult(obs)

############################################
# Request two procedures, then create a plot
# Attention: plots ignore the fact that the times do NOT perfectly match!
obs <- getObservation(sos = weathersos,
		offering = off,
		procedure = sosProcedures(off),
		inspect = TRUE,
		eventTime = sosCreateTime(weathersos,
				time = "2009-08-10 12:00::2009-08-20 12:00"))
str(obs[[1]], max.level = 3)
str(obs[[2]]@result)
summary(obs)

sosResult(obs[[1]], coordinates = TRUE)[1:2,]

# plot it!
x <- 800
plot(x = obs[[1]]@result[[1]][1:x], y = obs[[1]]@result[[3]][1:x], type = "l",
		col = "steelblue", main = "Temperature in Muenster and Kaernten, 2009",
		xlab = "Time (00:00 o'clock)",
		ylab = "Temperature (°C)",
		xaxt="n") # do not plot x-axis
r <- as.POSIXct(round(range(obs[[1]]@result[[1]]), "days"))
axis.POSIXct(side = 1, x = obs[[1]]@result[[1]][1:x], format = "%d. %h",
		at = seq(r[1], r[2], by="day"))
lines(x = obs[[2]]@result[[1]][1:x], y = obs[[2]]@result[[3]][1:x],
		col = "orange")
legend("topleft", legend = c("Muenster", "Kaernten"),
		col = c("steelblue", "orange"), lty = 1, bty="n")

################################################################################
# Time series analysis
library("xts")

# set up request parameters
station <- sosProcedures(weathersos)[[1]]
temperatureOffering <- sosOfferings(weathersos)[["ATMOSPHERIC_TEMPERATURE"]]
temperature <- sosObservedProperties(temperatureOffering)[1]
september <- sosCreateTime(sos = weathersos,
		time = "2010-09-01 00:00/2010-09-30 00:00")
# make the request
obsSept <- getObservation(sos = weathersos, # verbose = TRUE,
		observedProperty = temperature,
		procedure = station,
		eventTime = september,
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
plot(tempSept, main = "Temperature at WeatherSOS-Station in Muenster",
		xlab = "Time", ylab = paste("Temperature in", attributes(temp)[["unit of measurement"]]),
		major.ticks = "weeks")
lines(data$Time, x$fitted, col = 'red', lwd=3)
#savePlot(type = "png", filename = "usecase.png")

################################################################################
# DescribeSensor Operation
procs <- unique(unlist(sosProcedures(weathersos)))

describeSensor(weathersos, procs[[1]]) #, verbose = TRUE)

procs.descr <- lapply(X = procs, FUN = describeSensor, # verbose = TRUE,
		sos = weathersos)
procs.descr

proc1 <- procs.descr[[1]]
proc1

# original xml:
#proc1@xml

##############################
# access parts of the SensorML
# (partly depends on SensorML profile for discovery)
sosId(proc1)
sosName(proc1)
sosAbstract(proc1)

# get spatial informatin (including unit and reference system information)
#sosCoordinates(proc1, weathersos, forceReparse = TRUE, verbose = TRUE)
coords <- sosCoordinates(proc1)
coords
#str(coords)
attributes(coords)
sosGetCRS(proc1)
sosBoundedBy(proc1)

# create spatial representation, which also will be basis for plottting
as(proc1, "Spatial")

plot(worldHigh.lines, col = "grey50")
for (x in procs.descr) {
	plot(x, add = TRUE, pch = 19)
}
text(sosCoordinates(procs.descr)[c("x", "y")], labels = sosId(procs.descr),
		pos = 4)
title(main = paste("Sensors of", sosTitle(weathersos)))

###################################
# Demo finished, try another one! #
###################################