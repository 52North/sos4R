# Copyright (C) 2010 by 52 North Initiative for Geospatial Open Source Software GmbH, Contact: info@52north.org
# This program is free software; you can redistribute and/or modify it under the terms of the GNU General Public License version 2 as published by the Free Software Foundation. This program is distributed WITHOUT ANY WARRANTY; even without the implied WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program (see gpl-2.0.txt). If not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or visit the Free Software Foundation web page, http://www.fsf.org.
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r
library("sos4R")

################################################################################
# AQE SOS
# Data source: http://www.eea.europa.eu/themes/air/airbase

# Set the converters for observed properties:
aqe.converters <- SosDataFieldConvertingFunctions(
		"http://giv-genesis.uni-muenster.de:8080/SOR/REST/phenomenon/OGC/Concentration[PM10]" = sosConvertDouble,
		"http://giv-genesis.uni-muenster.de:8080/SOR/REST/phenomenon/OGC/Concentration[NO2]" = sosConvertDouble,
		"http://giv-genesis.uni-muenster.de:8080/SOR/REST/phenomenon/OGC/Concentration[O3]" = sosConvertDouble)

# Create the SOS connection:
aqe <- SOS(url = "http://giv-uw.uni-muenster.de:8080/AQE/sos",
		dataFieldConverters = aqe.converters)
summary(aqe)

# Get the available offerings:
aqe.offerings <- sosOfferings(aqe)
names(aqe.offerings)

###########
# Plot SOS:
library(maps); library(mapdata); library(maptools)
data(worldHiresMapEnv)
crs <- sosGetCRS(aqe)[[1]]
region <- map.where(database = "worldHires",
		sosCoordinates(aqe.offerings)) # find region
worldHigh <- pruneMap(map(database = "worldHires", region = region,
				plot = FALSE))
worldHigh.lines <- map2SpatialLines(worldHigh, proj4string = crs)

plot(worldHigh.lines, col = "grey50")
plot(aqe, add = TRUE, lwd = 3)
title(main = paste("Offerings Germany by '", sosTitle(aqe), "'", sep = ""),
		sub = toString(names(aqe.offerings)))

################################################################################
# NO2
#
# Extract one offering of interest and explore:
aqe.off.no2 <- aqe.offerings[["NO2"]]
aqe.off.no2
summary(aqe.off.no2)

# Get observations, december 2003 is arbitrary choice!
dec2003.12Hrs = sosCreateEventTimeList(sosCreateTimePeriod(sos = aqe,
				begin = as.POSIXct("2003/12/01 08:00"),
				end = as.POSIXct("2003/12/01 20:00")))
dec2003.24Hrs = sosCreateEventTimeList(sosCreateTimePeriod(sos = aqe,
				begin = as.POSIXct("2003/12/01 08:00"),
				end = as.POSIXct("2003/12/02 08:00")))
dec2003 = sosCreateEventTimeList(sosCreateTimePeriod(sos = aqe,
				begin = as.POSIXct("2003/12/01"),
				end = as.POSIXct("2003/12/31")))

# Request data (request and response can be check by setting the inspect=TRUE):
obs.no2.12Hrs <- getObservation(sos = aqe, # inspect = TRUE,
		offering = aqe.off.no2,
		#procedure = sosProcedures(aqe.off.no2)[1:20],
		saveOriginal = TRUE,# saves file in getwd()
		eventTime = dec2003.12Hrs)
# 38 secs

# Explore the returned observation collection:
obs.no2.12Hrs
# There is one observatio for every FOI / procedure combination :
names(obs.no2.12Hrs)[1:3]

# Subset the collection (features, observed properties and procedures):
# sosFeatureIds(obs.no2.12Hrs)[c(1,100)]
obs.no2.12Hrs[sosFeatureIds(obs.no2.12Hrs)[c(1,100)]]
# sosObservedProperties(obs.no2.12Hrs)[2]
obs.no2.12Hrs[sosObservedProperties(obs.no2.12Hrs)[2]]
# sosProcedures(obs.no2.12Hrs)[200:201]
obs.no2.12Hrs[sosProcedures(obs.no2.12Hrs)[200:201]]

# More requests for more data for testing of response time:
#obs.no2.24Hrs <- getObservation(sos = aqe, # inspect = TRUE,
#		offering = aqe.off.no2,
#		eventTime = dec2003.24Hrs)
## 41 secs
#obs.no2.dec <- getObservation(sos = aqe, # inspect = TRUE,
#		offering = aqe.off.no2,
#		eventTime = dec2003)
## 3:25 mins
#obs.no2.dec
#result.no2.dec <- sosResult(obs.no2.dec)[1:10, ]


################################################################################
# Get the result data for all observations, with coordinates:
result.no2.12Hrs <- sosResult(obs.no2.12Hrs, coordinates = TRUE)
# Coordinates only:
#sosCoordinates(obs.no2.12Hrs[1:10])
# One observation only
# sosResult(obs.no2.12Hrs[[42]])

summary(result.no2.12Hrs)
NO2 <- colnames(result.no2.12Hrs)[[3]]

# Subset and sort the data with subset or sort_df
subset(result.no2.12Hrs, feature=="foi_DEBY109")
require("reshape")
# The ten highest values:
tail(sort_df(result.no2.12Hrs, NO2), 10)

# Histogram of NO2 data:
hist(result.no2.12Hrs[,3], main = "NO2")
# Test plot:
plot(result.no2.12Hrs[["Time"]], result.no2.12Hrs[[NO2]])


################################################################################
# Get the result data and create sp object:
obs.no2.crs <- sosGetCRS(obs.no2.12Hrs)
no2.spdf <- SpatialPointsDataFrame(
		coords = result.no2.12Hrs[,c("lon", "lat")],
		data = result.no2.12Hrs[,c("Time", "feature", NO2)],
		proj4string = obs.no2.crs)
bbox(no2.spdf)
#obs.no2.bbox <- sosBoundedBy(obs.no2.12Hrs, bbox = TRUE) # equal
summary(no2.spdf)

#########################################
# Shortcut to get SpatialPointsDataFrame:
#as(obs.no2.12Hrs[[1]], "SpatialPointsDataFrame")
no2.spdf.shortcut <- as(obs.no2.12Hrs, "SpatialPointsDataFrame")
summary(no2.spdf.shortcut)

####################################
# Plot stations with background map:
require("mapdata")
germany.p <- pruneMap(map(database = "worldHires", region = "Germany",
				plot = FALSE))
germany.sp <- map2SpatialLines(germany.p, proj4string = obs.no2.crs)
proj4string(germany.sp) <- obs.no2.crs
plot(x = germany.sp, col = "grey")
plot(no2.spdf, pch = 20, col = "blue", add = TRUE)
title("NO2 Germany")

##############
# Bubble plot:
bubble(no2.spdf, zcol = 3, maxsize = 2, col = c("#1155ff"),
		main = "NO2 in Germany", do.sqrt = TRUE)

################################################################################
# Transform to UTM for kriging and background map:
require("rgdal")
utm32 = CRS("+proj=utm +zone=32 +datum=WGS84")
germany.utm <- spTransform(germany.sp, utm32)
no2.spdf.utm = spTransform(no2.spdf, utm32)
plot(germany.utm, col = "grey")
plot(no2.spdf.utm, add = TRUE)
title(main = "NO2 Germany", sub = "UTM projection")

 
################################################################################
# TODO Kriging with automap?

################################################################################
# Plot with whole year 2004 for one station:
# See http://www.eea.europa.eu/themes/air/airbase/interpolated for identifiers.
denw095 <- "urn:ogc:object:feature:Sensor:EEA:airbase:4.0:DENW095"
denw095.descr <- describeSensor(aqe, denw095)
denw095.descr
#procedure1.descr@xml

obs.denw095.2004 <- getObservation(sos = aqe, # inspect = TRUE,
		offering = aqe.off.no2,
		procedure = denw095,
		eventTime = sosCreateEventTimeList(sosCreateTimePeriod(sos = aqe,
						begin = as.POSIXct("2004/01/01"),
						end = as.POSIXct("2004/12/31")))
)
# second(s)

# Plot it:
data.denw095.2004 <- sosResult(obs.denw095.2004)
summary(data.denw095.2004)

denw095.NO2.attributes <- attributes(data.denw095.2004[[NO2]])
#data.denw095.2004.locRegr = loess(
#		formula = data.denw095.2004[[NO2]]~data.denw095.2004[["Time"]],
#		data = data.denw095.2004[[NO2]])
plot(data.denw095.2004[["Time"]], data.denw095.2004[[NO2]], type = "l",
		main = "NO2 in Muenster", sub = denw095,
		xlab = "Time",
		ylab = paste("NO2 in ",
				denw095.NO2.attributes[["unit of measurement"]],
						sep = ""))
#lines(data.denw095.2004$Time, data.denw095.2004.locRegr$fitted, col = 'red', lwd=3)

