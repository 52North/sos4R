############################################################################## #
# Copyright (C) 2019 by 52 North                                               #
# Initiative for Geospatial Open Source Software GmbH                          #
#                                                                              #
# Contact: Andreas Wytzisk                                                     #
# 52 North Initiative for Geospatial Open Source Software GmbH                 #
# Martin-Luther-King-Weg 24                                                    #
# 48155 Muenster, Germany                                                      #
# info@52north.org                                                             #
#                                                                              #
# This program is free software; you can redistribute and/or modify it under   #
# the terms of the GNU General Public License version 2 as published by the    #
# Free Software Foundation.                                                    #
#                                                                              #
# This program is distributed WITHOUT ANY WARRANTY; even without the implied   #
# WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU #
# General Public License for more details.                                     #
#                                                                              #
# You should have received a copy of the GNU General Public License along with #
# this program (see gpl-2.0.txt). If not, write to the Free Software           #
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or #
# visit the Free Software Foundation web page, http://www.fsf.org.             #
#                                                                              #
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

library("sos4R")


############################################################################## #
# IOOS/NDBC
#
# Service Description: http://sdf.ndbc.noaa.gov/sos/
# Test Client: http://sdf.ndbc.noaa.gov/sos/test.shtml
#
# IOOS Map: http://www.ioos.gov/catalog/
# NDBC Map: http://www.ndbc.noaa.gov/
# Good for selecting subgroups/programmes of sensors
#
ioos <- SOS(url = "https://sdf.ndbc.noaa.gov/sos/server.php",
		timeFormat = "%Y-%m-%dT%H:%M:%SZ")
ioos.off <- sosOfferings(ioos)
names(ioos.off)
sosResponseFormats(ioos)

#############
# first test:
# !!! use sosName() of offering
obs.test <- getObservation(ioos, offering = sosName(ioos.off[[100]]),
		responseFormat = "text/csv",
		observedProperty = sosObservedProperties(ioos.off[[100]])[1])
obs.test

#off.coords <- sosCoordinates(ioos.off)
off.names <- sosName(ioos.off)

# Plot offerings:
crs <- sosGetCRS(ioos.off[[1]])
library(maps); library(mapdata); library(maptools)
world <- pruneMap(map(database = "world", plot = FALSE))
world.lines <- map2SpatialLines(world, proj4string = crs)

plot(world.lines, col = "grey50")
plot(ioos, lwd = 3, add = TRUE)
title(main = sosTitle(ioos))
#text(x = off.coords[,1], y = off.coords[,1], col = "black",
#		labels = off.names, adj = c(1, 0), cex = 0.75)

ioos.procedures <- unique(unlist(sosProcedures(ioos)))
length(ioos.procedures); length(ioos.off)
# seems like one offering per sensor

############################################################################## #
# get data:

# offerings in pacific?
# Terminology: http://www.pmel.noaa.gov/tao/jsdisplay/help/help_terminology_f.html
# Data display: http://www.pmel.noaa.gov/tao/jsdisplay/
# Create similar plot: http://www.pmel.noaa.gov/cgi-tao/cover.cgi?P1=uwnd&P2=20110304-March-6-2011&P3=month&P4=off&script=jsdisplay/scripts/lat-lon-5day-jsd.csh
# About the buoys: http://www.pmel.noaa.gov/tao/proj_over/pubs/mil96paper.html

offerings.wmo52 <- ioos.off[grep(pattern = "wmo:52", x = off.names)]
obsProps.wmo52 <- unique(unlist(sosObservedProperties(offerings.wmo52)))

last48hrs = sosCreateEventTimeList(sosCreateTimePeriod(ioos,
				begin = as.POSIXct(Sys.time() - 3600 * 48),
				end = as.POSIXct(Sys.time())))
phenomenon <- list("http://mmisw.org/ont/cf/parameter/sea_water_temperature")

# use lapply to call getObservation for every offering with "...:wmo:52..."
# possible alternative: use "all" offering:
#sosName(ioos.off[[1]])
obs.wmo52 <- lapply(X = sosName(offerings.wmo52), FUN = getObservation, #verbose = TRUE,
		sos = ioos, observedProperty = phenomenon,
		eventTime = last48hrs,
		responseFormat = "text/csv")
length(obs.wmo52)
obs.wmo52[[1]]
#attributes(obs.wmo52[[1]])
names(obs.wmo52[[1]])

obs.wmo52.all <- sosResult(obs.wmo52)
summary(obs.wmo52.all)
utils::str(obs.wmo52.all)
# columns are all factors, convert!
obs.wmo52.all[["sea_water_temperature (C)"]] <- as.numeric(obs.wmo52.all[["sea_water_temperature (C)"]])
obs.wmo52.all[["latitude (degree)"]] <- as.numeric(obs.wmo52.all[["latitude (degree)"]])
obs.wmo52.all[["longitude (degree)"]] <- as.numeric(obs.wmo52.all[["longitude (degree)"]])
obs.wmo52.all[["depth (m)"]] <- as.numeric(obs.wmo52.all[["depth (m)"]])
obs.wmo52.all[["date_time"]] <- as.POSIXct(obs.wmo52.all[["date_time"]])
summary(obs.wmo52.all)
dim(obs.wmo52.all)

hist(obs.wmo52.all[["sea_water_temperature (C)"]])
colnames(obs.wmo52.all)

# coordinates seem wrong and there are NA values in the
# data.frame, must be removed
obs.wmo52.all <- obs.wmo52.all[complete.cases(obs.wmo52.all),]

spdf <- SpatialPointsDataFrame(
		coords = obs.wmo52.all[c(4,3)],
		data = obs.wmo52.all[-c(4,3)],
		proj4string = crs)
summary(spdf)

plot(world.lines, col = "grey50")
plot(spdf, add = TRUE)


############################################################################## #
# describe sensor:
# requires SensorML 1.0.0
describeSensorOp <- sosOperation(ioos, sosDescribeSensorName)
describeSensor.outputFormat <- describeSensorOp@parameters[["outputFormat"]][[1]]
ioos.procedures <- unique(unlist(sosProcedures(ioos.get)))
ioos.sensor.1.1 <- describeSensor(sos = ioos, procedure = ioos.procedures[[1]][[1]],
		outputFormat = describeSensor.outputFormat, verbose = TRUE)
ioos.sensor.1.1
ioos.sensor.1.1@xml

##############
# time series:
#begin <- sosTime(ioos.post.off[[1]], convert = TRUE)[[1]]
end <- as.POSIXct(Sys.time())
begin <- end - 3600 * 24 * 30

.offering <- sosOfferings(ioos, name = sosName(ioos.off[[1]]))
.offeringId <- sosId(.offering)
obs.001 <- getObservation(sos = ioos,
		offering = sosName(.offering), # "urn:ioos:network:noaa.nws.ndbc:all"
		procedure = sosProcedures(ioos.off[[1]])[690:700],
		observedProperty = sosObservedProperties(ioos.off[[1]])[6:7],
		responseFormat = "text/csv",
		eventTime = sosCreateEventTimeList(
				sosCreateTimePeriod(sos = ioos, begin = begin, end = end)),
		inspect = TRUE, verbose = TRUE)

plot(x = obs.001$date_time, y = obs.001$`sea_floor_depth_below_sea_surface (m)`)

###################################
# Demo finished, try another one! #
###################################
