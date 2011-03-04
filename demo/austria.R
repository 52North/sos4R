# Copyright (C) 2010 by 52 North Initiative for Geospatial Open Source Software GmbH, Contact: info@52north.org
# This program is free software; you can redistribute and/or modify it under the terms of the GNU General Public License version 2 as published by the Free Software Foundation. This program is distributed WITHOUT ANY WARRANTY; even without the implied WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program (see gpl-2.0.txt). If not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or visit the Free Software Foundation web page, http://www.fsf.org.
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r

################################################################################
# http://ispacevm10.researchstudio.at/sostester/
# Contact: Michael Lippautz: michael.lippautz@researchstudio.at
# Use the two development services on ispacevm10.

# PROBLEMS using GET:
# - Temporal filtering has no effect...

################################################################################
# Nationalpark Berchtesgaden
npbg.converter <- SosDataFieldConvertingFunctions(
		"urn:ogc:def:property:OGC:Time:iso8601" = sosConvertTime,
		"urn:ogc:def:property:OGC:Reflection" = sosConvertDouble,
		"urn:ogc:def:property:OGC:Insolation" = sosConvertDouble)
npbg <- SOS("http://ispacevm10.researchstudio.at/geoservices/npbg",
		method = "GET",
		#verboseOutput = TRUE,
		dataFieldConverters = npbg.converter,
		sections = NA)
npbg
summary(npbg)

#################
# Plot whole sos:
library(maps); library(mapdata); library(maptools)
data(worldHiresMapEnv)
crs <- unique(sosGetCRS(npbg))[[1]]
worldHigh <- pruneMap(map(database = "worldHires", region = "Austria",
				plot = FALSE))
worldHigh.lines <- map2SpatialLines(worldHigh, proj4string = crs)

plot(worldHigh.lines, col = "grey50")
plot(npbg, add = TRUE, lwd = 2)
map.axes()
map.scale()
.offNames <- sapply(names(sosOfferings(npbg)), FUN = strsplit, split = ":")
title(main = paste("Offerings by '", sosTitle(npbg), "'", sep = ""),
		sub = toString(sapply(.offNames, "[[", 3)))

#########################
# superordinate offering:
np.off <- sosOfferings(npbg)[["org:npbg:Nationalpark"]]
#np.off
summary(np.off)

np.obsProp <- sosObservedProperties(np.off)
np.obsProp
np.proc <- sosProcedures(np.off)
np.proc

###########
# Get data:
lastDay <- sosCreateTimePeriod(sos = npbg, begin = (Sys.time() - 3600 * 24),
		end = Sys.time())

obs.proc1 <- getObservation(sos = npbg, offering = np.off, inspect = TRUE,
	procedure = np.proc[[1]],
	eventTime = sosCreateEventTimeList(lastDay)
	)

# cannot get the coordinates automatically via sosResult(), because they are
# given as attribute, not as a featureOfInterest
#result.proc1 <- sosResult(obs.proc1, coordinates = TRUE)
#spdf.proc1 <- as(obs.proc1, "Spatial")

# Coordinates inline:
result.proc1 <- sosResult(obs.proc1)
summary(result.proc1)

coords.proc1 <- unique(result.proc1[c("Latitude", "Longitude")])
coords.proc1


##################
# plot all values:
names(result.proc1)
plot(result.proc1[7:9],
		main = paste(np.proc[[1]], "at", toString(coords.proc1)))

######################
# xyplot, dotplot ...:
xyplot(Insolation ~ RelativeHumidity, data = result.proc1, 
		main = paste(np.proc[[1]], "(", 
				toString(coords.proc1), ")"),
		sub = paste("Time range: ", toString(range(result.proc1[["Time"]]))))

xyplot(Insolation ~ RelativeHumidity | AirTemperature,
		data = result.proc1[1:100,])

dotplot(Insolation ~ RelativeHumidity, data = result.proc1)


####################################
# plot values against time with xts:
proc1.times <- unique(result.proc1[["Time"]])

library(xts)
proc1.xts.refl <- xts(x = result.proc1[["Reflection"]],
		order.by = result.proc1[["Time"]], unique = TRUE)
proc1.xts.inso <- xts(x = result.proc1[["Insolation"]],
		order.by = result.proc1[["Time"]], unique = TRUE)
plot(proc1.xts.refl,
		main = paste("Reflection at", np.proc[[1]], "(", 
				toString(coords.proc1), ")"),
		type = "bars")

############################
# plot time series with zoo:
library(zoo)

proc1.zoo <- zoo(x = as.matrix(result.proc1[5:9]),
		order.by = result.proc1[["Time"]])
str(proc1.zoo)
plot(proc1.zoo, main = paste("Time Series at", np.proc[[1]], "(", 
				toString(coords.proc1), ")"),
		plot.type = "multiple")


####################
# request more data:
obs.proc123 <- getObservation(sos = npbg, offering = np.off, # inspect = TRUE,
		procedure = np.proc[1:3])
#Finished getObservation to http://ispacevm10.researchstudio.at/geoservices/npbg 
#--> received 3 observation(s) having 31345 result values [ 8942, 11112, 11291 ].
obs.proc123

str(obs.proc123[[1]])

sosProcedures(obs.proc123)

################################################################################
# Land Oberösterreich
ooe <- SOS("http://ispacevm10.researchstudio.at/geoservices/ooe", sections = NA,
		verboseOutput = TRUE)
ooe
summary(ooe)

# TODO KML export, or make visualization example following howto:
# http://spatial-analyst.net/wiki/index.php?title=Export_maps_to_GE
