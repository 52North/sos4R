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
title(main = paste("Offerings by '", sosTitle(npbg), "'", sep = ""),
		sub = toString(names(sosOfferings(npbg))))

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

obs.proc1 <- getObservation(sos = npbg, offering = np.off, #inspect = TRUE
	procedure = np.proc[[1]],
#	eventTime = sosCreateEventTimeList(lastDay),
	)

# cannot get the coordinates via result, because they are given as attribute,
# not as a featureOfInterest
#result.proc1 <- sosResult(obs.proc1, coordinates = TRUE)
#spdf.proc1 <- as(obs.proc1, "Spatial")

# Coordinates inline:
result.proc1 <- sosResult(obs.proc1)
summary(result.proc1)

coords.proc1 <- unique(result.proc1[c("Latitude", "Longitude")])
coords.proc1

plot(result.proc1)

################################################################################
# Land Oberösterreich
ooe <- SOS("http://ispacevm10.researchstudio.at/geoservices/ooe",
		method = "GET", sections = NA)
ooe
summary(ooe)








# TODO KML export, or make visualization example following howto:
# http://spatial-analyst.net/wiki/index.php?title=Export_maps_to_GE
