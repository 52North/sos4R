# Copyright (C) 2010 by 52 North Initiative for Geospatial Open Source Software GmbH, Contact: info@52north.org
# This program is free software; you can redistribute and/or modify it under the terms of the GNU General Public License version 2 as published by the Free Software Foundation. This program is distributed WITHOUT ANY WARRANTY; even without the implied WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program (see gpl-2.0.txt). If not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or visit the Free Software Foundation web page, http://www.fsf.org.
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r

##############################################################################
# AirQuality
airsos <- SOS(url = "http://giv-sos.uni-muenster.de:8080/AirQualitySOS/sos")

airsos.offerings <- sosOfferings(airsos)
names(airsos.offerings)

##############################################################################
# EEA SOS

# Preview: some units are not supported out of the box (saw this when I first did
# a getObservation call):
# 	No converter for the unit of measurement  µg/m^3  with the definition 
# 	urn:ogc:def:phenomenon:OGC:1.0.30:PM10 ! Trying a default, but you can add
#	one when creating a SOS using SosDataFieldConvertingFunctions().
eea.converters <- SosDataFieldConvertingFunctions(
		"urn:ogc:def:phenomenon:OGC:1.0.30:PM10" = sosConvertDouble,
		"urn:ogc:def:phenomenon:OGC:1.0.30:NO2" = sosConvertDouble)
eeasos <- SOS(url = "http://discomap-test.eea.europa.eu/swe/sos",
		dataFieldConverters = eea.converters)

###########
# OFFERINGS
# get the available offerings
eea.offerings <- sosOfferings(eeasos)
eea.offerings
# extract one offering
eea.off.pm10 <- eea.offerings[["PM10"]]
eea.off.pm10
eea.off.no2 <- eea.offerings[["NO2"]]

#####################
# OBSERVED PROPERTIES
# get the observed properties of all offerings
eea.obsProp <- sosObservedProperties(eeasos)
# (be aware that this is a list of character vectors
str(eea.obsProp)

# get the observed properties
eea.obsProp.pm10 <- sosObservedProperties(eea.off.pm10)
eea.obsProp.no2 <- sosObservedProperties(eea.off.no2)
# both the same:
eea.obsProp.pm10; eea.obsProp[["PM10"]][1]

##############
# BOUNDING BOX
#sosBoundedBy(eea.off.pm10) # or eea.off.pm10@boundedBy
# *** Here the format could definitly be improved, i.e. matrix as in sp, even with a proj4string based on srsName?

#############
# TIME PERIOD
# for all data based on eventTime parameter in GetObservation metadata
sosTime(eeasos)

# for one offering
sosTime(eea.off.pm10)
# *** Just name what format would work best here! Or coercion to some R format?

############
# PROCEDURES
sosProcedures(eeasos) # list of vectors
sosProcedures(eea.off.pm10) # still a long vector!

####################
# OBSERVATIONS: PM10
# *** The event time handling is not really nice yet...
lastWeek = sosCreateEventTimeList(sosCreateTimePeriod(sos = eeasos,
				begin = as.POSIXct(Sys.time() - 3600*24*7),
				end = as.POSIXct(Sys.time())))

# if you want to see what get's in and out, just set "inspect" flag to TRUE
observation.pm10.week <- getObservation(sos = eeasos,
		offering = eea.off.pm10,
#		observedProperty = eea.obsProp.pm10, # not needed, taken from the offering as default
		eventTime = lastWeek,
		procedure = sosProcedures(eea.off.pm10),
#		inspect = TRUE
)

# explore the returned observation collection:
observation.pm10.week
observation.pm10.week[[1]]

# sosResult currently does the same as as.data.frame for single observations
# and binds the observation together for observation collections:
as.data.frame(observation.pm10.week[[1]]); sosResult(observation.pm10.week[[1]])
result.pm10.week <- sosResult(observation.pm10.week)

# sort 
sort_df(result.pm10.week, names(result.pm10.week)[3])
sort_df(result.pm10.week, "Time")

# subset that data frame
subset(result.pm10.week, feature=="foi_AT10001")
subset(result.pm10.week, feature==features[2:5] & PM10 > 10)

# subset the observation collection based on features, observed properties and 
# procedures
features <- levels(result.pm10.week$feature)
features
observation.pm10.week[features[1:4]]
observation.pm10.week[sosObservedProperties(observation.pm10.week)[2]]
observation.pm10.week[sosProcedures(observation.pm10.week)[2:4]]

# get observation metadata
result01 <- sosResult(observation.pm10.week[[1]])
attributes(result01)

# get station position
sosCoordinates(observation.pm10.week[[1]])
sosCoordinates(observation.pm10.week[1:3])
sosCoordinates(observation.pm10.week)
str(sosCoordinates(observation.pm10.week))

# create some plots
plot(result01[["Time"]], result01[["PM10"]])
library(lattice)
xyplot(PM10~Time|feature, sosResult(observation.pm10.week[1:50]), type='l',
		par.strip.text=list(cex=.7))

###
# getting data for a whole year and many procedures, still quite fast as not much data.
lastYear = sosCreateEventTimeList(sosCreateTimePeriod(sos = eeasos,
				begin = as.POSIXct(Sys.time() - 3600*24*365),
				end = as.POSIXct(Sys.time())))
observation.pm10.year <- getObservation(sos = eeasos,
		offering = eea.off.pm10,
#		observedProperty = eea.obsProp.pm10, # not needed, taken from the offering as default
		eventTime = lastYear,
		procedure = sosProcedures(eea.off.pm10)[c(1:100)])


###################
# OBSERVATIONS: NO2

# ! DN: There is some problem here if requestion a lot of procedures (> about 177) at once, I'm looking into that.

observation.no2.year <- getObservation(sos = eeasos,
		offering = eea.off.no2,
#		observedProperty = eea.obsProp.pm10, # not needed, taken from the offering as default
		eventTime = lastYear,
		procedure = sosProcedures(eea.off.no2)[1:180],
		verbose = TRUE)

# observations from complete time period
timePeriod.no2 <- sosTime(eea.off.no2)
observation.no2.all <- getObservation(sos = eeasos,
		offering = eea.off.no2,
#		observedProperty = eea.obsProp.pm10, # not needed, taken from the offering as default
		eventTime = sosCreateEventTimeList(timePeriod.no2),
		procedure = sosProcedures(eea.off.no2)[1:20])

