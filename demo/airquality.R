# Copyright (C) 2010 by 52 North Initiative for Geospatial Open Source Software GmbH, Contact: info@52north.org
# This program is free software; you can redistribute and/or modify it under the terms of the GNU General Public License version 2 as published by the Free Software Foundation. This program is distributed WITHOUT ANY WARRANTY; even without the implied WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program (see gpl-2.0.txt). If not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or visit the Free Software Foundation web page, http://www.fsf.org.
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r

##############################################################################
# AirQuality
airsos.converters <- SosDataFieldConvertingFunctions(
		"urn:ogc:def:phenomenon:OGC:1.0.30:PM10" = sosConvertDouble,
		"urn:ogc:def:phenomenon:OGC:1.0.30:NO2" = sosConvertDouble)
airsos <- SOS(url = "http://giv-sos.uni-muenster.de:8080/AirQualitySOS/sos",
		dataFieldConverters = airsos.converters)

airsos.offerings <- sosOfferings(airsos)
names(airsos.offerings)

##############################################################################
# airsos SOS

# Preview: some units are not supported out of the box (saw this when I first did
# a getObservation call):
# 	No converter for the unit of measurement  µg/m^3  with the definition 
# 	urn:ogc:def:phenomenon:OGC:1.0.30:PM10 ! Trying a default, but you can add
#	one when creating a SOS using SosDataFieldConvertingFunctions().
airsos.converters <- SosDataFieldConvertingFunctions(
		"urn:ogc:def:phenomenon:OGC:1.0.30:PM10" = sosConvertDouble,
		"urn:ogc:def:phenomenon:OGC:1.0.30:NO2" = sosConvertDouble)
sos = NA
airsos2 <- SOS(url = sos2, dataFieldConverters = airsos.converters)

###########
# OFFERINGS
# get the available offerings
airsos.offerings <- sosOfferings(airsos)
airsos.offerings
# extract one offering
airsos.off.pm10 <- airsos.offerings[["PM10"]]
airsos.off.pm10
airsos.off.no2 <- airsos.offerings[["NO2"]]

#####################
# OBSERVED PROPERTIES
# get the observed properties of all offerings
airsos.obsProp <- sosObservedProperties(airsos)
# (be aware that this is a list of character vectors
str(airsos.obsProp)

# get the obseairsos.obsProp.pm10rved properties
airsos.obsProp.pm10 <- sosObservedProperties(airsos.off.pm10)
airsos.obsProp.no2 <- sosObservedProperties(airsos.off.no2)
# both the same:
airsos.obsProp.pm10; airsos.obsProp[["PM10"]][1]

##############
# BOUNDING BOX
sosBoundedBy(airsos.off.pm10) # or airsos.off.pm10@boundedBy
# *** Here the format could definitly be improved, i.e. matrix as in sp, even with a proj4string based on srsName?

#############
# TIME PERIOD
# for all data based on eventTime parameter in GetObservation metadata
sosTime(airsos)

# for one offering
sosTime(airsos.off.pm10)
# *** Just name what format would work best here! Or coercion to some R format?

############
# PROCEDURES
sosProcedures(airsos) # list of vectors
sosProcedures(airsos.off.pm10) # still a long vector!

####################
# OBSERVATIONS: PM10
# *** The event time handling is not really nice yet...
lastWeek = sosCreateEventTimeList(sosCreateTimePeriod(sos = airsos,
				begin = as.POSIXct(Sys.time() - 3600*24*7),
				end = as.POSIXct(Sys.time())))

# if you want to see what get's in and out, just set "inspect" flag to TRUE
observation.pm10.week <- getObservation(sos = airsos,
		offering = airsos.off.pm10,
#		observedProperty = airsos.obsProp.pm10, # not needed, taken from the offering as default
		eventTime = lastWeek,
		procedure = sosProcedures(airsos.off.pm10)
#		inspect = TRUE
)

# explore the returned observation collection:
observation.pm10.week
observation.pm10.week[[1]]
features <- levels(result.pm10.week$feature)
features

# sosResult currently does the same as as.data.frame for single observations
# and binds the observation together for observation collections:
as.data.frame(observation.pm10.week[[1]]); sosResult(observation.pm10.week[[1]])
result.pm10.week <- sosResult(observation.pm10.week)

# sort 
sort_df(result.pm10.week, names(result.pm10.week)[3])
sort_df(result.pm10.week, "Time")

# subset that data frame
subset(result.pm10.week, feature=="foi_AT10001")
subset(result.pm10.week, feature=="foi_AT10001" & PM10 > 12)

# subset the observation collection based on features, observed properties and 
# procedures
observation.pm10.week[features[1:4]]
observation.pm10.week[sosObservedProperties(observation.pm10.week)[2]]
observation.pm10.week[sosProcedures(observation.pm10.week)[2:4]]

#
result.pm10.week.split = split(result.pm10.week, result.pm10.week$feature)

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
lastYear = sosCreateEventTimeList(sosCreateTimePeriod(sos = airsossos,
				begin = as.POSIXct(Sys.time() - 3600*24*365),
				end = as.POSIXct(Sys.time())))
observation.pm10.year <- getObservation(sos = airsossos,
		offering = airsos.off.pm10,
#		observedProperty = airsos.obsProp.pm10, # not needed, taken from the offering as default
		eventTime = lastYear,
		procedure = sosProcedures(airsos.off.pm10)[c(1:100)])


###################
# OBSERVATIONS: NO2

observation.no2.week <- getObservation(sos = airsos,
		offering = airsos.off.no2,
#		observedProperty = airsos.obsProp.pm10, # not needed, taken from the offering as default
		eventTime = lastWeek,
		procedure = sosProcedures(airsos.off.no2)
#		inspect = TRUE
)

# ! DN: There is some problem here if requestion a lot of procedures (> about 177) at once, I'm looking into that.

observation.no2.year <- getObservation(sos = airsossos,
		offering = airsos.off.no2,
#		observedProperty = airsos.obsProp.pm10, # not needed, taken from the offering as default
		eventTime = lastYear,
		procedure = sosProcedures(airsos.off.no2)[1:180],
		verbose = TRUE)

# observations from complete time period
timePeriod.no2 <- sosTime(airsos.off.no2)
observation.no2.all <- getObservation(sos = airsossos,
		offering = airsos.off.no2,
#		observedProperty = airsos.obsProp.pm10, # not needed, taken from the offering as default
		eventTime = sosCreateEventTimeList(timePeriod.no2),
		procedure = sosProcedures(airsos.off.no2)[1:20])


# make visualization example following
# http://spatial-analyst.net/wiki/index.php?title=Export_maps_to_GE

