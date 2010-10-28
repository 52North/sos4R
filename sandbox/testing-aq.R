################################################################################
# Copyright (C) 2010 by 52 North                                               #
# Initiative for Geospatial Open Source Software GmbH                          #
#                                                                              #
# Contact: Andreas Wytzisk                                                     #
# 52 North Initiative for Geospatial Open Source Software GmbH                 #
# Martin-Luther-King-Weg 24                                                    #
# 48155 Muenster, Germany                                                      #
# info@52north.org                                                             #
#                                                                              #
# This program is free software; you can redistribute and/or modify it under   #
# the terms of the GNU General Publipc License version 2 as published by the    #
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
# Created: 2010-10-28                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

# load package from source
# source("<YOUR PATH>/sos4R/sandbox/loadSources.R")

################################################################################
# AirQuality
airsos <- SOS(url = "http://giv-sos.uni-muenster.de:8080/AirQualitySOS/sos")

airsos.offerings <- sosOfferings(airsos)

################################################################################
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
sosBoundedBy(eea.off.pm10) # or eea.off.pm10@boundedBy
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
		procedure = sosProcedures(eea.off.pm10)[c(1:42)],
		inspect = TRUE)
# in my case just 3 observations

# explore the returned observation collection:
observation.pm10.week
observation.pm10.week[[1]]

sosResult(observation.pm10.week)
result01 <- sosResult(observation.pm10.week[[1]])
# sosResult currently does the same as as.data.frame:
as.data.frame(observation.pm10.week[[1]])

# get observation metadata
attributes(result01)

plot(result01[["Time"]], result01[["PM10"]])

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

# ! DN: There is some problem here if requestion a lot of procedures (> 100) at once, I'm looking into that.

observation.no2.year <- getObservation(sos = eeasos,
		offering = eea.off.no2,
#		observedProperty = eea.obsProp.pm10, # not needed, taken from the offering as default
		eventTime = lastYear,
		procedure = sosProcedures(eea.off.no2)[100:300],
		verbose = TRUE)
# Not too much data:
# Finished getObservation to http://discomap-test.eea.europa.eu/swe/sos - received 107 observation(s)/measurement(s) having 3, 16, 24, 23, 8, 37, 42, 24, 24, 6, 25, 3, 59, 20, 58, 4, 27, 24, 28, 9, 1, 19, 73, 15, 79, 24, 4, 64, 15, 17, 27, 20, 23, 4, 1, 68, 23, 24, 24, 14, 24, 24, 6, 24, 26, 8, 13, 24, 17, 15, 24, 1, 37, 8, 19, 4, 24, 329, 30, 22, 15, 21, 1, 11, 24, 25, 63, 25, 16, 17, 25, 24, 26, 16, 25, 8, 1, 16, 20, 18, 60, 24, 85, 24, 24, 21, 127, 2, 24, 7, 7, 24, 13, 1, 9, 77, 63, 2, 10, 25, 22, 52, 3, 24, 27, 36, 12 elements.
observation.no2.year.count <- sum(sapply(sapply(sosResult(observation.no2.year), dim), "[", 1))
observation.no2.year.count

# observations from complete time period
timePeriod.no2 <- sosTime(eea.off.no2)
observation.no2.all <- getObservation(sos = eeasos,
		offering = eea.off.no2,
#		observedProperty = eea.obsProp.pm10, # not needed, taken from the offering as default
		eventTime = sosCreateEventTimeList(timePeriod.no2),
		procedure = sosProcedures(eea.off.no2)[1:200])

