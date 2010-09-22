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
# Author: Daniel Nüst (daniel.nuest@uni-muenster.de)                           #
# Created: 2010-06-20                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

################################################################################
source("/home/daniel/Dokumente/2010_SOS4R/workspace/sos4R/sandbox/loadSources.R")
################################################################################

################################################################################
# WeatherSOS
weathersos <- SOS(url = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos")


################################################################################
# PegelOnlineSOS
pegelsos <- SOS(url = "http://v-sos.uni-muenster.de:8080/PegelOnlineSOSv2/sos")

# works so far... :-)

latestObs <- getObservation(sos = pegelsos,
		observedProperty = sosObservedProperties(pegelsos)[1],
		offering = sosOfferings(pegelsos)[[1]],
		procedure = sosProcedures(pegelsos)[2501],  # Wasserstand-Stahlbrode_9650070
		latest = TRUE, inspect = TRUE)
latestObs@result

# three procedures, but only getting 1 element with one procedure...
sosSetFunctionsToDefault(pegelsos)
pegelObs <- getObservation(sos = pegelsos,
		observedProperty = sosObservedProperties(pegelsos)[1],
		offering = sosOfferings(pegelsos)[[1]],
		procedure = sosProcedures(pegelsos)[2503], #[c(2501,2503,2505)],
		eventTime = sosCreateEventTime(sosCreateTimePeriod(
						sos = pegelsos,
						begin = Sys.time() - (3600 * 24 * 360),
						end = Sys.time())))
#septemberObs@result[100:105,]

range(pegelObs@result[,3])
# [1] -1.00e+09  2.52e+02 -- weird results?

plot(x = pegelObs@result[,1], y = pegelObs@result[,3], type = "l")


# Good data?
# Felix's tip: look at the coastal stations, much more interesting!

################################################################################
# AirQualitySOS
airsos <- SOS(url = "http://v-sos.uni-muenster.de:8080/AirQualityEurope/sos")

ozone <- sosOfferings(airsos)[["OZONE_(AIR)"]]
ozone@observedProperty
ozone@procedure

latestOzone <- getObservation(sos = pegelsos,
		observedProperty = list(ozone@observedProperty),
		offering = ozone,
		procedure = list(ozone@procedure),
		latest = TRUE, inspect = TRUE)
#Object of class OwsExceptionReport; version: 1.0.0, lang: NA,  1 exceptions: (code @ locator : text)
#InvalidParameterValue @ offering : The value (OZONE_(AIR)) of the parameter 'offering' is invalid 

getObs <- airsos@capabilities@operations@operations[[sosGetObservationName]]
getObs@parameters[["offering"]]
# OZONE_(AIR) is there...


# the time is not given in observation offerings...
sosOfferings(airsos)[[1]]@time


################################################################################
# "catchall" SOS
givsos <- SOS("http://giv-sos.uni-muenster.de:8080/52nSOSv3/sos")

# TODO fix errors when requesting capabilities from givsos

################################################################################
# MoodSOS
#
# offerings worth checking out: SummerVillerest, PatientCondition,
# WaterColourNormal, FoamPresence, Microcystin

moodsos <- SOS("http://giv-genesis.uni-muenster.de:8080/52nSOSv3-MoodSOS/sos")

################################################################################
# 
umweltsos <- SOS(url = "https://develop.umweltbundesamt.at/SOSsrv/sos")
#https://develop.umweltbundesamt.at/SOSsrv/sos?service=SOS&request=GetCapabilities
# 503 Service temporarily unavailable
