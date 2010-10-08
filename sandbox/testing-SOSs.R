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
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)                          #
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
print(object.size(pegelsos), units = c("Mb"))
# works so far... :-)

latestObs <- getObservation(sos = pegelsos,
		observedProperty = sosObservedProperties(pegelsos),
		offering = sosOfferings(pegelsos)[[2]],
		procedure = sosProcedures(pegelsos)[11],
		latest = TRUE) #, inspect = TRUE, verbose = TRUE)
sosResult(latestObs)


# three procedures, but only getting 1 element with one procedure...
pegelsos <- SOS(url = "http://v-sos.uni-muenster.de:8080/PegelOnlineSOSv2/sos")
pegelObs <- getObservation(sos = pegelsos,
		observedProperty = sosObservedProperties(pegelsos)[3],
		offering = sosOfferings(pegelsos)[[2]],
		procedure = sosProcedures(pegelsos)[c(2503)], #[c(2501,2503,2505)],
		eventTime = sosCreateEventTimeList(time = sosCreateTimePeriod(
						sos = pegelsos,
						begin = Sys.time() - (3600 * 24), # * 360),
						end = Sys.time())), verbose = TRUE)
# Finished getObservation to http://v-sos.uni-muenster.de:8080/PegelOnlineSOSv2/sos - received 2 observation(s)/measurement(s) having 1372 1372 elements.
# YEAH!

# show parts of the data frame:
pegelObs[[1]]@result[1:5,]

# not enough info? got field descriptions as attributes for each column:
attributes(pegelObs[[1]]@result[,1])
attributes(pegelObs[[1]]@result[,2])
attributes(pegelObs[[1]]@result[,3])


# TODO make plot out of two or three related stations
range(pegelObs[[1]]@result[,3]); range(pegelObs[[2]]@result[,3])

# Attention: plots ignore the fact that the times do NOT perfectly match!
#x <- 700
#plot(x = obs4[[1]]@result[[1]][1:x], y = obs4[[1]]@result[[3]][1:x], type = "l",
#		col = "steelblue", main = "Temperature in Münster and Kärnten, 2009",
#		xlab = "Time (00:00 o'clock)",
#		ylab = "Temperature (°C)",
#		xaxt="n") # do not plot x-axis
#r <- as.POSIXct(round(range(obs4[[1]]@result[[1]]), "days"))
#axis.POSIXct(side = 1, x = obs4[[1]]@result[[1]][1:x], format = "%d. %h",
#		at = seq(r[1], r[2], by="day"))
#lines(x = obs4[[2]]@result[[1]][1:x], y = obs4[[2]]@result[[3]][1:x],
#		col = "orange")
#legend("topleft", legend = c("Münster", "Kärnten"),
#		col = c("steelblue", "orange"), lty = 1, bty="n")

plot(x = pegelObs[[1]]@result[,1], y = pegelObs[[2]]@result[,3], type = "l")

# Good data?
# Felix's tip: look at the coastal stations, much more interesting!

procedure <- "Wasserstand-Elster_501390"
elster <- getObservation(sos = pegelsos, offering = sosOfferings(pegelsos)[[2]],
		procedure = list(procedure))

################################################################################
# Elsterhochwasser, 30.09.2010
elster@result[1:3,]
range(elster@result$Wasserstand)
elsterClean <- subset(elster@result, Wasserstand > 0)

plot(x = elster@result$Time, y = elster@result$Wasserstand, ylim = c(100, 600),
		type = "l")

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
# ClimateSOS
climatesos <- SOS("http://giv-sos.uni-muenster.de:8080/ClimateSOS/sos")

length(sosProcedures(climatesos))
# 1440

lapply(sosOfferings(climatesos), slot, "name")

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
# Umweltbundesamt SOS
umweltsos <- SOS(url = "https://develop.umweltbundesamt.at/SOSsrv/sos")
# https://develop.umweltbundesamt.at/SOSsrv/sos?service=SOS&request=GetCapabilities
# --> 503 Service temporarily unavailable


################################################################################
# OOSTethys SOS                                                                #
# http://www.oostethys.org/development/web-services/web-services-summary       #
################################################################################
source("/home/daniel/Dokumente/2010_SOS4R/workspace/sos4R/sandbox/loadSources.R")
################################################################################

################################################################################
# Sensor Observation Service (SOS) for Marine Metadata Interoperability
# Initiative (MMI)
MBARI <- SOS("http://mmisw.org/oostethys/sos", method = "GET", verboseOutput = TRUE)
# Using POST: InvalidRequest @ NA : Not able to understand the operation. This service supports the following operations: GetCapabilities, DescribeSensor and, GetObservation 
# Using GET works!


################################################################################
# Ocean Process Analysis Laboratory, Institute for the Study of Earth, Oceans,
# and Space, University of New Hampshire SOS
COOA_UNH <- SOS("http://www.cooa.unh.edu/cgi-bin/sos/oostethys_sos")
# --> 500 Internal Server Error, and
# --> Capabilities are shown when opening the link above in a browser, but it
# has a strange version: <ows:ServiceTypeVersion>0.0.31</ows:ServiceTypeVersion>


# Gulf of Maine Ocean Observing System SOS
GoMOOS <- SOS("http://www.gomoos.org/cgi-bin/sos/oostethys_sos.cgi",
		version = "0.0.31", verboseOutput = TRUE)
# --> Capabilities are shown when opening the link above in a browser, but it
# has a strange version: <ows:ServiceTypeVersion>0.0.31</ows:ServiceTypeVersion>
# 
# --> Object of class OwsExceptionReport; version: 1.0.0, lang: NA,  1 exceptions (code @ locator : text):
#	MissingParamterValue @ service : No input parameters 


################################################################################
# others:
################################################################################
#
# TAMU <- SOS("http://vastserver.nsstc.uah.edu/vastGC/adcp", verboseOutput = TRUE)
# --> no response
#
# MVCO_WHOI <- SOS("http://mvcodata.whoi.edu/cgi-bin/sos/oostethys_sos")
# --> seems almost empty



################################################################################
source("/home/daniel/Dokumente/2010_SOS4R/workspace/sos4R/sandbox/loadSources.R")
################################################################################


################################################################################
# SOS @ CSIRO
# The South Esk test bed
bom <- SOS("http://wron.net.au/BOM_SOS/sos")
csiro <- SOS("http://wron.net.au/CSIRO_SOS/sos")
dpiw <- SOS("http://wron.net.au/DPIW_SOS/sos")

# ??
hutchins <- SOS("http://150.229.66.73/HutchinsSOS/sos")
elliotwsn <- SOS("http://150.229.66.73/ElliotWSNSOS/sos")

################################################################################
# some french sos, 52N, but just one week of data....
sandre <- SOS("http://services.sandre.eaufrance.fr/52nSOSv3/sos")

################################################################################
# OCEAN STUFF, a lot of interesting data!
#
# http://www.openioos.org/real_time_data/gm_sos.html
#
oceanwatch <- SOS("http://oceanwatch.pfeg.noaa.gov/pysos/sos_mysql2.py")
ww6 <- SOS("http://ww6.geoenterpriselab.com:8080/SOS_Weather/sos ")
sos-ws <- SOS("http://sos-ws.tamu.edu/tethys/tabs")
		
