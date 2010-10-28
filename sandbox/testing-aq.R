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


################################################################################
# AirQuality
airsos <- SOS(url = "http://giv-sos.uni-muenster.de:8080/AirQualitySOS/sos")

airsos.offerings <- sosOfferings(airsos)

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
# EEA SOS
eaasos <- SOS(url = "http://discomap-test.eea.europa.eu/swe/sos")
