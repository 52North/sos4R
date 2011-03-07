# Copyright (C) 2010 by 52 North Initiative for Geospatial Open Source Software GmbH, Contact: info@52north.org
# This program is free software; you can redistribute and/or modify it under the terms of the GNU General Public License version 2 as published by the Free Software Foundation. This program is distributed WITHOUT ANY WARRANTY; even without the implied WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program (see gpl-2.0.txt). If not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or visit the Free Software Foundation web page, http://www.fsf.org.
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r
library("sos4R")

##############################################################################
# Coastlab SOS - http://www.coastlab.org/
#
# This SOS provides COSYNA data, see the project website at
# http://www.hzg.de/institute/coastal_research/structure/operational_systems/KOK/projects/ICON/index.html
#
# GEOSS description:
# https://geossregistries.info/geosspub/service_details_ns.jsp?serviceId=urn:uuid:b7e0c9d4-9c4a-428c-adb9-fc0efebc9798
#
# Data disclaimer: http://www.coastlab.org/Disclaimer.html
#
# Web Portal: http://kofserver2.hzg.de/codm/
# You can also plot the data here: http://tsdata.hzg.de/index.cgi?seite=plot_form
#
coastlab <- SOS(url = "http://kopc02.gkss.de/sos/sos.py",
		method = SosSupportedConnectionMethods()[["GET"]])

coastlab.off <- sosOfferings(coastlab)
names(coastlab.off)

jade1 <- coastlab.off[["Pile_Jade1"]]
sosObservedProperties(jade1)

jade1.watertemp <- getObservation(sos = coastlab, offering = jade1,
		observedProperty = list("WaterTemperature"), verbose = TRUE,
		eventTime = sosCreateEventTimeList(sosCreateTimePeriod(
						sos = coastlab,
						begin = as.POSIXct(Sys.time() - 3600 * 24 * 180),
						end = as.POSIXct(Sys.time()))))

# TODO unhandled response document, it contains om:resultDefinition ...

# TODO create parsing function for om:resultDefinition with exchangeability feature!




jade1.watertemp.result <- sosResult(jade1.watertemp)
summary(jade1.watertemp.result)