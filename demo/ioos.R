# Copyright (C) 2010 by 52 North Initiative for Geospatial Open Source Software GmbH, Contact: info@52north.org
# This program is free software; you can redistribute and/or modify it under the terms of the GNU General Public License version 2 as published by the Free Software Foundation. This program is distributed WITHOUT ANY WARRANTY; even without the implied WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program (see gpl-2.0.txt). If not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or visit the Free Software Foundation web page, http://www.fsf.org.
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r

##############################################################################
# IOOS
# http://sdf.ndbc.noaa.gov/sos/
ioos.get <- SOS(url = "http://sdf.ndbc.noaa.gov/sos/server.php",
		method = SosSupportedConnectionMethods()[["GET"]],
		timeFormat = "%Y-%m-%dT%H:%M:%SZ")
#		parsers = SosParsingFunctions("GetObservation" = parseNoParsing)

ioos.off <- sosOfferings(ioos.get)
names(ioos.off)

ioos.procedures <- sosProcedures(ioos.get)
length(ioos.procedures)

# most recent:
#http://sdf.ndbc.noaa.gov/sos/server.php?request=GetObservation&service=SOS&
#		offering=urn:ioos:station:wmo:46403&
#		observedproperty=sea_floor_depth_below_sea_surface&
#		responseformat=text/csv
obs.csv <- getObservation(ioos.get, offering = sosName(ioos.off[[100]]),
		responseFormat = "text/csv",
		observedProperty = sosObservedProperties(ioos.off[[100]])[2],
		verbose = TRUE, saveOriginal = TRUE)
obs.csv
# GET WORKS!

# requires SensorML 1.0.0
describeSensorOp <- sosOperation(ioos.get, sosDescribeSensorName)
describeSensor.outputFormat <- describeSensorOp@parameters[["outputFormat"]][[1]]
ioos.sensor.1.1 <- describeSensor(sos = ioos.get, procedure = ioos.procedures[[1]][[1]],
		outputFormat = describeSensor.outputFormat)
ioos.sensor.1.1@xml

#obs <- parseCSV(obs.csv)

# point in time:
#http://sdf.ndbc.noaa.gov/sos/server.php?request=GetObservation&service=SOS&
#		offering=urn:ioos:station:wmo:46403&
#		observedproperty=sea_floor_depth_below_sea_surface&
#		responseformat=text/csv&
#		eventtime=2008-07-17T00:00Z

# time interval:
#http://sdf.ndbc.noaa.gov/sos/server.php?request=GetObservation&service=SOS&
#		offering=urn:ioos:station:wmo:46403&
#		observedproperty=sea_floor_depth_below_sea_surface&
#		responseformat=text/csv&
#		eventtime=2008-07-17T00:00Z/2008-07-17T23:59Z

# mine:
#http://sdf.ndbc.noaa.gov/sos/server.php?service=SOS&request=GetObservation&
#		version=1.0.0&
#		offering=urn%3Aioos%3Astation%3Awmo%3A42056&
#		observedProperty=http%3A//mmisw.org/ont/cf/parameter/sea_water_temperature&
#		responseFormat=text/csv&
#		eventTime=2008-07-17T00%3A00%3A00Z/2008-07-17T00%3A00%3A00Z 

ioos.get.interval <- getObservation(ioos.get,
		offering = sosName(ioos.off[[100]]),
		responseFormat = "text/csv",
		observedProperty = sosObservedProperties(ioos.off[[100]])[3],
		eventTime = sosCreateEventTimeList(
				sosCreateTimePeriod(sos = ioos.get,
						begin = as.POSIXct("2008-07-17T00:00Z"),
						end = as.POSIXct("2008-07-17T23:59Z")))
)
#		, verbose = TRUE)

# time interval, bbox
#http://sdf.ndbc.noaa.gov/sos/server.php?request=GetObservation&service=SOS&
#		offering=urn:ioos:network:noaa.nws.ndbc:all&
#		featureofinterest=BBOX:-90,25,-65,35&
#		observedproperty=sea_floor_depth_below_sea_surface&
#		responseformat=text/csv&
#		eventtime=2008-07-17T00:00Z/2008-07-17T23:59Z
# bounding box is defined in feature of interest...


################################################################################
# POST
# http://sdf.ndbc.noaa.gov/sos/test.shtml
ioos.post <- ioos <- SOS(url = "http://sdf.ndbc.noaa.gov/sos/server.php",
		timeFormat = "%Y-%m-%dT%H:%M:%SZ")
ioos.post.off <- sosOfferings(ioos.post)
obs.csv.post <- getObservation(ioos.post,
		offering = sosName(ioos.post.off[[100]]),
		responseFormat = "text/csv",
		observedProperty = sosObservedProperties(ioos.post.off[[100]])[1],
		verbose = TRUE)
obs.csv.post

# time series
#begin <- sosTime(ioos.post.off[[1]], convert = TRUE)[[1]]
end <- as.POSIXct(Sys.time())
begin <- end - 3600 * 24 * 30

.offering <- sosOfferings(ioos.post, name = sosName(ioos.post.off[[1]]))
.offeringId <- sosId(.offering)
obs.001 <- getObservation(sos = ioos.post,
		offering = sosName(.offering), # "urn:ioos:network:noaa.nws.ndbc:all"
		procedure = sosProcedures(ioos.post.off[[1]])[690:700],
		observedProperty = sosObservedProperties(ioos.post.off[[1]])[6:7],
		responseFormat = "text/csv",
		eventTime = sosCreateEventTimeList(
				sosCreateTimePeriod(sos = ioos.post, begin = begin, end = end)),
		inspect = TRUE, verbose = TRUE)



################################################################################
# another SOS from IOOS
# https://geossregistries.info/geosspub/component_details_ns.jsp?compId=urn:uuid:c1af67f9-4a1b-42d2-b352-e2fdb3bcdeb1
# request examples at http://opendap.co-ops.nos.noaa.gov/ioos-dif-sos/
# Also good information about existing/active stations!
ioosdif <- SOS(url = "http://opendap.co-ops.nos.noaa.gov/ioos-dif-sos/SOS")
#		method = "GET",
#		verboseOutput = TRUE)
ioosdif.off <- sosOfferings(ioosdif)
sosName(ioosdif.off)

length(ioosdif.off)
# 771

# TODO check out ioosdif

sosResponseFormats(ioosdif)
# TODO write parser for ioos:Composite results, do here to test exchangeability!
		

