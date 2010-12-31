# Copyright (C) 2010 by 52 North Initiative for Geospatial Open Source Software GmbH, Contact: info@52north.org
# This program is free software; you can redistribute and/or modify it under the terms of the GNU General Public License version 2 as published by the Free Software Foundation. This program is distributed WITHOUT ANY WARRANTY; even without the implied WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program (see gpl-2.0.txt). If not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or visit the Free Software Foundation web page, http://www.fsf.org.
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r

##############################################################################
# IOOS
# http://sdf.ndbc.noaa.gov/sos/
ioos.get <- SOS(url = "http://sdf.ndbc.noaa.gov/sos/server.php",
		method = SosSupportedConnectionMethods()[["GET"]],
		parsers = SosParsingFunctions("GetObservation" = parseSosNoParsing))

ioos.off <- sosOfferings(ioos)
names(ioos.off)

ioos.procedures <- sosProcedures(ioos)
length(ioos.procedures)

# requires SensorML 1.0.0
describeSensorOp <- sosOperation(ioos.get, sosDescribeSensorName)
describeSensor.outputFormat <- describeSensorOp@parameters[["outputFormat"]][[1]]
ioos.sensor.1.1 <- describeSensor(sos = ioos, procedure = ioos.procedures[[1]][[1]],
		outputFormat = describeSensor.outputFormat)
ioos.sensor.1.1@xml

# TODO
begin <- sosTime(ioos.off[[1]], convert = TRUE)[[1]]
end <- as.POSIXct(Sys.time())

# most recent:
#http://sdf.ndbc.noaa.gov/sos/server.php?request=GetObservation&service=SOS&
#		offering=urn:ioos:station:wmo:46403&
#		observedproperty=sea_floor_depth_below_sea_surface&
#		responseformat=text/csv
obs.csv <- getObservation(ioos.get, offering = sosName(ioos.off[[100]]),
		responseFormat = "text/csv",
		observedProperty = sosObservedProperties(ioos.off[[100]])[1],
		verbose = TRUE)
#save(obs.csv, file = "temp.csv", ascii = TRUE)
#write.csv(obs.csv, "temp.csv")
#read.csv("temp.csv")
#read.csv(file = obs.csv)
# does not work

obs <- parseCSV(obs.csv)

#
# TODO write to r-help on parsing csv from object
#
parseCSV <- function(obj) {
	lines <- strsplit(x = obs.csv, split = "\n")[[1]]
	data <- do.call(what = "strsplit", args = list(lines, split = ","))
	
	# clean up names (double quotes)
	.names <- data[[1]]
	.newNames <- c()
	for (.n in .names) {
		.newNames <- c(.newNames,
				gsub(pattern = "\"", replacement = "", x = .n))
	}
	.names <- .newNames
	
	.rows <- length(data)
	df <- NULL
	for (.r in seq(2,.rows)) {
		# initialize first column of the data frame so it can be bound in loop
		
		# TODO add parsers based on field names
		.row.df <- as.data.frame(data[[.r]][1])
		names(.row.df) <- .names[[1]]
		
		for (i in seq(2,length(.names))) {
			.df <- as.data.frame(data[[.r]][i])
			names(.df) <- .names[[i]]
			.row.df <- cbind(.row.df, .df)
		}
#		print(paste("row", .r))
#		print(.row.df)
		
		if(is.null(df))
			df <- .row.df
		else
			df <- do.call(rbind, list(df, .row.df))
	}
	
	return(df)
}


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


################################################################################
# POST
# http://sdf.ndbc.noaa.gov/sos/test.shtml
ioos.post <- ioos <- SOS(url = "http://sdf.ndbc.noaa.gov/sos/server.php",
		parsers = SosParsingFunctions("GetObservation" = parseSosNoParsing))
obs.csv.post <- getObservation(ioos.post, offering = sosName(ioos.off[[100]]),
		responseFormat = "text/csv",
		observedProperty = sosObservedProperties(ioos.off[[100]])[1],
		verbose = TRUE)
obs.post <- parseCSV(obs.csv.post)
obs.post
str(obs.post)

write(obs.csv.post, "test.txt")
