# Copyright (C) 2010 by 52 North Initiative for Geospatial Open Source Software GmbH, Contact: info@52north.org
# This program is free software; you can redistribute and/or modify it under the terms of the GNU General Public License version 2 as published by the Free Software Foundation. This program is distributed WITHOUT ANY WARRANTY; even without the implied WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program (see gpl-2.0.txt). If not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or visit the Free Software Foundation web page, http://www.fsf.org.
# Author: 	Daniel Nuest (https://github.com/nuest/)
#						Paul Breen (https://github.com/paul-breen/)
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r
library("sos4R")

###############################################################################
# SOS @ BAS
# project homepage: http://basmet.nerc-bas.ac.uk/sos/
url <- "http://sosmet.nerc-bas.ac.uk:8080/sosmet/sos"
bas <- SOS(url = url)
bas
sosContents(bas)


# WPS using sos4R: http://sosmet.nerc-bas.ac.uk:8080/wpsmet/WebProcessingService?Request=GetCapabilities&Service=WPS

# WPS processes for monthly means:
# description: http://sosmet.nerc-bas.ac.uk:8080/wpsmet/WebProcessingService?Request=GetCapabilities&Service=WPS
# script file: http://sosmet.nerc-bas.ac.uk:8080/wpsmet/R/scripts/metMonthlyMeans.R

# default values:
offeringId <- "HalleyMet"
observedPropertyId <- "urn:ogc:def:phenomenon:OGC:1.0.30:air_temperature"
year <- "2013"

### BEGIN content of the script > the analysis
SOS_UOM_UNHANDLED_TYPES <- list("mbar"=sosConvertDouble, "okta"=sosConvertDouble, "code"=sosConvertDouble);
SOS_METADATA_UOM_TEXT <- "unit of measurement";

# The SOS library doesn't know how to handle the following units, so we have
# to explicitly tell it.  Note: the function call sosDataFieldConverters(sos)
# will list all units handled by the library
sos.conv <- do.call(SosDataFieldConvertingFunctions, SOS_UOM_UNHANDLED_TYPES);

# Instantiate an SOS object
sos <- SOS(url = url, dataFieldConverters=sos.conv);

# Get a particular offering from those offered by this SOS
off <- sosOfferings(sos)[[offeringId]];

# Specify a temporal filter
# N.B.: Fix up the datetimes.  We subtract a ms from the start, because the
#       SOS uses a semi-closed interval (start,end], & replace the date/time
#       separator with a space
startDatetime <- paste(year, "01-01 00:00:00.000Z", sep="-");
startDatetime <- as.character(as.POSIXct(startDatetime) - 1e-3);
endDatetime <- paste(year, "12-31 23:59:59.999Z", sep="-");
interval <- paste(startDatetime, endDatetime, sep="::");

# Get the specified data
obs <- getObservation(sos=sos, off, observedProperty=as.list(observedPropertyId), eventTime=sosCreateTime(sos=sos, time=interval));
obs.result <- sosResult(obs);

# Store some metadata for convenience
offering.name <- sosName(off);
observedProperty.name <- names(sosResult(obs))[[2]];
observedProperty.uom <- attr(sosResult(obs)[[2]], SOS_METADATA_UOM_TEXT);

# Calculate monthly means & package up as a data.frame
f <- factor(as.POSIXlt(obs.result$SamplingTime)$mon+1);
means <- tapply(obs.result[[2]], f, mean);
sds <- tapply(obs.result[[2]], f, sd);
nsamples <- tapply(obs.result[[2]], f, length);
sems <- (sds / sqrt(nsamples));

months <- levels(f);
years <- rep(year, times=length(months));

df <- data.frame("year"=years, "month"=months, "mean"=means, "sd"=sds, "nsamples"=nsamples, "sem"=sems);

### END of script copy
str(df)
plot(df)
#plot(off)

# mean data
df
plot(df$mean)

# original data
str(obs.result)
plot(obs.result, pch=".")

# TODO combine plots
