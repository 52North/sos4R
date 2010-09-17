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
# Created: 2010-06-18                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

# Snippets used (mainly) in the blog
################################################################################

# using download to file
download.file(
		url = "http://giv-sos.uni-muenster.de:8080/52nSOSv3/sos?request=GetCapabilities&version=1.0.0&service=SOS",
		destfile = "caps.xml")

# using RCurl
library(RCurl)
caps <- getURL("http://giv-sos.uni-muenster.de:8080/52nSOSv3/sos?request=GetCapabilities&version=1.0.0&service=SOS")
print(caps)

################################################################################
# parsing ISO 8601 time

# Using basic R functions to create a POSIXt object from a ISO 8601 string.
x <- "2005-12-05T01:17:42.999+02:00"
t <- strptime(x, "%Y-%m-%dT%H:%M:%S")
# "2005-12-05 01:17:42" -- Does not handle fractions of seconds and time zone.

# tz	A timezone specification to be used for the conversion. System-specific (see as.POSIXlt), but "" is the current time zone, and "GMT" is UTC.
t <- strptime(x, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
# "2005-12-05 01:17:42 GMT" -- This is OK, so I just ignore the time zone designator!

?strptime # gives the answer to the missing milliseconds:
# 'Specific to R is %OSn, which for output gives the seconds to 0 <= n <= 6 decimal places (and if %OS is not followed by a digit, it uses the setting of getOption("digits.secs"), or if that is unset, n = 3). Further, for strptime %OS will input seconds including fractional seconds. Note that %S ignores (and not rounds) fractional parts on output.'
t <- strptime(x, "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
# "2005-12-05 01:17:42 UTC" -- Milliseconds are not shown... yet.
format(t, "%Y-%m-%dT%H:%M:%OS3")
# to show the milliseconds, use the option with n manually set
# "2005-12-05T01:17:42.999" -- Done.

#
#
#
parseMeasurement <- function(measurement, timeFormat = sosDefaultTimeParsingFormat) {
	.samplingTime <- parseSamplingTime(measurement[["samplingTime"]], sos)
# [...]
	
# To replace the format, set the default...
sosDefaultTimeParsingFormat <- "%Y/%m/%d"
# ... or wrap the parsing function in you own function

myParseMeasurement <- function(measurement) {
	return(parseMeasurement(measurement, "%Y/%m/%d"))
}
sos <- SOS(url = "http://v-swe.uni-muenster.de/WeatherSOS",
		parsers = SOSParsers("Measurement" = myParseMeasurement))

