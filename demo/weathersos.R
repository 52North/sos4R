# Copyright (C) 2010 by 52 North Initiative for Geospatial Open Source Software GmbH, Contact: info@52north.org
# This program is free software; you can redistribute and/or modify it under the terms of the GNU General Public License version 2 as published by the Free Software Foundation. This program is distributed WITHOUT ANY WARRANTY; even without the implied WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program (see gpl-2.0.txt). If not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or visit the Free Software Foundation web page, http://www.fsf.org.
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r

# establish a connection to a SOS instance
weathersos <- SOS(url = "http://v-swe.uni-muenster.de:8080/WeatherSOS/sos")


# get the latest observation (not standard conform!)
obs <- getObservation(sos = weathersos,
		observedProperty = sosObservedProperties(weathersos)[4],
		procedure = sosProcedures(weathersos)[1],
		offering = sosOfferings(weathersos)[[4]],
		latest = TRUE)

# show the result
obs@result


# Two procedures, including plot
#
# Attention: plots ignore the fact that the times do NOT perfectly match!
#
obs <- getObservation(sos = weathersos,
		observedProperty = sosObservedProperties(weathersos)[5],
		procedure = sosProcedures(weathersos),
		eventTime = sosCreateEventTimeList(sosCreateTimePeriod(sos = weathersos,
						begin = as.POSIXct("2009-08-10 12:00"),
						end = as.POSIXct("2009-08-20 12:00"))),
		#featureOfInterest = foiBBox,
		offering = sosOfferings(weathersos)[[5]])
str(obs[[1]]@result)
str(obs[[2]]@result)

# plot it!
x <- 800
plot(x = obs[[1]]@result[[1]][1:x], y = obs[[1]]@result[[3]][1:x], type = "l",
		col = "steelblue", main = "Temperature in Münster and Kärnten, 2009",
		xlab = "Time (00:00 o'clock)",
		ylab = "Temperature (°C)",
		xaxt="n") # do not plot x-axis
r <- as.POSIXct(round(range(obs[[1]]@result[[1]]), "days"))
axis.POSIXct(side = 1, x = obs[[1]]@result[[1]][1:x], format = "%d. %h",
		at = seq(r[1], r[2], by="day"))
lines(x = obs[[2]]@result[[1]][1:x], y = obs[[2]]@result[[3]][1:x],
		col = "orange")
legend("topleft", legend = c("Münster", "Kärnten"),
		col = c("steelblue", "orange"), lty = 1, bty="n")


#
# TODO using zoo with data from SOS
#

#
# TODO using sp with data from SOS
#
