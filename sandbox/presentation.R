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
source("/home/daniel/Dropbox/2010_SOS4R/workspace/sos4R/sandbox/loadSources.R")
################################################################################


################################################################################
# PegelOnlineSOS
pegelsos <- SOS(url = "http://v-sos.uni-muenster.de:8080/PegelOnlineSOSv2/sos")
print(object.size(pegelsos), units = c("Mb"))

# what data do I get?
names(sosOfferings(pegelsos))

# let's find interesting data
procs <- sosProcedures(pegelsos)
contain_Bake <- procs %in% grep("*Bake*", procs, value=TRUE)
baken <- subset(procs, contain_Bake)
baken
# Bake_Z
# http://www.pegelonline.wsv.de/gast/stammdaten?pegelnr=9510066

# what?
wasserstand <- sosObservedProperties(pegelsos)[3]
wasserstand_roh <- sosOfferings(pegelsos)[[2]]

# when?
tPeriod <- sosCreateEventTimeList(
	time = sosCreateTimePeriod(
		sos = pegelsos,
		begin = Sys.time() - (3600 * 6), # 24 * 30),
		end = Sys.time()))
str(tPeriod)
encodeXML(tPeriod[[1]], pegelsos)

# three procedures, but only getting 1 element with one procedure...
pegelObs <- getObservation(sos = pegelsos,
		observedProperty = wasserstand,
		offering = wasserstand_roh,
		procedure = baken[c(7, 9, 11)],
		eventTime = tPeriod, inspect = TRUE)

# show parts of the data frame:
pegelObs[[1]]@result[1:2,]
pegelObs[[2]]@result[1:2,]
pegelObs[[3]]@result[1:2,]

# not enough info? got field descriptions as attributes for each column:
attributes(pegelObs[[1]]@result$Time) # TIME
attributes(pegelObs[[1]]@result$Wasserstand) # actual value

# do something with the data!
r1 <- pegelObs[[1]]@result
range(r1$Wasserstand)
r1clean <- subset(r1, Wasserstand > 0)
range(r1clean$Wasserstand)
plot(r1clean$Time, r1clean$Wasserstand, type = "l", ylim=c(200,800))

# Plot a quantile regression line with standard error bounds, using the quantreg package.
library("ggplot2"); library("latticeExtra"); library("quantreg")
r1plot <- xyplot(r1clean$Wasserstand ~ r1clean$Time, r1clean, type = "l", col = "orange")
r1plot + layer(panel.quantile(x, y, tau = c(.95, .5, .05)))


#latestObs <- getObservation(sos = pegelsos,
#		observedProperty = wasserstand,
#		offering = sosOfferings(pegelsos)[[2]],
#		procedure = baken[11],
#		latest = TRUE) #, inspect = TRUE, verbose = TRUE)
#sosResult(latestObs)
#attributes(sosResult(latestObs)$Wasserstand)
#
# if returning reference
#latestObs <- getObservation(sos = pegelsos,
#		observedProperty = sosObservedProperties(pegelsos),
#		offering = sosOfferings(pegelsos)[[2]],
#		procedure = sosProcedures(pegelsos)[11],
#		latest = TRUE, inspect = TRUE) # , verbose = TRUE)
#sosResult(latestObs)

################################################################################
# OOSTethys SOS                                                                #
# http://www.oostethys.org/development/web-services/web-services-summary       #
################################################################################
# Sensor Observation Service (SOS) for Marine Metadata Interoperability
# Initiative (MMI) # Using GET works!
MBARI <- SOS("http://mmisw.org/oostethys/sos", method = "GET")

MBARI
sosOfferings(MBARI)
paste(sosProcedures(MBARI))
paste(sosObservedProperties(MBARI))

myOff <- sosOfferings(MBARI)[[1]]
myProc <- sosProcedures(MBARI)[3]

mbariObs <- getObservation(sos = MBARI, offering = myOff, procedure = myProc)
# responseFormat must be text/xml;subtype="sensorML/1.0.1"
#mbariObs <- getObservation(sos = MBARI, offering = sosOfferings(MBARI)[[1]],
#	procedure = sosProcedures(MBARI)[3],
#	responseFormat = 'text/xml;subtype="sensorML/1.0.1"')
# The responseFormat: text/xml;subtype="sensorML/1.0.1" is not supported.
# It should be text/xml;subtype="sensorML/1.0.1"
# without (even default) response format
#mbariObs <- getObservation(sos = MBARI, offering = sosOfferings(MBARI)[[1]],
#procedure = sosProcedures(MBARI)[3], responseFormat = NA_character_)

# add converter for UOM C (which is not a valid unit)
# http://www.unc.edu/~rowlett/units/dictC.html
# C [1] the Roman numeral 100...
# C [2] a symbol for international standard paper sizes...
# C [3] a unit of relative current for batteries...
myConverters <- SosFieldConvertingFunctions(
	# mapping for UOM:
	"C" = sosConvertDouble,
	"S/m" = sosConvertDouble,
	# mapping for definition:
	"http://mmisw.org/ont/cf/parameter/sea_water_salinity" = sosConvertDouble)
MBARI <- SOS("http://mmisw.org/oostethys/sos", method = "GET",
	dataFieldConverters = myConverters)
mbariObs <- getObservation(sos = MBARI, offering = myOff, procedure = myProc)

mbariObs@result[1:3,]
attributes(mbariObs@result$Temperature)
str(mbariObs@result)

# quick plot
plot(mbariObs@result)
plot(mbariObs@result[,c("Temperature", "Salinity", "Conductivity")])

# covariance, ...
cov(mbariObs@result[, 5:7])
# correlation
cor(mbariObs@result[, 5:7])


