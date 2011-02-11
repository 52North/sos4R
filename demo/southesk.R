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
# Created: 2011-02-04                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

################################################################################
# SOS @ CSIRO
# The South Esk test bed
cat("Go to the following website for details of the South Esk Hydrological Sensor Web - Tasmania, Australia: ", "http://www.csiro.au/sensorweb/au.csiro.OgcThinClient/OgcThinClient.html")

# Data subject to CSIRO's legal disclaimer: http://www.csiro.au/org/LegalNoticeAndDisclaimer.html

# See also:
# http://external.opengis.org/twiki_public/bin/view/ClimateChallenge2009/ServiceOfferingCSIRO

# Bureau of Meteorology (red and dark blue on map)
bom <- SOS("http://wron.net.au/BOM_SOS/sos")

# Hydro Tasmania Consulting (yellow on map)
ht <- SOS("http://140.79.3.21/HT_SOS/sos")
# not available... maybe in offering in CSIRO_SOS

# Forestry Tasmania (green on map)
ft <- SOS("http://140.79.3.21/Forestry_SOS/sos")
# not available...
# rainfalltoday

# CSIRO (orange, purple on map)
# rainfalltoday?
csiro <- SOS("http://wron.net.au/CSIRO_SOS/sos")

# Tasmania Department of Primary Industries, Parks, Wildlife and Environment (DPIPWE, white on map)
# http://140.79.3.21/DPIPWE_SOS/sos
dpiw <- SOS("http://wron.net.au/DPIW_SOS/sos")

# phenomenon rainfall or rainfalltoday is available at all stations
rainfall <- "urn:ogc:def:phenomenon:OGC:rainfall"
rainfall.off.bom <- sosOfferings(bom)[[1]]
plot(rainfall.off.bom, regions = "Australia", map.cities = TRUE)

rainfall.off.csiro <- sosOfferings(csiro)["Rain Gauges"][[1]]
plot(rainfall.off.csiro, map.cities = FALSE, off.col = "green", add = TRUE)

rainfall.off.ht <- sosOfferings(csiro)["HT Weather Stations"][[1]]
plot(rainfall.off.ht, add = TRUE, off.col = "blue")

# plot all together
plot(csiro, regions = "Australia")
plot(csiro, regions = "Australia:Tasmania")
plot(bom)

lastWeek.bom <- sosCreateTimePeriod(sos = bom, begin = (Sys.time() - 3600 * 24),
		end = Sys.time())

# make analysis
