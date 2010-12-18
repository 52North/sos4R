# Copyright (C) 2010 by 52 North Initiative for Geospatial Open Source Software GmbH, Contact: info@52north.org
# This program is free software; you can redistribute and/or modify it under the terms of the GNU General Public License version 2 as published by the Free Software Foundation. This program is distributed WITHOUT ANY WARRANTY; even without the implied WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program (see gpl-2.0.txt). If not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or visit the Free Software Foundation web page, http://www.fsf.org.
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r

##############################################################################
# IOOS
ioos <- SOS(url = "http://sdf.ndbc.noaa.gov/sos/server.php",
		method = SosSupportedConnectionMethods()[["GET"]])

ioos.off <- sosOfferings(ioos)
names(ioos.off)

ioos.procedures <- sosProcedures(ioos)
length(ioos.procedures)

# requires SensorML 1.0.0
describeSensorOp <- sosOperation(ioos, sosDescribeSensorName)
describeSensor.outputFormat <- describeSensorOp@parameters[["outputFormat"]][[1]]
ioos.sensor.1.1 <- describeSensor(sos = ioos, procedure = ioos.procedures[[1]][[1]],
		outputFormat = describeSensor.outputFormat)
ioos.sensor.1.1@xml


# TODO