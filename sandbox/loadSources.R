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

# load required libraries
library("XML")
library("RCurl")

# load required source files for testing
path = "/home/daniel/Dropbox/2010_SOS4R/workspace/sos4R"

# define url for testing
climatesos = "http://giv-sos.uni-muenster.de:8080/ClimateSOS/sos"

source(paste(path, "R", "Class-OWS.R",  sep="/"))
source(paste(path, "R", "OWS-methods.R",  sep="/"))

source(paste(path, "R", "Class-DescribeSensor.R",  sep="/"))
source(paste(path, "R", "Class-GetObservation.R",  sep="/"))
source(paste(path, "R", "Class-SOS.R",  sep="/"))
source(paste(path, "R", "Class-SensorML.R",  sep="/"))

source(paste(path, "R", "DescribeSensor-methods.R",  sep="/"))
source(paste(path, "R", "GetObservation-methods.R",  sep="/"))
source(paste(path, "R", "SOS-methods.R",  sep="/"))
source(paste(path, "R", "SensorML-methods.R",  sep="/"))

source(paste(path, "R", "PrintShowStructureSummmary-methods.R",  sep="/"))

