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
# Created: 2010-06-18                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

# load required libraries
library("XML")
library("RCurl")
library("sp")

# load required source files for testing
if(!exists(".path"))
	.path = "/home/daniel/Dokumente/2010_SOS4R/workspace/sos4R"

source(paste(.path, "R", "Constants.R",  sep = "/"))

source(paste(.path, "R", "Class-OWS.R",  sep = "/"))
source(paste(.path, "R", "Class-GML.R",  sep = "/"))
source(paste(.path, "R", "Class-SWE.R",  sep = "/"))
source(paste(.path, "R", "Class-OM.R",  sep = "/"))
source(paste(.path, "R", "Class-SA.R",  sep = "/"))
source(paste(.path, "R", "Class-OGC.R",  sep = "/"))
source(paste(.path, "R", "Class-SOS.R",  sep = "/"))
source(paste(.path, "R", "Class-SML.R",  sep = "/"))

source(paste(.path, "R", "Generic-methods.R",  sep = "/"))
source(paste(.path, "R", "OWS-methods.R",  sep = "/"))
source(paste(.path, "R", "OWS-methods-parsing.R",  sep = "/"))
source(paste(.path, "R", "SOS-methods-parsing.R",  sep = "/"))
source(paste(.path, "R", "SOS-methods-plotting.R",  sep = "/"))
source(paste(.path, "R", "OM-methods.R",  sep = "/"))
source(paste(.path, "R", "OM-methods-parsing.R",  sep = "/"))
source(paste(.path, "R", "OM-methods-coercion.R",  sep = "/"))
source(paste(.path, "R", "SA-methods.R",  sep = "/"))
source(paste(.path, "R", "GML-methods.R",  sep = "/"))
source(paste(.path, "R", "SWE-methods.R",  sep = "/"))
source(paste(.path, "R", "SML-methods.R",  sep = "/"))
source(paste(.path, "R", "GML-methods-parsing.R",  sep = "/"))
source(paste(.path, "R", "SA-methods-parsing.R",  sep = "/"))
source(paste(.path, "R", "SWE-methods-parsing.R",  sep = "/"))
source(paste(.path, "R", "OGC-methods.R",  sep = "/"))

source(paste(.path, "R", "PrintShowStructureSummmary-methods.R",  sep = "/"))

source(paste(.path, "R", "SOS-methods-coercion.R",  sep = "/"))
source(paste(.path, "R", "SML-methods-coercion.R",  sep = "/"))

source(paste(.path, "R", "SOS-methods-util.R",  sep = "/"))
source(paste(.path, "R", "SML-methods-util.R",  sep = "/"))

source(paste(.path, "R", "SOS-methods.R",  sep = "/"))

source(paste(.path, "R", "Defaults.R",  sep = "/"))