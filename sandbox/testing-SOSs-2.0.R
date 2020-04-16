################################################################################
# Copyright (C) 2019 by 52 North                                               #
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
# Created: 2013-03-06                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
################################################################################

.sos4Rpath <- "D:/workspace-R/sos4R";
source("D:/workspace-R/sos4R/sandbox/loadSources.R")

twozero <- SOS(url = "http://localhost:8080/sos/sos/pox", version = "2.0",
		binding = "POX",
		dcpFilter = list("POX" = "/pox"))
summary(twozero)
sosCapabilitiesDocumentOriginal(twozero, verbose = TRUE)


################################################################################
# http://hydro-sos.niwa.co.nz/KiWIS/KiWIS
#
# Capabilities: http://hydro-sos.niwa.co.nz/KiWIS/KiWIS?SERVICE=SOS&Version=2.0.0&request=getcapabilities&datasource=0&observedProperty=http://hydro-sos.niwa.co.nz/parameters/Stage/Flow%20rating
source("D:/workspace-R/sos4R_0.3/sandbox/loadSources.R")
kiwis <- SOS(url = "http://hydro-sos.niwa.co.nz/KiWIS/KiWIS", version = "2.0",
		binding = "POX", 
		dcpFilter = list("POX" = "/pox"),
		verboseOutput = TRUE)
# probably only supports SOAP binding...

kiwiskvp <- SOS(url = "http://hydro-sos.niwa.co.nz/KiWIS/KiWIS", version = "2.0",
		binding = "KVP", 
		additionalKVPs = list("datasource" = "0"),
		verboseOutput = TRUE)
kiwiskvp
# had to implement additional request parameters

sosCapabilitiesDocumentOriginal(kiwiskvp)
# does not work because DCPs are missing > FIXME get getcap working if DCP is missing
 
sosProcedures(kiwiskvp)
# NA
sosContents(kiwiskvp)
# NULL
sosOfferings(kiwiskvp)
# NA

str(kiwiskvp)
#Formal class 'SOS_2.0' [package ".GlobalEnv"] with 15 slots
#..@ url                : chr "http://hydro-sos.niwa.co.nz/KiWIS/KiWIS"
#..@ binding            : chr "KVP"
#..@ curlHandle         :Formal class 'CURLHandle' [package "RCurl"] with 1 slots
#[...]
#..@ version            : chr "2.0"
#..@ capabilities       :Formal class 'SosCapabilities_1.0.0' [package ".GlobalEnv"] with 8 slots
#.. .. ..@ filterCapabilities: NULL
#.. .. ..@ identification    :Formal class 'OwsServiceIdentification' [package ".GlobalEnv"] with 8 slots
#.. .. .. .. ..@ serviceType       : Named chr "SOS"
#.. .. .. .. .. ..- attr(*, "names")= chr "ServiceType"
#.. .. .. .. ..@ serviceTypeVersion: Named chr "2.0.0"
#.. .. .. .. .. ..- attr(*, "names")= chr "ServiceTypeVersion"
#.. .. .. .. ..@ profile           :List of 2
#.. .. .. .. .. ..$ Profile: chr "http://www.opengis.net/spec/SOS/2.0/conf/gfoi"
#.. .. .. .. .. ..$ Profile: chr "http://www.opengis.net/spec/SOS/2.0/conf/soap"
#.. .. .. .. ..@ title             : Named chr "KISTERS KiWIS SOS2"
#.. .. .. .. .. ..- attr(*, "names")= chr "Title"
#.. .. .. .. ..@ abstract          : list()
#.. .. .. .. ..@ keywords          : list()
#.. .. .. .. ..@ fees              : chr(0) 
#.. .. .. .. ..@ accessConstraints : list()
#.. .. ..@ provider          :Formal class 'OwsServiceProvider' [package ".GlobalEnv"] with 3 slots
#.. .. .. .. ..@ providerName  : chr "NIWA"
#.. .. .. .. ..@ providerSite  : chr "http://www.niwa.co.nz"
#.. .. .. .. ..@ serviceContact:Classes 'XMLInternalElementNode', 'XMLInternalNode', 'XMLAbstractNode' <externalptr> 
#		.. .. ..@ operations        : NULL
#.. .. ..@ contents          : NULL
#.. .. ..@ version           : chr "2.0.0"
#.. .. ..@ updateSequence    : chr NA
#.. .. ..@ owsVersion        : chr "1.1.0"
#..@ parsers            :List of 29
#[...]
#..@ dataFieldConverters:
#[...]
#..@ timeFormat         : chr "%Y-%m-%dT%H:%M:%OS"
#..@ verboseOutput      : logi TRUE
#..@ switchCoordinates  : logi FALSE
#..@ useDCPs            : logi TRUE
#..@ dcpFilter          :List of 4
#.. ..$ GET : chr "*"
#.. ..$ POST: chr "*"
#.. ..$ KVP : chr "*"
#.. ..$ POX : chr "*"
#..@ additionalKVPs     :List of 1
#.. ..$ datasource: chr "0"

sosTitle(kiwiskvp)
sosServiceProvider(kiwiskvp)
