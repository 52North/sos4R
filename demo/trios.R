############################################################################## #
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
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

library("sos4R")

myConverters <- SosDataFieldConvertingFunctions(
  "http://www.nexosproject.eu/dictionary/definitions.html#Turb_NTU" = sosConvertDouble
)

triosSOS <- SOS(url = "http://hspeed.trios.de:8888/52n-sos-webapp/service/kvp",
                inspect = T, version = "1.0.0",
                #verboseOutput = T,
                useDCPs = FALSE,
                binding = "KVP", dcpFilter = list("/KVP"),
                dataFieldConverters = myConverters)

trios.off <- sosOfferings(triosSOS)
names(trios.off)

SN.off <- sosOfferings(triosSOS)[[1]]
trios_time <- sosCreateTimePeriod(
  sos = triosSOS,
  begin = as.POSIXct("2017-6-13 00:00"),
  end = as.POSIXct("2017-06-20 05:00")
)

obs_SN <- getObservation(
  sos = triosSOS,
  procedure = sosProcedures(triosSOS)[[1]],
  offering = SN.off,
  eventTime = sosCreateEventTimeList(trios_time),
  inspect = TRUE
)

for (i in c(1:length(obs_SN))) {
  print(summary(obs_SN[[i]]))
}

data_1 <- sosResult(obs_SN[[1]])

plot(main = paste0(sosProcedures(obs_SN[[1]]), "\nmeasuring\n", sosObservedProperties(obs_SN[[1]])),
     x = data_1$phenomenonTime,
     y = data_1$`http://www.nexosproject.eu/dictionary/definitions.html#XX1_590.460`,
     pch = 20)
