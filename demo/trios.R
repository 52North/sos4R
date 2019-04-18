#library(sos4R)
#sessionInfo()

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
