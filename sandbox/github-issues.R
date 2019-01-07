# https://github.com/52North/sos4R/issues/33

mySOS <- SOS(url = "http://sos.irceline.be/sos")
procs <- unique(unlist(sosProcedures(mySOS)))
S = describeSensor(mySOS,procs[[1]])
S

# > FIXED in dev version

# https://github.com/52North/sos4R/issues/37

testsos <- SOS("http://sensorweb.demo.52north.org/52n-sos-webapp/sos/pox", dcpFilter = list("POX" = "/pox"))
testsosoffering <- sosOfferings(testsos)[[1]]
obs <- getObservation(sos = testsos, offering = testsosoffering, observedProperty = sosObservedProperties(testsosoffering)[4], verbose = TRUE)
sosResult(obs)


# https://github.com/52North/sos4R/issues/53

CEH <- SOS("http://192.171.139.63/observations/service",
           binding = "KVP",
           version = "2.0.0",
           #dataFieldConverters = myConverters
           useDCPs = FALSE
           )

myOff <- sosOfferings(CEH)[['/ECN/T04/RAIN/2/raw/']]
myTemporalFilter <- sosCreateEventTimeList(sosCreateTimePeriod(sos = CEH,
                                                               begin = as.POSIXct(Sys.time() - 3600 * 24), #* 180),
                                                               end = as.POSIXct(Sys.time())))
last24hObs <- getObservation(sos = CEH,
                             #responseFormat = "application/json",
                             offering = myOff,
                             verbose = TRUE,
                             observedProperty = list("http://vocabs.lter-europe.net/EnvThes/USLterCV_443"),
                             eventTime = myTemporalFilter
                             )