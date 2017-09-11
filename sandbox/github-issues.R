# https://github.com/52North/sos4R/issues/33

mySOS <- SOS(url = "http://sos.irceline.be/sos")
procs <- unique(unlist(sosProcedures(mySOS)))
S = describeSensor(mySOS,procs[[1]])
S

# FIXED in dev version

# https://github.com/52North/sos4R/issues/37

testsos <- SOS("http://sensorweb.demo.52north.org/52n-sos-webapp/sos/pox", dcpFilter = list("POX" = "/pox"))
testsosoffering <- sosOfferings(testsos)[[1]]
obs <- getObservation(sos = testsos, offering = testsosoffering, observedProperty = sosObservedProperties(testsosoffering)[4], verbose = TRUE)
sosResult(obs)
