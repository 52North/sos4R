sos = SOS("http://sos.irceline.be/sos", 
					curlHandle = getCurlHandle(http.version=HTTP_VERSION_1_0))

sosCheatSheet()
?sosCreateTime

################################################################################
# test graph
go.offering = sosOfferings(sos)[["90500 - PN1"]] # UFP
go.observedProperty = sosObservedProperties(sos)["90500 - PN1"] # UFP
go.eventTime3 = sosCreateEventTimeList(sosCreateTimePeriod(sos = sos,
							begin = as.POSIXct("2014-01-01 18:00"),
							end = as.POSIXct("2014-01-10 18:00")))

obs3 <- getObservation(sos = sos, observedProperty = go.observedProperty,
											 #procedure = list(sosProcedures(sos)["99993"]),
											 verbose = TRUE,
											 inspect = TRUE,
											 eventTime = go.eventTime3,
											 offering = go.offering)

sosResult(obs3)

summary(obs3@result) # finally!
plot(x = obs3@result[["Time"]], y = obs3@result[["particle numbers"]],
		 type = "l", main = "UFP Antwerp", xlab = "Time", ylab = "number of particles < 1um")




################################################################################
# test manual
names(sosOfferings(sos))
sosProcedures(sos)
sosObservedProperties(sos)
# Create time period (last 30 days)
tPeriod <- sosCreateEventTimeList(
	time = sosCreateTimePeriod(
		sos = sos,
		begin = Sys.time() - (3600 * 24 * 30),
		end = Sys.time()))

# Request data for all observed properties and procedures of a certain offering
observation <- getObservation(sos = sos,
															observedProperty = sosObservedProperties(sos),
															offering = sosOfferings(sos)[[2]],
															verbose = TRUE,
															#procedure = sosProcedures(sos)["99993"],
															eventTime = tPeriod)
# Inspect result
sosResult(observation)
str(sosResult(observation))
# Inspect attributes of the data fields
if(is.list(sosResult(observation))) {
	attributes(sosResult(observation)[,1])
}
else {
	attributes(sosResult(pegelObs)[,1])
}

# Use custom converting function and connection method. This mechanism works
# the same for encoders and decoders.
myConverters <- SosDataFieldConvertingFunctions(
	"myNumericUnit" = sosConvertDouble)
mySos <- SOS(sos.url, method = "GET", dataFieldConverters = myConverters)
sosDataFieldConverters(mySos)


################################################################################
# test 1