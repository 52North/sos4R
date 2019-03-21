#
# Load latest version of sos4R----
#
devtools::load_all()
#
# VARIABLES----
#
.verbose <- TRUE
.saveOriginal <- FALSE
.version <- sos200_version
.binding <- "KVP"
.responseFormat <- "http://www.opengis.net/om/2.0"
.procedure <- "wxt520"
.observedProperty <- list("AirTemperature")
.sos <- "http://sensorweb.demo.52north.org/sensorwebtestbed/service"
.foi <- "Vaisala_WXT520"
.temporalFilter <- "2018-04-22T17:45:15+02:00/2018-05-24T17:45:15+02:00"
#
# Test
#
sos <- SOS(url = .sos, version = .version, verboseOutput = .verbose, binding = .binding)
myGetObservation <- getObservation(sos = sos,
                                   offering = sosOfferings(sos)[[.procedure]],
                                   observedProperty = .observedProperty,
                                   responseFormat = .responseFormat,
                                   eventTime = sosCreateTime(sos = sos, time = .temporalFilter),
                                   verbose = .verbose,
                                   saveOriginal = .saveOriginal)
# get class of result: should be OmMeasurement but it's OmOM_Observation
myClass <- class(myGetObservation[[1]])
toString(myClass)
# get result: should be of type GmlMeasure
myResultClass <- class(myGetObservation[[1]]@result)
toString(myResultClass)
