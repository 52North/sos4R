#
# Load dev version of sos4R ----
#
devtools::load_all()
webmockr::disable()
#
# VARIABLES----
#
.verbose <- TRUE
.saveOriginal <- FALSE
.version <- sos200_version
.binding <- "KVP"
.responseFormat <- "http://www.opengis.net/om/2.0"
#
# LOCAL
#
#.sos <- "http://localhost:8080/52n-sos-webapp/service"
.foi <- "vaisala-wxt520"
.procedure <- "wxt520"
# list(sosOfferings(sos)[[.procedure]]@observableProperty[[1]])
.observedProperty <- list("AirTemperature")
#
# Other
#
.sos <- "http://sensorweb.demo.52north.org/sensorwebtestbed/service"
.foi <- "Vaisala_WXT520"
.temporalFilter <- "2018-04-22T17:45:15+02:00/2018-05-24T17:45:15+02:00"
# .binding <- "POX"
#
# NIWA
#
# https://climate-sos.niwa.co.nz/?service=SOS&version=2.0.0&request=GetCapabilities
#.sos <- "https://climate-sos.niwa.co.nz/"
#.foi <-
#.temporalFilter <- "2018-12-27T00:00:00+00:00/2018-12-28T02:30:00+00:00"
#
# Set-Up SOS object----
#
sos <- SOS(url = .sos, version = .version, verboseOutput = .verbose, binding = .binding)
#
# GetCapabilities::Contents----
#
phenomena(sos, includeTemporalBBox = TRUE, includeSiteId = FALSE)

#
# GetDataAvailability----
#
myGDA <- getDataAvailability(sos = sos, verbose = verbose, saveOriginal = saveOriginal)
myGDA[1]

#
# Metadata exploration----
#
# cat("\nNames of offerings:\n")
# print(names(sosOfferings(sos)))
# print(sosProcedures(sos)[[.procedure]])


# Playground----
#
# sosTime <- sosCreateTime(sos = sos, time = "2018-12-28/2018-12-29")
# is.list(sosTime)
# length(sosTime)==1
# isS4(sosTime[[1]])
# print(sosTime)
# show(sosTime)
# toString(sosTime)
# cat(paste0(sosTime))
# .kvpEscapeSpecialCharacters(sosTime)

mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service", version = sos100_version, binding = "KVP")
S <- describeSensor(mySOS, unique(unlist(sosProcedures(mySOS)))[[1]])
