\name{sos4R-package}

\docType{package}

\alias{sos4R-package}
\alias{sos4R}

\alias{sosChanges}
\alias{sosCheatSheet}

\title{A client for the OGC Sensor Observation Service}

\description{
\pkg{sos4R} is a client for Sensor Observation Services (SOS). It allows users to retrieve metadata from SOS web service instances as specified by the Open Geospatial Consortium (OGC) and subsequently to interactively create requests for observation data based on the available sensors, phenomena, observations, offerings etc.
}

\details{
	\tabular{ll}{
	Package: \tab sos4R\cr
	Type: \tab Package\cr
	Version: \tab 0.2-3\cr
	Date: \tab 2011-05-02\cr
	License: \tab GPL-2\cr
	LazyLoad: \tab yes\cr
	Depends: \tab XML, RCurl\cr
	}
}

\author{
Daniel Nuest <daniel.nuest@uni-muenster.de>
}

\references{
Na, A., Priest, M. (Eds.), 2007. Sensor Observation Service. OpenGIS Implementation Standard, Version 1.0, OGC 06-009r6
}

\seealso{See also the package Vignette, \code{vignette(sos4R)}.}

\note{
The development of this software was gratefully supported by the 52North Student Innovation Prize for Geoinformatics 2010.

To stay updated on all matters around \pkg{sos4R} go to the development blog at \url{http://www.nordholmen.net/sos4r/}.

If you want to ask questions about using the software, please go to the 52North forum for the geostatistics community (\url{http://geostatistics.forum.52north.org/}), or consider subscribing to the mailing list (\url{http://list.52north.org/mailman/listinfo}).

The most extensive documenation is contained in the \strong{package vignette}.
}

\examples{

\dontrun{

# Take a SOS from the example list
sos.url = SosExampleServices()[[1]]

# Open the connection
sos = SOS(url = SOS)

# List offerings, procedures and observedProperties
names(sosOfferings(sos))
sosProcedures(sos)
sosObservedProperties(sos)

# Create time period (last 30 days)
tPeriod <- sosCreateEventTimeList(
	time = sosCreateTimePeriod(
		sos = pegelsos,
		begin = Sys.time() - (3600 * 24 * 30),
		end = Sys.time()))

# Request data for all observed properties and procedures of a certain offering
observation <- getObservation(sos = sos,
		observedProperty = sosObservedProperties(sos),
		offering = sosOfferings(sos)[[2]],
		procedure = sosProcedures(sos),
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

# Use custom converting function and connection method. This mechanism works the same for encoders and decoders.
myConverters <- SosDataFieldConvertingFunctions(
	"myNumericUnit" = sosConvertDouble)
mySos <- SOS(sos.url, method = "GET", dataFieldConverters = myConverters)
sosDataFieldConverters(mySos)

# print the changes document
sosChanges()

# get the cheat sheet
sosCheatSheet()

}

}

\keyword{package}
\keyword{connection}
\keyword{ts}
\keyword{spatial}
\keyword{database}
