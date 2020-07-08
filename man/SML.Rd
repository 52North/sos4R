\name{SML}
\Rdversion{1.1}

\docType{class}

\alias{SensorML-class}
\alias{show,SensorML-method}
\alias{print,SensorML-method}
\alias{toString,SensorML-method}
\alias{SensorML}
\alias{sml}

\alias{sosId,SensorML-method}
\alias{sosName,SensorML-method}
\alias{sosAbstract,SensorML-method}
\alias{sosCoordinates,SensorML-method}

\alias{sosBoundedBy,SensorML-method}
\alias{sosGetCRS,SensorML-method}
\alias{sosTime,SensorML-method}

\alias{plot.SensorML}

\alias{plot,SensorML,missing-method}

\title{Classes of the Namespace sml}

\description{
Classes, construction functions, and accessor functions for elements from the OGC specification \dQuote{OpenGIS(R) Sensor Model Language (SensorML) Implementation Specification}.

The only class at the moment is \code{"SensorML"} which wraps an \code{"XMLInternalDocument"} and some additional information. This strongly depends on the SensorML Profile for Discovery to find the respective paramters.
}

\section{Objects from the Class}{
Objects can be created by calls to the construction method in the form \code{SensorML(...)}.
}

\section{Slots}{
  \describe{
    \item{\code{boundedBy}:}{Object of class \code{"matrix"}, the bounding box of the sensor.}
    \item{\code{coords}:}{Object of class \code{"data.frame"}, holds the position of the sensor.}
    \item{\code{description}:}{Object of class \code{"character"}, a normal text description of the sensor.}
    \item{\code{id}:}{Object of class \code{"character"}, the main identifier of the sensor.}
    \item{\code{name}:}{Object of class \code{"character"}, a naming identifier of the sensor.}
    \item{\code{validTime}:}{Object of class \code{"GmlTimeObjectOrNULL"}, the time period or instance for which the description is valid (only SOS 2.0.0).}
    \item{\code{xml}:}{Object of class \code{"XMLInternalDocument"}, holds the XML representation of the sensor description.}
  }
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "SensorML")}: Prints a short statement to the command line.}
	\item{plot}{\code{signature(object = "SensorML")}: Plots the sensor using coercion to an object of class \code{"Spatial"}.}
	}
}

\references{Botts, M., Robin, A. (Eds.), OpenGIS(R) Sensor Model Language (SensorML) Implementation Specification, Open Geospatial Consortium: 07-000.

Houbie, F., Skivee F., Robin A., Jirka S., Broering, A., Nuest D. (2009): OGC(R) Catalogue Services Specification 2.0 - Extension Package for ebRIM Application Profile: SensorML. OGC Discussion Paper. Open Geospatial Consortium: 09-163. \url{http://portal.opengeospatial.org/files/?artifact_id=37944}.
}

\author{Daniel Nuest \email{daniel.nuest@uni-muenster.de}}

\seealso{\code{\link{DescribeSensor}}}

\examples{
showClass("SensorML")

\dontrun{
mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/kvp",
             binding = "KVP")
mySensor <- describeSensor(sos = mySOS,
		procedure = sosProcedures(mySOS)[[1]],
		outputFormat = 'text/xml; subtype="sensorML/1.0.1"', # space is needed!
		)
class(mySensor)
print(mySensor)

sosId(mySensor)
sosName(mySensor)
sosBoundedBy(mySensor)
sosCoordinates(mySensor)
sosGetCRS(mySensor)

# valid time from SOS 2.0.0
mySOS2 <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/kvp",
             binding = "KVP",
             version = "2.0.0")
mySensor2 <- describeSensor(sos = mySOS2,
		procedure = sosProcedures(mySOS2)[[1]],
		outputFormat = 'text/xml; subtype="sensorML/1.0.1"')

sosTime(mySensor2)

plot(mySensor)
}

}

\keyword{classes}
