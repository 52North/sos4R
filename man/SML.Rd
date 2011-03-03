\name{SensorML-class}
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

\alias{as.SensorML.SpatialPointsDataFrame}
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
    \item{\code{coords}:}{Object of class \code{"data.frame"}, holds the position of the sensor.}
    \item{\code{xml}:}{Object of class \code{"XMLInternalDocument"}, holds the XML representation of the sensor description.}
  }
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "SensorML")}: Prints a short statement to the command line.}
	 }
}

\references{Botts, M., Robin, A. (Eds.), OpenGIS® Sensor Model Language (SensorML) Implementation Specification, OGC® 07-000

Houbie, F., Skivee F., Robin A., Jirka S., Broering, A., Nuest D. (2009): OGC® Catalogue Services Specification 2.0 - Extension Package for ebRIM Application Profile: SensorML. OGC Discussion Paper. Open Geospatial Consortium: 09-163. \url{http://portal.opengeospatial.org/files/?artifact_id=37944}.
}

\author{Daniel Nüst \email{daniel.nuest@uni-muenster.de}}

%\note{  }
%\section{Warning }{}
%\seealso{}

\examples{
showClass("SensorML")

\dontrun{
weathersos <- SOS("http://v-swe.uni-muenster.de:8080/WeatherSOS/sos")
proc1 <- sosProcedures(weathersos)[[1]][[1]]
proc1.descr <- describeSensor(weathersos, proc1, verbose = TRUE)
plot(proc1.descr)
class(proc1.descr)
}

}

\keyword{classes}
