\name{SensorML-class}
\Rdversion{1.1}

\docType{class}

\alias{SensorML-class}
\alias{show,SensorML-method}
\alias{print,SensorML-method}
\alias{toString,SensorML-method}
\alias{SensorML}
\alias{sml}

\title{Classes of the Namespace sml}

\description{
Classes and construction functions for elements from the OGC specification \dQuote{OpenGIS(R) Sensor Model Language (SensorML) Implementation Specification}.

The only class at the moment is \code{"SensorML"} which only wraps an \code{"XMLInternalDocument"}.
}

\section{Objects from the Class}{
Objects can be created by calls to the construction method in the form \code{SensorML(...)}.
}

\section{Slots}{
  \describe{
    \item{\code{xml}:}{Object of class \code{"XMLInternalDocument"}, holds the XML representation of the sensor description.}
  }
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "SensorML")}: Prints a short statement to the command line.}
	 }
}

\references{Botts, M., Robin, A. (Eds.), OpenGIS® Sensor Model Language (SensorML) Implementation Specification, OGC® 07-000}

\author{Daniel Nüst \email{daniel.nuest@uni-muenster.de}}

%\note{  }
%\section{Warning }{}
%\seealso{}

\examples{
showClass("SensorML")

# create a XmlInternalDocument and a SensorML class with it
xml <- xmlParse("<SensorML />")
sml <- SensorML(xml)

# print it
print(sml)

# access the inner document
sml@xml

}

\keyword{classes}
