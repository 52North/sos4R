\name{SosFilter_Capabilities-class}
\Rdversion{1.1}

\docType{class}

\alias{SosFilter_Capabilities-class}
\alias{show,SosFilter_Capabilities-method}
\alias{print,SosFilter_Capabilities-method}
\alias{toString,SosFilter_Capabilities-method}
\alias{SosFilter_Capabilities}
\alias{SosFilter_CapabilitiesOrNULL-class}

\title{Classes and Construction Functions for "SosFilter_Capabilities" Elements}

\description{
Additional section in the service metadata document of a Sensor Observation Service, which contains information about the supported filters.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SosFilter_Capabilities", ...)}.

\code{SosFilter_CapabilitiesOrNULL} is virtual class: No objects may be created from it.	 
}

\usage{
SosFilter_Capabilities(spatial = list(NA_character_), temporal = list(NA_character_), scalar = list(NA_character_), id = list(NA_character_))
}

\arguments{
  \item{spatial}{A character list of names of available spatial filters.}
  \item{temporal}{A character list of names of available temporal filters.}
  \item{scalar}{A character list of names of available scalar filters.}
  \item{id}{A character list of names of available filters on identifiers.}
}

\section{Slots}{
  \describe{
    \item{\code{spatial}:}{Object of class \code{"list"} with character strings for names of spatial filters.}
    \item{\code{temporal}:}{Object of class \code{"list"} with character strings for names of temporal filters.}
    \item{\code{scalar}:}{Object of class \code{"list"} with character strings for names of scalar filters.}
    \item{\code{id}:}{Object of class \code{"list"} with character strings for names of ID filters.}
  }
}

\section{Extends}{
Class \code{"\linkS4class{SosFilter_CapabilitiesOrNULL}"}, directly.
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "SosFilter_Capabilities")}: ... }
	 }
}

\details{
The FilterCapabilities section is used to indicate what types of query parameters are supported by the service. These capabilities refer to the parameters of the GetObservation operation which is the only operation that includes OGC filter-like expressions.
}

\references{
See section 8.2.3.1, \dQuote{FilterCapabilities Section}, the SOS specification: Na, A., Priest, M. (Eds.), Sensor Observation Service, Open Geospatial Consortium Inc., OGC 06-009r6, Version: 1.0
}

\author{Daniel Nuest \email{daniel.nuest@uni-muenster.de}}

%\note{  }
%\section{Warning }{}

\seealso{\code{\linkS4class{SosCapabilities}}}

\examples{
showClass("SosFilter_Capabilities")
showClass("SosFilter_CapabilitiesOrNULL")
}

\keyword{classes}
