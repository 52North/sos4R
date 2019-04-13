\name{SosContents-class}
\Rdversion{1.1}

\docType{class}

\alias{SosContents-class}
\alias{SosContentsOrNULL-class}
\alias{show,SosContents-method}
\alias{SosContents}
\alias{print,SosContents-method}
\alias{toString,SosContents-method}

\title{Class and Construction Function of "SosContents"}

\description{
\code{SosContents} models the sos:Contents section in a service metadata document.
}

\section{Objects from the Class}{
Objects can be created by calls to the construction funtion in the form \code{SosContents(...)}.

The virtual class \code{SosContentsOrNULL} is used to model optional slots in classes containing \code{SosContents}: No objects may be created from it.
}

\section{Slots}{
  \describe{
    \item{\code{observationOfferings}:}{Object of class \code{"list"} ~~ }
    \item{\code{xml}:}{Object of class \code{"XMLAbstractNode"} ~~ }
  }
}

\section{Extends}{
Class \code{"\linkS4class{OwsContents}"}, directly.
Class \code{"\linkS4class{SosContentsOrNULL}"}, directly.
Class \code{"\linkS4class{OwsContentsOrNULL}"}, by class "OwsContents", distance 2.
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "SosContents")}: ... }
	 }
}

\usage{
SosContents(observationOfferings)
}

\arguments{
  \item{observationOfferings}{A list of objects of class \code{SosObservationOffering}.}
}

\details{
The SosContents section extends the generic ows:Contents elements. It contains the \code{\linkS4class{SosObservationOffering}}s of a Sensor Observation Service.
}

\references{See section 8.2.3.2, \dQuote{Contents Section}, of the SOS specification.}


\author{Daniel Nuest \email{daniel.nuest@uni-muenster.de}}

\seealso{\code{\linkS4class{SosObservationOffering}}, \code{\linkS4class{OwsContents}}
}

\examples{
showClass("SosContents")
showClass("SosContentsOrNULL")
}
\keyword{classes}
