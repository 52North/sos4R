\name{TM_Operators}

\docType{class}

\alias{print,TM_After-method}
\alias{print,TM_Before-method}
\alias{print,TM_During-method}
\alias{print,TM_Equals-method}
\alias{show,TM_After-method}
\alias{show,TM_Before-method}
\alias{show,TM_During-method}
\alias{show,TM_Equals-method}
\alias{toString,TM_After-method}
\alias{toString,TM_Before-method}
\alias{toString,TM_During-method}
\alias{toString,TM_Equals-method}
\alias{TM_After-class}
\alias{TM_Before-class}
\alias{TM_During-class}
\alias{TM_Equals-class}
\alias{TM_Operators-class}
\alias{TM_Operators}
\alias{TM_After}
\alias{TM_Before}
\alias{TM_During}
\alias{TM_Equals}

\title{Classes and Construction Methods for Temporal Operator Classes}
\description{
Classes for temporal operators from OpenGIS(R) Filter Encoding used in filters in GetObservation requests.
}

\usage{
TM_After(propertyName = sosDefaultTempOpPropertyName, time)
TM_Before(propertyName = sosDefaultTempOpPropertyName, time)
TM_During(propertyName = sosDefaultTempOpPropertyName, time)
TM_Equals(propertyName = sosDefaultTempOpPropertyName, time)
}

\arguments{
  \item{propertyName}{The name of the property that is used to wrap the time.}
  \item{time}{A time instant or period to be used as the temporal operand.}
}

\value{An object of the respective class, so \code{TM_After}, \code{TM_Before}, \code{TM_During} or \code{TM_Equals}}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("TM_After", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{time}:}{Object of class \code{"GmlTimeInstant"} ~~ }
    \item{\code{propertyName}:}{Object of class \code{"character"} ~~ }
  }
}

\section{Extends}{
Class \code{"\linkS4class{OgcBinaryTemporalOp}"}, directly.
Class \code{"\linkS4class{OgcBinaryTemporalOpOrNULL}"}, by class "OgcBinaryTemporalOp", distance 2.
}

\section{Methods}{
  \describe{
    \item{encodeXML}{\code{signature(obj = "TM_After", sos = "SOS")}: ... }
    \item{show}{\code{signature(object = "TM_After")}: ... }
    \item{encodeXML}{\code{signature(obj = "TM_Before", sos = "SOS")}: ... }
    \item{show}{\code{signature(object = "TM_Before")}: ... }
    \item{encodeXML}{\code{signature(obj = "TM_During", sos = "SOS")}: ... }
    \item{show}{\code{signature(object = "TM_During")}: ... }
    \item{encodeXML}{\code{signature(obj = "TM_Equals", sos = "SOS")}: ... }
    \item{show}{\code{signature(object = "TM_Equals")}: ... }
	 }
}

\references{
Vretanos, Panagiotis A. (Ed.), OpenGIS® Filter Encoding Implementation Specification, OGC 04-095, Version: 1.1.0

See the schema file: \url{http://schemas.opengis.net/sos/1.0.0/ogc4sos.xsd}.
}

\author{Daniel Nüst \email{daniel.nuest@uni-muenster.de}}

\seealso{\code{\link{SosGetObservation}}}

\examples{
showClass("TM_After")
showClass("TM_Before")
showClass("TM_During")
showClass("TM_Equals")

\dontrun{
# create times to use for operators
t1 <- sosCreateTimeInstant(sos = weathersos, time = Sys.time())
p1 <- sosCreateTimePeriod(sos = weathersos, begin = as.POSIXct("2010-03-01 12:15"), end = as.POSIXct("2010-03-02 12:15"))

# create temporal operator
afterNow <- TM_After(time = t1)
print(afterNow)
encodeXML(t1, sos)

during <- TM_During(time = p1)
print(during)
}

}

\keyword{classes}
\keyword{utilities}
