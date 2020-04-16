\name{SosEventTime}
\Rdversion{1.1}

\docType{class}

\alias{SosEventTime-class}
\alias{show,SosEventTime-method}
\alias{print,SosEventTime-method}
\alias{toString,SosEventTime-method}
\alias{SosEventTime}

\title{Classes and Construction Functions for sos:eventTime elements.}

\description{Temporal query parameters for GetObservation requests.}

\usage{
SosEventTime(temporalOps)
}

\arguments{
  \item{temporalOps}{An object of class \link{OgcBinaryTemporalOp-class} to be wrapped by the sos:eventTime element.}
}

\section{Objects from the Classes}{
Objects can be created by calls to the construction functions of the form \code{SosEventTime(...)}.
}

\section{Slots}{
  \describe{
    \item{\code{temporalOps}:}{Object of class \code{"OgcBinaryTemporalOp"} for \code{SosEventTime}, the temporal operand to be inserted into the event time, or an object of class \code{"character"} for \code{SosEventTimeLatest}.}
  }
}

\section{Methods}{
  \describe{
    \item{encodeKVP}{\code{signature(obj = "SosEventTime", sos = "SOS")}: Encode the given object as a key-value pair.}
    \item{encodeXML}{\code{signature(obj = "SosEventTime", sos = "SOS")}: Encode the given object as XML.}
	 }
}

\details{
Specifies the time period(s) for which observations are requested. This allows a client to request observations from a specific instant, multiple instances or periods of time in the past, present and future. The supported range is listed in the selected offering capabilities. The objects of these classes are used in the GetObservation (paramter in \link{GetObservation}).

A typical example in a POST request:
\verb{
<eventTime>
	<ogc:TM_During>
		<ogc:PropertyName>om:samplingTime</ogc:PropertyName>
		<gml:TimePeriod>
		<gml:beginPosition>2006-11-05T17:18:58.000-06:00</gml:beginPosition>
		<gml:endPosition>2006-11-05T21:18:59.000-06:00</gml:endPosition>
		</gml:TimePeriod>
	</ogc:TM_During>
</eventTime>
}

In GET binding (\code{\link{SosBindings}}) the eventTime is simply omitted for getting the latest observation.

It is recommended to use the creation functions as shown in the examples.
}

\references{See SOS specification, Table 4: \dQuote{Parameters of GetObservation Request}.}

\author{Daniel Nuest \email{daniel.nuest@uni-muenster.de}}

\seealso{See also \code{\link{SosGetObservation-class}}, \code{\link{sosCreateEventTimeList-methods}}.}

\examples{
showClass("SosEventTime")

# create SosEventTime for all times after the given time stamp
timePos <- GmlTimePosition(as.POSIXct("2010-01-01 12:00"))
tOps <- TM_After(time = GmlTimeInstant(timePosition = timePos))
time1 <- SosEventTime(tOps)

# encode it as XML
encodeXML(time1, sos = SOS_Test())

# encode it as KVP
encodeKVP(time1, sos = SOS_Test())
}

\keyword{classes}
