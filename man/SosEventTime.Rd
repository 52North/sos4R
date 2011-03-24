\name{SosEventTime}
\Rdversion{1.1}

\docType{class}

\alias{SosEventTime-class}
\alias{SosEventTimeLatest-class}
\alias{show,SosEventTime-method}
\alias{print,SosEventTime-method}
\alias{toString,SosEventTime-method}
\alias{SosEventTime}
\alias{SosEventTimeLatest}

\title{Classes and Construction Functions for sos:eventTime elements.}

\description{Temporal query parameters for GetObservation requests.}

\usage{
SosEventTime(temporalOps)
SosEventTimeLatest()
}

\arguments{
  \item{temporalOps}{An object of class \link{OgcBinaryTemporalOp-class} to be wrapped by the sos:eventTime element.}
}

\section{Objects from the Classes}{
Objects can be created by calls to the construction functions of the form \code{SosEventTime(...)} or \code{SosEventTimeLatest(...)}.
}

\section{Slots}{
  \describe{
    \item{\code{temporalOps}:}{Object of class \code{"OgcBinaryTemporalOp"} for \code{SosEventTime}, the temporal operand to be inserted into the event time, or an object of class \code{"character"} for \code{SosEventTimeLatest}.}
  }
}

\section{Methods}{
  \describe{
    \item{encodeKVP}{\code{signature(obj = "SosEventTime", sos = "SOS")}: Encode the given object as a key-value pair.}
    \item{encodeXML}{\code{signature(obj = "SosEventTime", sos = "SOS")} or \code{signature(obj = "SosEventTimeLatest", sos = "SOS")}: Encode the given object as XML.}
	 }
}

\details{
Specifies the time period(s) for which observations are requested. This allows a client to request observations from a specific instant, multiple instances or periods of time in the past, present and future. The supported range is listed in the selected offering capabilities. The objects of these classes are used in the GetObservation (paramter in \link{GetObservation}).

The request for the latest observation is \bold{not} standard conform. The distinction between the classes happens eventually during encoding, where both classes have different encoding functions.

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

\author{Daniel Nüst \email{daniel.nuest@uni-muenster.de}}

%\note{  }
%\section{Warning }{}

\seealso{See also \code{\link{SosGetObservation-class}}, \code{\link{sosCreateEventTimeList-methods}}.}

\examples{
showClass("SosEventTime")
showClass("SosEventTimeLatest")

\dontrun{
# create SosEventTime for all times after the given time stamp
tOps <- TM_After(time = GmlTimeInstant(timePosition = GmlTimePosition(as.POSIXct("2010-01-01 12:00"))))
time1 <- SosEventTime(tOps)

# encode it as XML and KVP
encodeXML(time1)
encodeKVP(time1)

time2 <- SosEventTimeLatest()
encodeXML(time2)
}

}

\keyword{classes}
