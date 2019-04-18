\name{SosObservationOffering-class}
\Rdversion{1.1}

\docType{class}

\alias{SosObservationOffering}
\alias{SosObservationOffering-class}
\alias{show,SosObservationOffering-method}
\alias{sosTime,SosObservationOffering-method}
\alias{sosBoundedBy,SosObservationOffering-method}
\alias{print,SosObservationOffering-method}
\alias{toString,SosObservationOffering-method}
\alias{sosName,SosObservationOffering-method}
\alias{sosResultModels,SosObservationOffering-method}
\alias{sosId,SosObservationOffering-method}

% Version 2.0.0
\alias{SosObservationOffering_2.0.0}
\alias{SosObservationOffering_2.0.0-class}
\alias{show,SosObservationOffering_2.0.0-method}
\alias{summary.SosObservationOffering_2.0.0}
\alias{print.summary.SosObservationOffering_2.0.0}
\alias{print,SosObservationOffering_2.0.0-method}
\alias{toString,SosObservationOffering_2.0.0-method}
\alias{sosName,SosObservationOffering_2.0.0-method}
\alias{sosResultModels,SosObservationOffering_2.0.0-method}
\alias{sosId,SosObservationOffering_2.0.0-method}

\title{Classes and Related Functions for "SosObservationOffering"}

\description{\code{SosObservationOffering}s collect all metadata about a specific offerign in a Sensor Observation Service.}

\section{Objects from the Class}{
Objects can be created by calls to the construction functions of the form \code{SosObservationOffering(...)}.
}

\usage{
SosObservationOffering(id, name = as.character(NA), time, procedure, observedProperty,
                       featureOfInterest, responseFormat,
                       intendedApplication = as.character(NA),
                       resultModel = as.character(NA),
                       responseMode = as.character(NA), boundedBy = list())
}

\section{Slots}{
  \describe{
    \item{\code{boundedBy}:}{Object of class \code{"list"}: A bounding box that contains all features in this offering.}
    \item{\code{featureOfInterest}:}{Object of class \code{"list"}: Features or feature collections that represent the identifiable object(s) on which the sensor systems are making observations.}
    \item{\code{id}:}{Object of class \code{"character"}: Identifier of an offering.}
    \item{\code{intendedApplication}:}{Object of class \code{"list"}: The intended category of use for this offering such as homeland security or natural resource planning}
    \item{\code{name}:}{Object of class \code{"character"}: The name of an offering.}
    \item{\code{observedProperty}:}{Object of class \code{"list"}: The observable/phenomenon that can be requested in this offering.}
    \item{\code{procedure}:}{Object of class \code{"list"}: A reference to one or more procedures, including sensor systems, instruments, simulators, etc, that supply observations in this offering. The DescribeSensor operation can be called to provide a detailed description of each system.}
    \item{\code{responseFormat}:}{Object of class \code{"list"}: MIME type of the data that will be returned as the result of a GetObservation request. This is usually text/xml; subtype="om/0.0.0".}
    \item{\code{responseMode}:}{Object of class \code{"list"}: Indicates what modes of response are supported for this offering. The value of resultTemplate is used to retrieve an observation template that will later be used in calls to GetResult. The other options allow results to appear inline in a resultTag (inline), external to the observation element (out-of-band) or as a MIME attachment (attached).}
    \item{\code{resultModel}:}{Object of class \code{"list"}: Indicates the namespace-qualified name of the result element that will be included in the document returned from a call to GetObservation for this offering, e.g. \samp{"om:Observation"} or \samp{"om:Measurement"}.}
    \item{\code{time}:}{Object of class \code{"GmlTimeGeometricPrimitive"}: Time period for which observations can be obtained. This supports the advertisement of historical as well as real-time observations.}
  }
}

\section{Methods}{
  \describe{
    \item{sosTime}{\code{signature(obj = "SosObservationOffering")}: Accessor function for the time slot, or to be more precise: the time period for which this offering provides data.}
	 }
}

\arguments{
  \item{boundedBy}{See the corresponding slot description.}
  \item{featureOfInterest}{See the corresponding slot description.}
  \item{id}{See the corresponding slot description.}
  \item{intendedApplication}{See the corresponding slot description.}
  \item{name}{See the corresponding slot description.}
  \item{observedProperty}{See the corresponding slot description.}
  \item{procedure}{See the corresponding slot description.}
  \item{responseFormat}{See the corresponding slot description.}
  \item{responseMode}{See the corresponding slot description.}
  \item{resultModel}{See the corresponding slot description.}
  \item{time}{See the corresponding slot description.}
}

\details{
ObservationOffering provides a mechanism for factoring groups of related observations within a single service instance. A functionally equivalent outcome could be obtained by factoring between different service instances.
}

\value{The construction functions return an object of the respective class, e.g. \code{SosObservationOffering}.}

\references{
See section 6.3, \dQuote{Observation Offerings}, of the SOS specification: Na, A., Priest, M. (Eds.), Sensor Observation Service, Open Geospatial Consortium Inc., OGC 06-009r6, Version: 1.0
}

\author{Daniel Nuest \email{daniel.nuest@uni-muenster.de}}

\seealso{\code{\linkS4class{SosContents}}, \code{\linkS4class{SosCapabilities}}}

\examples{
showClass("SosObservationOffering")
showClass("SosObservationOffering_2.0.0")

# explore offerings of an SOS
mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/kvp",
             binding = "KVP")
offering1 <- sosOfferings(mySOS)[[1]]

sosId(offering1)
sosName(offering1)
sosTime(offering1)
sosBoundedBy(offering1)
}

\keyword{classes}
