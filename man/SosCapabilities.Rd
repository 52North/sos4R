\name{SosCapabilities_1.0.0-class}
\Rdversion{1.1}

\docType{class}

\alias{SosCapabilities_1.0.0-class}
\alias{SosCapabilities}
\alias{SosCapabilities-class}

\title{Class and Construction Function for "SosCapabilities_1.0.0"}

\description{
The service metadata document of a Sensor Observation Service.
}

\section{Objects from the Class}{
Objects can be created by calls to the construction function of the form \code{SosCapabilities(...)} including the parameter \code{"owsVersion"} for the respective version of the service metadata document.
}

\section{Slots}{
  \describe{
    \item{\code{filterCapabilities}:}{Object of class \code{"SosFilter_CapabilitiesOrNULL"} ~~ }
    \item{\code{identification}:}{Object of class \code{"OwsServiceIdentificationOrNULL"} ~~ }
    \item{\code{provider}:}{Object of class \code{"OwsServiceProviderOrNULL"} ~~ }
    \item{\code{operations}:}{Object of class \code{"OwsOperationsMetadataOrNULL"} ~~ }
    \item{\code{contents}:}{Object of class \code{"OwsContentsOrNULL"} ~~ }
    \item{\code{version}:}{Object of class \code{"character"} ~~ }
    \item{\code{updateSequence}:}{Object of class \code{"character"} ~~ }
    \item{\code{owsVersion}:}{Object of class \code{"character"} ~~ }
  }
}

\section{Extends}{
Class \code{"\linkS4class{OwsCapabilities_1.1.0}"}, directly.
Class \code{"\linkS4class{OwsCapabilities}"}, by class "OwsCapabilities_1.1.0", distance 2.
}

\section{Methods}{
No methods defined with class "SosCapabilities_1.0.0" in the signature.
}

\usage{
SosCapabilities(version, updateSequence = NA, owsVersion = "1.1.0", identification = NULL, provider = NULL, operations = NULL, filterCapabilities = NULL, contents = NULL)
}

\arguments{
  \item{version}{The version of the service.}
  \item{updateSequence}{Service metadata document version, value is "increased" whenever any change is made in complete service metadata document. This can be used to request a certain version of a metadata document. Parameter is found in both request and reponse, but may not be supported by a service.}
  \item{owsVersion}{The used OWS specification version.}
  \item{identification}{The identification section of a capabilities document, object of class \code{OwsServiceIdentification}.}
  \item{provider}{The provider section of a capabilities document, object of class \code{OwsServiceProvider}.}
  \item{operations}{A list of objects of class \code{OwsOperation} in a \code{OperationsMetadata} object. The provider section of a capabilities document.}
  \item{filterCapabilities}{An object of class \code{SosFilter_Capabilities} contaning the filter capabilities of a service.}
  \item{contents}{The provider section of a capabilities document, object of class \code{SosContents}.}
}

\details{
This document provides clients with service metadata about a specific service instance, including metadata about the tightly-coupled data served.

The portions of the GetCapabilities response document that are defined in the OWS Common specification are not modified for SOS. The sections of the response that are specific for the SOS are the Filter_Capabilities and the Contents section.
}

\references{Section 8.2.3 of the SOS specification: Na, A., Priest, M. (Eds.), Sensor Observation Service, Open Geospatial Consortium Inc., OGC 06-009r6, Version: 1.0}

\author{Daniel Nuest \email{daniel.nuest@uni-muenster.de}}

%\note{  }
%\section{Warning }{}

\seealso{
\code{\linkS4class{SosFilter_Capabilities}}, \code{\linkS4class{SosContents}}, \code{\linkS4class{OwsCapabilities}}
}

\examples{
showClass("SosCapabilities_1.0.0")
}
\keyword{classes}

