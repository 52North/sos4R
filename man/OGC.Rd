\name{OGC}
\Rdversion{1.1}

\docType{class}

\alias{OGC}
\alias{ogc}

\alias{OgcBBOX-class}
\alias{show,OgcBBOX-method}
\alias{OgcBinarySpatialOp}
\alias{OgcBinarySpatialOp-class}
\alias{OgcBinaryTemporalOp}
\alias{OgcBinaryTemporalOp-class}
\alias{OgcBinaryTemporalOpOrNULL-class}
\alias{OgcComparisonOps}
\alias{OgcComparisonOps-class}
\alias{OgcComparisonOpsOrXMLOrNULL-class}
\alias{OgcContains-class}
\alias{show,OgcContains-method}
\alias{OgcIntersects-class}
\alias{show,OgcIntersects-method}
\alias{OgcOverlaps-class}
\alias{show,OgcOverlaps-method}
\alias{OgcSpatialOps}
\alias{OgcSpatialOps-class}
\alias{OgcSpatialOpsOrNULL-class}
\alias{OgcBBOX}
\alias{OgcContains}
\alias{OgcIntersects}
\alias{OgcOverlaps}
\alias{print,OgcBBOX-method}
\alias{print,OgcContains-method}
\alias{print,OgcIntersects-method}
\alias{print,OgcOverlaps-method}
\alias{toString,OgcBBOX-method}
\alias{toString,OgcContains-method}
\alias{toString,OgcIntersects-method}
\alias{toString,OgcOverlaps-method}

\title{Classes and Construction Functions for the OGC Namespace}

\description{
These classes represent elements from the OpenGIS(R) Filter Encoding Implementation Specification that are used in requests to Sensor Observation Services.
}

\section{Objects from the Class}{
Objects can be created by calls to the respective construction functions of the form \code{OgcBBOX( ...)}, \code{OgcContains(...)}, or \code{OgcIntersects}.

The following classes are virtual, no objects may be created from them: \code{OgcBinaryTemporalOp}, \code{OgcBinaryTemporalOpOrNULL}, \code{OgcComparisonOps}, code{OgcComparisonOpsOrNULL}, \code{OgcSpatialOps}, \code{OgcSpatialOpsOrNULL}.
}

\usage{
OgcBBOX(propertyName = sosDefaultSpatialOpPropertyName, envelope)
OgcContains(propertyName = sosDefaultSpatialOpPropertyName, geometry = NULL,
            envelope = NULL)
OgcIntersects(propertyName = sosDefaultSpatialOpPropertyName, geometry = NULL,
              envelope = NULL)
OgcOverlaps(propertyName = sosDefaultSpatialOpPropertyName, geometry = NULL,
            envelope = NULL)
}

\section{Slots}{
  \describe{
    \item{\code{propertyName}:}{Object of class \code{"character"}, the value of the propertyName attribute.}
    \item{\code{geometry}:}{Object of class \code{"GmlGeometry"}, a geometry contained in a spatial filter.}
    \item{\code{envelope}:}{Object of class \code{"GmlEnvelope"}, an envelope contained in a spatial filter.}
    \item{\code{time}:}{Object of class \code{"GmlTimeGeometricPrimitive"}, a time element contained in a temporal filter.}
  }
}

\section{Extends}{
\bold{OgcBBOX, OgcBinarySpatialOp:}
Class \code{"\linkS4class{OgcSpatialOps}"}, directly.
Class \code{"\linkS4class{OgcSpatialOpsOrNULL}"}, by class "OgcSpatialOps", distance 2.

\bold{OgcBinaryTemporalOp:}
Class \code{"\linkS4class{OgcBinaryTemporalOpOrNULL}"}, directly.

\bold{OgcContains, OgcIntersects, OgcOverlaps:}
Class \code{"\linkS4class{OgcBinarySpatialOp}"}, directly.
Class \code{"\linkS4class{OgcSpatialOps}"}, by class "OgcBinarySpatialOp", distance 2.
Class \code{"\linkS4class{OgcSpatialOpsOrNULL}"}, by class "OgcBinarySpatialOp", distance 3.

\bold{OgcSpatialOps:}
Class \code{"\linkS4class{OgcSpatialOpsOrNULL}"}, directly.

}

\section{Methods}{
  \describe{
    \item{encodeKVP}{\code{signature(obj = "OgcBinaryTemporalOp", sos = "SOS")}: Encode the given operation in key-value-pair style, see \code{\link{encodeKVP}}}
    \item{encodeXML}{\code{signature(obj = "OgcBBOX", sos = "SOS")}: Encode the given operation in XML, see \code{\link{encodeXML}}}
    \item{encodeXML}{\code{signature(obj = "OgcComparisonOps", sos = "SOS")}: Encode the given operation in XML, see \code{\link{encodeXML}}}
    \item{encodeXML}{\code{signature(obj = "OgcContains", sos = "SOS")}: Encode the given operation in XML, see \code{\link{encodeXML}}}
    \item{encodeXML}{\code{signature(obj = "OgcIntersects", sos = "SOS")}: Encode the given operation in XML, see \code{\link{encodeXML}}}
    \item{encodeXML}{\code{signature(obj = "OgcOverlaps", sos = "SOS")}: Encode the given operation in XML, see \code{\link{encodeXML}}}
%    \item{show}{\code{signature(object = "OgcBBOX")}: Show the given object in a human readable fashion.}
%    \item{show}{\code{signature(object = "OgcContains")}: Show the given object in a human readable fashion.}
%    \item{show}{\code{signature(object = "OgcOverlaps")}: Show the given object in a human readable fashion.}
	 }
}

\arguments{
Arguments for the construction functions are as follows.

  \item{propertyName}{The value for the propertyName attribute.}
  \item{geometry}{The geometry to be used in a spatial filter.}
  \item{envelope}{The geometry to be used in a spatial filter.}
}

\value{The value of the construction functions is an object of the respective class.}

\details{
These comprise spatial and temporal operations and operators which can be encoded in different ways.

The \verb{...OrNULL} classes are used to model optional slots.

}

\references{Vretanos, Panagiotis A. (Ed.), OpenGIS(R) Filter Encoding Implementation Specification, OGC 04-095, Version: 1.1.0

Schemas: \url{http://schemas.opengis.net/filter/1.1.0/}
}

\author{Daniel Nuest \email{daniel.nuest@uni-muenster.de}}

\note{This implementation of the Filter Encoding Specification is not complete.}

\section{Warning }{The encoding functions of these classes are not completely implemented yet.}

\examples{
showClass("OgcBBOX")
showClass("OgcBinarySpatialOp")
showClass("OgcBinaryTemporalOp")
showClass("OgcBinaryTemporalOpOrNULL")
showClass("OgcComparisonOps")
showClass("OgcContains")
showClass("OgcOverlaps")
showClass("OgcSpatialOps")
showClass("OgcSpatialOpsOrNULL")
}

\keyword{classes}
\keyword{utilities}
