\name{encodeXML-methods}

\docType{methods}

\alias{encodeXML}
\alias{encodeXML-methods}
\alias{encodeXML,GmlDirectPosition,SOS-method}
\alias{encodeXML,GmlEnvelope,SOS-method}
\alias{encodeXML,GmlLineString,SOS-method}
\alias{encodeXML,GmlPointProperty,SOS-method}
\alias{encodeXML,GmlPoint,SOS-method}
\alias{encodeXML,GmlPolygon,SOS-method}
\alias{encodeXML,GmlTimeInstantProperty,SOS-method}
\alias{encodeXML,GmlTimeInstant,SOS-method}
\alias{encodeXML,GmlTimePeriod,SOS-method}
\alias{encodeXML,GmlTimePosition,SOS-method}
\alias{encodeXML,OgcBBOX,SOS-method}
\alias{encodeXML,OgcComparisonOps,SOS-method}
\alias{encodeXML,OgcContains,SOS-method}
\alias{encodeXML,OgcIntersects,SOS-method}
\alias{encodeXML,OgcOverlaps,SOS-method}
\alias{encodeXML,SosEventTime,SOS-method}
\alias{encodeXML,SosFeatureOfInterest,SOS-method}
\alias{encodeXML,SosGetFeatureOfInterest_2.0.0,SOS-method}
\alias{encodeXML,TM_After,SOS-method}
\alias{encodeXML,TM_Before,SOS-method}
\alias{encodeXML,TM_During,SOS-method}
\alias{encodeXML,TM_Equals,SOS-method}
\alias{encodeXML,XMLNode,SOS-method}
\alias{encodeXML,character,SOS-method}
\alias{encodeXML,XMLInternalElementNode,SOS-method}
\alias{encodeXML,POSIXt,SOS-method}
\alias{encodeXML,xml_document,SOS-method}
\alias{encodeXML,xml_node,SOS-method}

\title{Encode Classes as XML}

\description{
These methods convert the given objects to XML representations for \code{HTTP POST} requests. The given instance of \code{SOS} is possibly used for encoding sub-elements or accessing metadata which is required for the encoding, like time stamp format.
}
\section{Methods}{
\describe{
	\item{\code{signature(obj = "GmlDirectPosition", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "GmlEnvelope", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "GmlLineString", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "GmlPointProperty", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "GmlPoint", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "GmlPolygon", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "GmlTimeInstantProperty", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "GmlTimeInstant", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "GmlTimePeriod", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "GmlTimePosition", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "POSIXt", sos = "SOS")}}{Converts the time object to a string that is suitable to be used as the value of XML time elements.}
	\item{\code{signature(obj = "OgcBBOX", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "OgcComparisonOps", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "OgcContains", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "OgcIntersects", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "OgcOverlaps", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "SosEventTime", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "SosFeatureOfInterest", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "SosGetFeatureOfInterest_2.0.0", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "TM_After", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "TM_Before", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "TM_During", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "TM_Equals", sos = "SOS")}}{Convert the given object to an XML representation.}
	\item{\code{signature(obj = "XMLNode", sos = "SOS")}}{Convert the given object to an XML representation.}
}
}

\references{XML specification: \url{http://www.w3.org/XML/}.}

\seealso{\code{\link{encodeRequestXML}}, \code{\link{encodeRequestSOAP}}}

\keyword{methods}
