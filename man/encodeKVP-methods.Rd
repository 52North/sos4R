\name{encodeKVP-methods}

\docType{methods}


\alias{encodeKVP}
\alias{encodeKVP-methods}
\alias{encodeKVP,GmlTimeInstant,SOS-method}
\alias{encodeKVP,GmlTimePeriod,SOS-method}
\alias{encodeKVP,GmlTimePosition,SOS-method}
\alias{encodeKVP,OgcBinaryTemporalOp,SOS-method}
\alias{encodeKVP,SosEventTime,SOS-method}
\alias{encodeKVP,SosEventTime,ANY-method}
\alias{encodeKVP,POSIXt,SOS-method}

\title{Encode Classes as KVP}

\description{
These methods convert a given object to a key-value-pair representation to be used in GET requests.
The given instance of \code{SOS} is possibly used for encoding sub-elements or accessing metadata which is required for the encoding, like time stamp format.
}

\section{Methods}{
\describe{
	\item{\code{signature(obj = "GmlTimeInstant", sos = "SOS")}}{Convert the given object to a KVP representation.}
	\item{\code{signature(obj = "GmlTimePeriod", sos = "SOS")}}{Convert the given object to a KVP representation.}
	\item{\code{signature(obj = "GmlTimePosition", sos = "SOS")}}{Convert the given object to a KVP representation.}
	\item{\code{signature(obj = "OgcBinaryTemporalOp", sos = "SOS")}}{Convert the given object to a KVP representation.}
	\item{\code{signature(obj = "SosEventTime", sos = "ANY")}}{Convert the given object to a KVP representation.}
	\item{\code{signature(obj = "POSIXt", sos = "ANY")}}{Convert the given object to a string format suitable for KVP representation.}
}

}

\seealso{\code{\link{SosBindings}}, \url{https://en.wikipedia.org/wiki/Key_Value_Pair}}

\keyword{methods}
