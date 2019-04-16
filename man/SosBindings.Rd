\name{SosBindings}

\alias{bindings}
\alias{SosBinding}
\alias{SosBindings}
\alias{GET}
\alias{POST}
\alias{KVP}
\alias{POX}
\alias{SOAP}
\alias{HTTP}

\title{Bindings and Connecition Methods of OGC Sensor Observation Service}
\description{
The SOS comes with three possible methods of transfering data, HTTP GET, HTTP POST and SOAP.}

\details{
The \strong{POST} binding is described in the official SOS specification and should be the default method.

The \strong{GET} binding is described by OOTethys in a Best Practice document: \url{https://web.archive.org/web/20120616065001/http://www.oostethys.org/best-practices/best-practices-get}. It contains some special encoding for bounding boxes, as the only spatial filter, and time periods, as the only temporal filter.

The \strong{SOAP} binding is not official with regards to the spec, and also not implemented yet.

The connection method can be changed on creation of a \code{SOS} object.
}

\references{Wikipedia page for HTTP request methods: \url{https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol#Request_methods}.}

\seealso{\code{\link{SosSupportedBindings}}}

\examples{

# HTTP connection methods supported by this sos4R implementation
supported <- SosSupportedBindings()
supported

\dontrun{
sos <- SOS("http://sosurl.org/", binding = "KVP")
}

}

\keyword{constants}
\keyword{XML}
