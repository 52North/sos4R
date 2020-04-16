\name{xml_document-class}
\Rdversion{1.1}
\docType{class}

\alias{xml_document}
\alias{xml_document-class}

\alias{xml_node}
\alias{xml_node-class}

\title{Classes to model S3 classes of package \code{xml2}}

\description{
Currently the main classes of the XML handling package \code{xml2} are only S3 classes and thus cannot be used in slots for S4 classes. To remedy this, classes can be registered with \code{setOldClass(..)}. This is done within this package, but should really be part of \code{xml2}, see https://github.com/r-lib/xml2/issues/248
}

\author{
Daniel Nuest
}

\keyword{classes}
