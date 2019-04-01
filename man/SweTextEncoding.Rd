\name{SweTextEncoding-class}

\Rdversion{1.1}

\docType{class}

\alias{SweTextEncoding-class}
\alias{SweTextEncoding}

\title{Class \code{"SweTextEncoding"}}
\description{
Representation of a text encoding defined in the OGC SWE Common specification. It defines the token, block, and decimal seperators for a text-encoded array of values.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SweTextEncoding", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{tokenSeparator}:}{Object of class \code{"character"} ~~ }
    \item{\code{blockSeparator}:}{Object of class \code{"character"} ~~ }
    \item{\code{decimalSeparator}:}{Object of class \code{"character"} ~~ }
    \item{\code{id}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Methods}{
No methods defined with class "SweTextEncoding" in the signature.
}

\examples{
showClass("SweTextEncoding")
}
\keyword{classes}
