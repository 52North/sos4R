\name{SamsShape-class}

\Rdversion{1.1}

\docType{class}

\alias{SamsShape-class}
\alias{SamsShape}
\alias{print,SamsShape-method}
\alias{show,SamsShape-method}
\alias{toString,SamsShape-method}

\title{Class \code{"SamsShape"}}
\description{
Representation of the geometry of a spatial sampling feature, that can be used as a feature of interest. Currently, only points are supported.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SamsShape", ...)} or \code{SamsShape}.}

\section{Slots}{
  \describe{
    \item{\code{point}:}{Object of class \code{"GmlPoint"} ~~ }
    \item{\code{id}:}{Object of class \code{"character"} ~~ }
    \item{\code{srsName}:}{Object of class \code{"character"} ~~ }
    \item{\code{srsDimension}:}{Object of class \code{"integer"} ~~ }
    \item{\code{axisLabels}:}{Object of class \code{"character"} ~~ }
    \item{\code{uomLabels}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{GmlGeometry}"}, directly.
}
\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "SamsShape")}: ... }
    \item{show}{\code{signature(object = "SamsShape")}: ... }
    \item{toString}{\code{signature(x = "SamsShape")}: ... }
	 }
}

\examples{
showClass("SamsShape")
}
\keyword{classes}
