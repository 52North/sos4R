\name{SWE}
\Rdversion{1.1}

\docType{class}

\alias{SweCompositePhenomenon-class}
\alias{show,SweCompositePhenomenon-method}
\alias{SwePhenomenon-class}
\alias{show,SwePhenomenon-method}
\alias{SwePhenomenonOrNULL-class}
\alias{SwePhenomenonProperty-class}
\alias{show,SwePhenomenonProperty-method}
\alias{SwePhenomenonPropertyOrNULL-class}
\alias{SweTextBlock-class}
\alias{show,SweTextBlock-method}

\alias{print,SweCompositePhenomenon-method}
\alias{print,SwePhenomenon-method}
\alias{print,SwePhenomenonProperty-method}
\alias{print,SweTextBlock-method}

\alias{toString,SweCompositePhenomenon-method}
\alias{toString,SwePhenomenon-method}
\alias{toString,SwePhenomenonProperty-method}
\alias{toString,SweTextBlock-method}

\alias{SweCompositePhenomenon}
\alias{SwePhenomenon}
\alias{SwePhenomenonProperty}
\alias{SweTextBlock}
\alias{sosObservedProperties,SweCompositePhenomenon-method}
\alias{sosObservedProperties,SwePhenomenonProperty-method}

\title{Classes and Construction Functions for the SWE Namespace}

\description{
These classes represent elements from the OpenGIS(R) Sensor Model Language (SensorML) Implementation Specification that are used to model observation data in responses from a Sensor Observation Service.
}

\section{Objects from the Class}{
Objects can be created by calls to the constrction functions of the form \code{SweCompositePhenomenon( ...)}, \code{SwePhenomenonProperty( ...)} and so forth.

The following classes are virtual, no objects may be created from them: \code{SwePhenomenonOrNULL}, \code{SwePhenomenonPropertyOrNULL}.
}

\usage{
SweCompositePhenomenon(id, name, description = as.character(NA), dimension,
                       components, base = NULL)
SwePhenomenon(id, name, description = as.character(NA))
SwePhenomenonProperty(href = as.character(NA), phenomenon = NULL)
SweTextBlock(tokenSeparator, blockSeparator, decimalSeparator, id = as.character(NA))
}

\section{Slots}{
  \describe{
    \item{\code{dimension}:}{Object of class \code{"integer"}, the value of the dimensions attribute of a composite phenomenon.}
    \item{\code{components}:}{Object of class \code{"list"}, the components of a composite phenomenon.}
    \item{\code{base}:}{Object of class \code{"SwePhenomenonPropertyOrNULL"}, the base of a composite phenomenon, may be \code{NULL}.}
    \item{\code{id}:}{Object of class \code{"character"}, the value of the id attribute of a (composite) phenomenon.}
    \item{\code{name}:}{Object of class \code{"character"}, the value of the name element of a (composite) phenomenon.}
    \item{\code{description}:}{Object of class \code{"character"}, the value of the description elements of a phenomenon.}
    \item{\code{href}:}{Object of class \code{"character"}, the value of the href attribute of a phenomenon property which references a phenomenon.}
    \item{\code{phenomenon}:}{Object of class \code{"SwePhenomenonOrNULL"}, the actual (inline) phenomenon of a phenomenon property.}
    \item{\code{tokenSeparator}:}{Object of class \code{"character"}, the symbol to be used as the token seperator in a \code{SweTextBlock}, e.g. in the case of \code{","} this would result in \verb{attribute1,attribute2}.}
    \item{\code{blockSeparator}:}{Object of class \code{"character"}, the symbol to be used as the block seperator in a \code{SweTextBlock}, e.g. in the case of \code{";"} this would result in \verb{attribute1_a,attribute2_a;attribute1_b,attribute2_b}.}
    \item{\code{decimalSeparator}:}{Object of class \code{"character"}, the symbol to be used as the decimal seperator in a \code{SweTextBlock}, e.g. in the case of \code{"."} this would result in \verb{attribute1,42.0,attribute3,23.0}.}
  }
}

\section{Extends}{
\bold{SweCompositePhenomenon:}
Class \code{"\linkS4class{SwePhenomenon}"}, directly.
Class \code{"\linkS4class{SwePhenomenonOrNULL}"}, by class "SwePhenomenon", distance 2.

\bold{SwePhenomenon:}
Class \code{"\linkS4class{SwePhenomenonOrNULL}"}, directly.

\bold{SwePhenomenonProperty:}
Class \code{"\linkS4class{SwePhenomenonPropertyOrNULL}"}, directly.
}

\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "SweCompositePhenomenon")}: ... }
    \item{show}{\code{signature(object = "SwePhenomenon")}: ... }
    \item{show}{\code{signature(object = "SwePhenomenonProperty")}: ... }
    \item{show}{\code{signature(object = "SweTextBlock")}: ... }
	 }
}

\arguments{
Arguments of the construction functions are as follows.

  \item{id}{The character string to be used for the id attribute (mandatory).}
  \item{name}{The character string to be used for the name element (mandatory).}
  \item{description}{The character string to be used for the description element.}
  \item{dimension}{The dimensions of a composite phenomenon (mandatory).}
  \item{components}{The (sub-) components of a composite phenomenon (mandatory).}
  \item{base}{The (optional) base element for a composite phenomenon.}
  \item{href}{A reference to an (online) object instead of a inline property.}
  \item{phenomenon}{The inline phenomenon of a phenomenon property.}
  \item{tokenSeparator}{The character to be used as the token seperator, often \code{","}.}
  \item{blockSeparator}{The character to be used as the block seperator, often \code{";"}.}
  \item{decimalSeparator}{The character to be used as the decimal seperator, often \code{"."}.}
}

\details{
The \verb{...OrNULL} classes are used to model optional slots.
}

\value{The construction functions return an object of the respective class.}

\references{See section 9, SWE Common XML Encoding and Examples, of Botts, M., Robin, A. (Eds.), OpenGIS(R) Sensor Model Language (SensorML) Implementation Specification, Open Geospatial Consortium Inc., OGC 07-000}

\author{Daniel Nuest \email{daniel.nuest@uni-muenster.de}}

\examples{
showClass("SweCompositePhenomenon")
showClass("SwePhenomenon")
showClass("SwePhenomenonProperty")
showClass("SwePhenomenonPropertyOrNULL")
showClass("SweTextBlock")
}

\keyword{classes}
