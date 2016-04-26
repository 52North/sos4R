\name{OmOM_Observation-class}

\Rdversion{1.1}

\docType{class}

\alias{OmOM_Observation-class}
\alias{OmOM_ObservationOrNULL-class}
\alias{OmOM_Observation}

\alias{sosCoordinates,OmOM_Observation-method}
\alias{sosFeatureIds,OmOM_Observation-method}
\alias{sosFeaturesOfInterest,OmOM_Observation-method}

\title{Class \code{"OmOM_Observation"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("OmOM_Observation", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{phenomenonTime}:}{Object of class \code{"GmlTimeObjectOrNULL"} ~~ }
    \item{\code{procedure}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{observedProperty}:}{Object of class \code{"SwePhenomenonPropertyOrNULL"} ~~ }
    \item{\code{featureOfInterest}:}{Object of class \code{"GmlFeatureOrGmlFeaturePropertyOrNULL"} ~~ }
    \item{\code{result}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{metadata}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{resultTime}:}{Object of class \code{"GmlTimeObjectOrNULL"} ~~ }
    \item{\code{resultQuality}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{parameter}:}{Object of class \code{"ANY"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{OmOM_ObservationOrNULL}"}, directly.
}
\section{Methods}{
  \describe{
    \item{sosCoordinates}{\code{signature(obj = "OmOM_Observation")}: ... }
    \item{sosFeatureIds}{\code{signature(obj = "OmOM_Observation")}: ... }
    \item{sosFeaturesOfInterest}{\code{signature(obj = "OmOM_Observation")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("OmOM_Observation")
}
\keyword{classes}
