\name{getFeatureOfInterest}

\alias{getFeatureOfInterest}
\alias{getFeatureOfInterest,SOS_2.0.0,character-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function retrieveing feature of Interest
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getFeatureOfInterest(sos, featureOfInterest, verbose = sos@verboseOutput, 
                     inspect = FALSE, saveOriginal = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sos}{
%%     ~~Describe \code{sos} here~~
}
  \item{featureOfInterest}{
%%     ~~Describe \code{featureOfInterest} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{inspect}{
%%     ~~Describe \code{inspect} here~~
}
  \item{saveOriginal}{
%%     ~~Describe \code{saveOriginal} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
structure(function (sos, featureOfInterest, verbose = sos@verboseOutput, 
    inspect = FALSE, saveOriginal = FALSE) 
{
    standardGeneric("getFeatureOfInterest")
  }, generic = structure("getFeatureOfInterest", package = "sos4R"), 
  package = "sos4R", group = list(), valueClass = character(0), 
  signature = c("sos", "featureOfInterest", "verbose", "inspect", "saveOriginal"),
  default = NULL, skeleton = (function (sos, 
    featureOfInterest, verbose = sos@verboseOutput, inspect = FALSE, 
    saveOriginal = FALSE) 
stop("invalid call in method dispatch to 'getFeatureOfInterest' (no default method)", 
    domain = NA))(sos, featureOfInterest, verbose, inspect, saveOriginal), 
    class = structure("nonstandardGenericFunction", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
