\name{SosGetObservation_2.0.0}
\alias{SosGetObservation_2.0.0}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SosGetObservation_2.0.0(service, version, offering, observedProperty, responseFormat, srsName = as.character(NA), eventTime = list(NA), procedure = as.character(NA), featureOfInterest = NULL, result = NULL, resultModel = as.character(NA), responseMode = as.character(NA), BBOX = as.character(NA))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{service}{
%%     ~~Describe \code{service} here~~
}
  \item{version}{
%%     ~~Describe \code{version} here~~
}
  \item{offering}{
%%     ~~Describe \code{offering} here~~
}
  \item{observedProperty}{
%%     ~~Describe \code{observedProperty} here~~
}
  \item{responseFormat}{
%%     ~~Describe \code{responseFormat} here~~
}
  \item{srsName}{
%%     ~~Describe \code{srsName} here~~
}
  \item{eventTime}{
%%     ~~Describe \code{eventTime} here~~
}
  \item{procedure}{
%%     ~~Describe \code{procedure} here~~
}
  \item{featureOfInterest}{
%%     ~~Describe \code{featureOfInterest} here~~
}
  \item{result}{
%%     ~~Describe \code{result} here~~
}
  \item{resultModel}{
%%     ~~Describe \code{resultModel} here~~
}
  \item{responseMode}{
%%     ~~Describe \code{responseMode} here~~
}
  \item{BBOX}{
%%     ~~Describe \code{BBOX} here~~
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
function (service, version, offering, observedProperty, responseFormat, 
    srsName = as.character(NA), eventTime = list(NA), procedure = as.character(NA), 
    featureOfInterest = NULL, result = NULL, resultModel = as.character(NA), 
    responseMode = as.character(NA), BBOX = as.character(NA)) 
{
    new("SosGetObservation_2.0.0", request = sosGetObservationName, 
        service = service, version = version, offering = offering, 
        observedProperty = observedProperty, responseFormat = responseFormat, 
        srsName = srsName, eventTime = eventTime, procedure = procedure, 
        featureOfInterest = featureOfInterest, result = result, 
        resultModel = resultModel, responseMode = responseMode, 
        BBOX = BBOX)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
