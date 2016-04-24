\name{OmOM_Observation}
\alias{OmOM_Observation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
OmOM_Observation(phenomenonTime, procedure, observedProperty, featureOfInterest, result, metadata = NA, resultTime = NULL, resultQuality = NA, parameter = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{phenomenonTime}{
%%     ~~Describe \code{phenomenonTime} here~~
}
  \item{procedure}{
%%     ~~Describe \code{procedure} here~~
}
  \item{observedProperty}{
%%     ~~Describe \code{observedProperty} here~~
}
  \item{featureOfInterest}{
%%     ~~Describe \code{featureOfInterest} here~~
}
  \item{result}{
%%     ~~Describe \code{result} here~~
}
  \item{metadata}{
%%     ~~Describe \code{metadata} here~~
}
  \item{resultTime}{
%%     ~~Describe \code{resultTime} here~~
}
  \item{resultQuality}{
%%     ~~Describe \code{resultQuality} here~~
}
  \item{parameter}{
%%     ~~Describe \code{parameter} here~~
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
function (phenomenonTime, procedure, observedProperty, featureOfInterest, 
    result, metadata = NA, resultTime = NULL, resultQuality = NA, 
    parameter = NA) 
{
    new("OmOM_Observation", phenomenonTime = phenomenonTime, 
        procedure = procedure, observedProperty = observedProperty, 
        featureOfInterest = featureOfInterest, result = result, 
        metadata = metadata, resultTime = resultTime, resultQuality = resultQuality, 
        parameter = parameter)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
