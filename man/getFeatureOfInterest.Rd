\name{getFeatureOfInterest}

\alias{getFeatureOfInterest}
\alias{getFeatureOfInterest,SOS_2.0.0,character-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function retrieving features of interest, i.e. the representations of the real world features that are observed and for which observations are provided.
}
\description{
This function retrieves , i.e. the representations of the real world features that are observed and for which observations are provided, from a Sensor Observation Service.
}
\usage{
getFeatureOfInterest(sos, featureOfInterest, verbose = sos@verboseOutput, 
                     inspect = FALSE, saveOriginal = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sos}{
The Sensor Observation Service from which features of interest shoudl be retrieved.
}
  \item{featureOfInterest}{
identifier(s) of features of interest
}
  \item{verbose}{
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

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
