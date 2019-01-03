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

\value{
An object of class \link{SosGetFeatureOfInterest-class}.
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
