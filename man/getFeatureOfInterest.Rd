\name{getFeatureOfInterest}

\alias{getFeatureOfInterest}
\alias{getFeatureOfInterest,SOS_2.0.0,ANY-method}
\alias{getFeatureOfInterest,SOS_2.0.0,character-method}

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

\arguments{
  \item{sos}{The Sensor Observation Service from which features of interest should be retrieved}
  \item{featureOfInterest}{identifier(s) of features of interest}
  \item{verbose}{A boolean value indicating whether debug information is printed out to the console during the execution}
  \item{inspect}{A boolean value to enable printing of the sent request and received response to the console}
  \item{saveOriginal}{Save the received document to the current working directory. If \code{TRUE} a filename is automatically generated, if a character string is given it is used as the filename}
}

\value{
An object of class \link{SosGetFeatureOfInterest_2.0.0-class}.
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
