\name{read.sos}

\alias{read.sos}

\title{Read Data from a SOS Connection}

\description{
This function provides a high level interface to read data from a SOS connection in an abstract and uncomplicated manner. It wraps the actual request operation (i.e. \verb{GetObservation}) and also uses different terms for soem properties.

\strong{WARNING}: FUNCTIONS ARE NOT IMPLEMENTED YET! But any ideas or input is highly appreciated.
}

\usage{
read.sos(sos, sensors = NA_character_, phenomena = NA_character_, bbox = NA_character_, times = c(NA), mergeResult = FALSE, addLocation = FALSE, verbose = FALSE)
}

\arguments{
  \item{sos}{The SOS to read data from.}
  \item{sensors}{A character array with sensor identifier(s) to limit data to certain sensors.}
  \item{phenomena}{A character array with phenomenon identifier(s) to limit data to certain phenomena.}
  \item{bbox}{A bounding box representation to request a certain spatial extend of data.}
  \item{times}{A vector of times (e.g. start and end time) as POSIXt classes or character string that can be parsed to POSIXt classes depicting a temporal extend.}
  \item{mergeResult}{A flag to turn merging of the results into a single data structure on or off.}
  \item{addLocation}{A flag to turn the adding of the location to every data set in the result on or off.}
  \item{verbose}{Turn verbose logging on/off.}
}

%\details{}
\value{An object or a list of object from class \code{OmObservation} or \code{OmMeasurement}.}

%\references{}

\author{Daniel Nüst \email{daniel.nuest@uni-muenster.de}}

%\note{}
%\section{Warning }{}

\seealso{See Also as \code{\link{getObservation}}.}

\examples{
# TBD
}

\keyword{utilities}
