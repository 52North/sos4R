\name{info}

\alias{info.features}
\alias{info.offerings}
\alias{info.phenomena}
\alias{info.sensors}
\alias{info.series}
\alias{info.sos}

\title{Information retrieval functions for Sensor Observation Services}

\description{These functions provide a high level interface to metadata of a SOS instance. They are ment to be used by non-SOS specialists to provide an immediate access to the hierarchycally ordered metadata of a capabilities document.

\code{info.series} returns a list of all valid combinations of sensors, phenomena and offerings.

\strong{WARNING}: FUNCTIONS ARE NOT IMPLEMENTED YET!
}

\usage{
info.features(sos)
info.offerings(sos)
info.phenomena(sos)
info.sensors(sos, phenomena = c(NA_character_), offerings = c(NA_character_), UOMs = c(NA_character_))
info.series(sos)
info.sos(sos, phenomena = TRUE, bbox = TRUE, timePeriod = TRUE, locations = FALSE, all = FALSE, sensors = FALSE, offerings = FALSE, features = FALSE, metadata = FALSE, operations = FALSE, UOMs = FALSE, sensor = NA_character_, phenomenon = NA_character_, offering = NA_character_, series = NA_integer_)
}

\arguments{
  \item{all}{Show all available information.}
  \item{bbox}{Filtering parameter, can be used to limit results of \code{info.sensors()} or \code{info.sos}.}
  \item{features}{Filtering parameter, can be used to limit results of \code{info.sos}.}
  \item{locations}{Filtering parameter, can be used to limit results of \code{info.sos}.}
  \item{metadata}{Filtering parameter, can be used to limit results of \code{info.sos}.}
  \item{offering}{Filtering parameter, can be used to limit results of \code{info.sos}.}
  \item{offerings}{Filtering parameter, can be used to limit results of \code{info.sensors()} or \code{info.sos}.}
  \item{operations}{Filtering parameter, can be used to limit results \code{info.sos}.}
  \item{phenomena}{Filtering parameter, can be used to limit results of \code{info.sensors()} or \code{info.sos}.}
  \item{phenomenon}{Filtering parameter, can be used to limit results of \code{info.sos}.}
  \item{sensor}{Filtering parameter, can be used to limit results of \code{info.sos}.}
  \item{sensors}{Filtering parameter, can be used to limit results of \code{info.sos}.}
  \item{series}{Filtering parameter, can be used to limit results of \code{info.sos}.}
  \item{sos}{The sos to extract the information from}
  \item{timePeriod}{Filtering parameter, can be used to limit results of \code{info.sensors()} or \code{info.sos}.}
  \item{UOMs}{Filtering parameter, can be used to limit results of \code{info.sensors()} or \code{info.sos}.}
}

%\details{}

\value{A list or a single character string with the identifiers of the found elements, e.g. features, offerings or sensors. For \code{info.series}, this is a list of all potential time series.}

%\references{}

\author{Daniel Nüst \email{daniel.nuest@uni-muenster.de}}

%\note{}

\section{Warning }{WARNING: FUNCTIONS ARE NOT IMPLEMENTED YET!}

%\seealso{}

\examples{
#TBD
}

\keyword{utilities}