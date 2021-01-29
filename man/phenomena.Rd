\name{phenomena}

\alias{phenomena}
\alias{phenomena-methods}
\alias{phenomena,SOS_2.0.0-method}

\title{Function retrieve phenomena of a SOS v2.0}
\description{
This function queries a SOS v2.0 for all its phenomena
}
\usage{
phenomena(sos, includeTemporalBBox = FALSE, includeSiteId = FALSE, ...)
}
\arguments{
  \item{sos}{An SOS object from a call to \code{\link[sos4R]{SOS}} containing the URL of a SOS.}
  \item{includeTemporalBBox}{Whether the observation containing timespan of each phenomenon shall also be reported.}
  \item{includeSiteId}{Whether the sites shall be reported at which each phenomenon has been observed.}
  \item{...}{Named arguments forwarded to other wrapper functions internally.}
}
\value{
If only the argument \code{sos} is provided, this function returns a character vector with the phenomenon ids.

If \code{timeInfo = TRUE}, the result is a \code{data.frame} where the character vector of phenomena is combined with a character columns \code{beginTime} and \code{endTime}. Note that these two columns indicate the very beginning and end of any time series recorded for each phenomenon \bold{across all sites} and does not imply any continuity.

If \code{sites = TRUE}, the result is a \code{data.frame} where the character vector of phenomena is combined with a character column \code{siteID}. Phenomena that have been observed at several sites will be replicated for each site. If also \code{timeInfo = TRUE}, the \code{data.frame} will have character columns \code{beginTime} and \code{endTime} per phenomenon and site. Note that these two columns indicate the very beginning and end of any time series recorded for each phenomenon \bold{at each site}, but does still not imply to be continuous series.
}

\author{
Benedikt Graeler, Eike Hinderk Juerrens
}

\examples{
\dontrun{
mySos <- SOS(url = "https://climate-sos.niwa.co.nz/",
             binding = "KVP", useDCPs = FALSE, version = "2.0.0")

phenomena(sos = mySos)
}
}