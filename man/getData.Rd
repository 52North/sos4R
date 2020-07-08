\name{getData}
\alias{getData}

\title{
This function queries a SOS v2.0 for time series data
}

\description{
For given sites and phenomena, a SOS is queried and the data returned as data.frame.
}

\usage{
getData(sos, phenomena, sites, spatialBBox = NA, begin = NA, end = NA, ...)
}

\arguments{
  \item{sos}{An SOS object from a call to \code{\link[sos4R]{SOS}} containing the URL of a SOS v2.0.}
  \item{phenomena}{A list, vector or one-column data.frame with characters identifying the relevant phenomena.}
  \item{sites}{A list, vector or one-column data.frame with characters identifying the relevant sites.}
  \item{spatialBBox}{A 2-by-2 matrix with \code{x} and \code{y} in the rows and \code{min} and \code{max} in the columns. See \code{\link{Spatial-class}} for details. If \code{sites} is set, this argument is ignored.}
  \item{begin}{Object of class \code{POSIXt}. Note that creating timestamps with \code{as.POSIXct(..)} will use your local timezone.}
  \item{end}{Object of class \code{POSIXt}. Note that creating timestamps with \code{as.POSIXct(..)} will use your local timezone.}
  \item{...}{Named arguments forwarded to other wrapper functions internally.}
  }

\value{
A data.frame containing the data in long form where each row contains a \code{siteID}, the time stamp of the observation and the observed measurements for each phenomenon in a separate column. The measurements are provided with units using \code{\link[units]{units}}.
}
\author{
Benedikt Graeler, Eike Hinderk Juerrens
}

\examples{
\dontrun{
mySos <- SOS(url = "https://climate-sos.niwa.co.nz/",
             binding = "KVP", useDCPs = FALSE, version = "2.0.0")

observationData <- getData(sos = mySos,
                           phenomena = phenomena[18,1],
                           sites = siteList[1:2,1])
}
}
