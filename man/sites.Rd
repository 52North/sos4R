\name{sites}

\alias{sites}
\alias{sites-methods}
\alias{sites,SOS_2.0.0-method}

\title{
This function queries a SOS v2.0 for its sites and returns them as SpatialPointsDataFrame
}

\description{
Information about the registered sites in a SOS can be queried using this function. This function allows for different levels of detail and returns a SpatialPointsDataFrame.
}
\usage{
sites(sos,
      begin = NA,
      end = NA,
      phenomena = list(),
      empty = FALSE,
      includePhenomena = FALSE,
      includeTemporalBBox = FALSE,
      ...)
}

\arguments{
  \item{sos}{A SOS object from a call to \code{\link[sos4R]{SOS}} containing the URL of a SOS.}
  \item{begin}{Object of class \code{POSIXt}. Note that creating timestamps with \code{as.POSIXct(..)} will use your local timezone.}
  \item{end}{Object of class \code{POSIXt}. Note that creating timestamps with \code{as.POSIXct(..)} will use your local timezone.}
  \item{phenomena}{A list, vector or one-column data.frame with characters identifying the relevant phenomena.}
  \item{empty}{Whether also empty sites shall be included in the response.}
  \item{includePhenomena}{Whether the phenomena shall also be reported in the result.}
  \item{includeTemporalBBox}{Whether the observation containing timespan of each phenomenon shall also be reported.}
  \item{...}{Named parameters passed on to other wrapper functions.}
}

\value{
A \code{\link{SpatialPointsDataFrame-class}} containing all sites that match the filter conditions with corresponding metadata defined via the arguments. If \code{empty = TRUE}, the data slot contains a logical column 'empty' indicating whether any data has been observed for this site. If \code{listPhenomena = TRUE}, the data.frame contains a logical column per phenomenon indicating whether this specific phenomenon has been observed at that site. If \code{includeTemporalBBox = TRUE}, the data.frame contains a two column data.frame with \code{beginTime} and \code{endTime} per phenomenon.
}

\author{
Benedikt Graeler, Eike Hinderk Juerrens
}

\examples{
\dontrun{
mySos <- SOS(url = "https://climate-sos.niwa.co.nz/",
             binding = "KVP", useDCPs = FALSE, version = "2.0.0")

sites(sos = mySos)
}
}
