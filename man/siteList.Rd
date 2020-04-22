\name{siteList}

\alias{siteList}
\alias{siteList-methods}
\alias{siteList,SOS_2.0.0-method}

\title{
This function queries a SOS v2.0 for its sites and returns them as data.frame
}
\description{
Information about the registered sites in a SOS can be queried using this function. This function allows for different levels of detail and returns a vector or data.frame.
}
\usage{
siteList(sos,
         empty = FALSE,
         begin = NA,
         end = NA,
         phenomena = list(),
         includePhenomena = FALSE,
         includeTemporalBBox = FALSE,
         ...)
}

\arguments{
  \item{sos}{A SOS object from a call to \code{\link[sos4R]{SOS}} containing the URL of a SOS.}
  \item{empty}{Whether also empty sites shall be included in the response.}
  \item{begin}{Object of class \code{POSIXt}. Note that creating timestamps with \code{as.POSIXct(..)} will use your local timezone.}
  \item{end}{Object of class \code{POSIXt}. Note that creating timestamps with \code{as.POSIXct(..)} will use your local timezone.}
  \item{phenomena}{A list, vector or one-column data.frame with characters identifying the relevant phenomena.}
  \item{includePhenomena}{Whether the phenomena shall also be reported in the result.}
  \item{includeTemporalBBox}{Whether the observation containing timespan of each phenomenon shall also be reported.}
  \item{...}{Named parameters passed on to other wrapper functions.}
}

\value{
If only the argument \code{sos} is provided and the flag \code{empty} is set to \code{TRUE} or \code{FALSE}, this function returns a character vector with the site ids.

If \code{listPhenomena = TRUE}, the data.frame contains the column containing the site ids and a character column phenomenon where sites with several phenomena are repeated for each of their phenomenon.

If \code{includeTemporalBBox = TRUE}, the data.frame also contains two character columns \code{beginTime} and \code{endTime} indicating per site id and phenomenon its earliest and latest observation time point.
}
\author{
Benedikt Graeler, Eike Hinderk Juerrens
}

\examples{
\dontrun{
mySos <- SOS(url = "https://climate-sos.niwa.co.nz/",
             binding = "KVP", useDCPs = FALSE, version = "2.0.0")
siteList(sos = mySos)
}
}
