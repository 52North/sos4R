\name{siteList}
\alias{siteList}
\title{
This function queries a SOS v2.0 for its sites and returns them as data.frame
}
\description{
Information about the registered sites in a SOS can be queried using this function. This function allows for different levels of detail and returns a vector or data.frame.
}
\usage{
siteList(sos, empty = FALSE, timeInterval = NA_character_,
         listPhenomena = FALSE, includeTemporalBBox = FALSE,
         phenomena = list())
}

\arguments{
  \item{sos}{
A SOS object from a call to \code{\link[sos4R]{SOS}} containing the URL of a SOS.
}
  \item{empty}{
Whether also empty sites shall be included in the response.
}
  \item{timeInterval}{
A character defining an ISO-String (ISO 8601), see also \link[xts]{xts}. Only sites with data within this interval will be retrieved.
}
  \item{listPhenomena}{
Whether the phenomena shall be listed per location that can be observed.
}
  \item{includeTemporalBBox}{
Whether the observation containing timespan of each phenomenon shall also be reported.
}
  \item{phenomena}{
A list, vector or one-column data.frame with characters identifying the relevant phenomena; only sites where these phenomena can be observed are retrieved.
}
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
## The function is currently defined as
function (sos, empty = FALSE, timeInterval = NA_character_, listPhenomena = FALSE,
    includeTemporalBBox = FALSE, phenomena = list())
{
    stopifnot(inherits(sos, "SOS_2.0.0"))
    stopifnot(is.logical(empty))
    stopifnot(is.character(timeInterval))
    stopifnot(is.logical(listPhenomena))
    stopifnot(is.logical(includeTemporalBBox))
    phenomena <- .validateListOrDfColOfStrings(phenomena, "phenomena")
    if (includeTemporalBBox && !listPhenomena) {
        listPhenomena <- TRUE
        warning("'listPhenomena' has been set to 'TRUE' as this is required for 'includeTemporalBBox'.")
    }
  }
}
