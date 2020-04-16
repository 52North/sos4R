\name{getDataAsST}
\alias{getDataAsST}

\title{
This function queries a SOS v2.0 for time series data and returns it as ST-object
}
\description{
For given sites and phenomena, a SOS is queried and the data returned as ST-object.
}
\usage{
getDataAsST(sos, phenomena, sites, spatialBBox = NA, timeInterval = NA_character_)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
   \item{sos}{
   An \code{\link[sos4R]{SOS}} object containing the URL of a SOS v2.0.
}
\item{phenomena}{
A list, vector or one-column data.frame with characters identifying the relevant phenomena.
}
  \item{sites}{
A list, vector or one-column data.frame with characters identifying the relevant sites.
}
  \item{spatialBBox}{
A 2-by-2 matrix with \code{x} and \code{y} in the rows and \code{min} and \code{max} in the columns. See \code{\link[sp]{Spatial}} for details. If \code{sites} is set, this argument is ignored.
}
  \item{timeInterval}{
A character defining an ISO-String (ISO 8601), see also \link[xts]{xts}.
}
}

\value{
An ST-object containing the data where the spatial and temporal information are contained in the \code{sp} and \code{time} slots respectively. The measurements are contained for each phenomenon in a separate column and are provided with units using \code{\link[units]{units}}.}

\author{
Benedikt Graeler, Eike Hinderk Juerrens
}
\examples{
## The function is currently defined as
function (sos, phenomena, sites, spatialBBox = NA, timeInterval = NA_character_)
{
    stopifnot(inherits(sos, "SOS_2.0.0"))
    stopifnot(is.character(timeInterval))
    if (missing(sites) && is.na(spatialBBox))
        stop("Either 'sites' or 'spatialBBox' must be provided.")
    phenomena <- .validateListOrDfColOfStrings(phenomena, "phenomena")
    if (!missing(sites)) {
        sites <- .validateListOrDfColOfStrings(sites, "sites")
        if (!is.na(spatialBBox)) {
            spatialBBox <- NA
            warning("'spatialBBox' has been ignored and 'sites' is used instead.")
        }
    }
  }
}
