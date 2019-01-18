## handy top level sos functions

# phenomena ####
# ~ us.1.1 ####
# What phenomena are available from a SOS?
#   phenomena(sos)
# → list[siteID]
# GetCapabilities::Contents
#
# ~ us.1.2 ####
# What phenomena are available from a SOS and what are their temporal ranges?
#   phenomena(sos, includeTemporalBBox=TRUE)
# → data.frame[phenomena, timeBegin, timeEnd]
# GetDataAvailability v1.0
#
# ~ us.1.4 ####
# What phenomena are available from a SOS and at which locations
#
# phenomena(sos, listSites=TRUE)
# →
# data.frame[phenomenon, siteID]
# GetDataAvailability v1.0

phenomena <- function(sos, includeTemporalBBox=FALSE, includeSiteId=FALSE) {
  stopifnot(inherits(sos, "SOS_2.0.0"))
  stopifnot(is.logical(includeTemporalBBox))
  stopifnot(is.logical(includeSiteId))
}

# siteList ####
# ~ us.2.1: List all sites (containing data) ####
#   siteList(sos)
# → list[siteID]
#
# ~ us.2.2: List all sites (also not containing data) ####
#   siteList(sos, empty=TRUE)
# → list[siteID]
#
# ~ us.2.3: List all sites w/wo data for a given time window ####
#   siteList(sos, timeInterval=ISO-String)
# → list[siteID]
#
# ~ us.2.4: List all sites with metadata ####
# siteList(sos, includePhenomena=T|F, includeTemporalBBox=T|F, ...)
# → data.frame[siteid, phenomenon, beginTime, endTime]
#
# ~ us.2.5: List all sites where specified phenomena have been captured ####
#   siteList(sos, phenomena=[List of phenomena])
# → data.frame[siteID, phenomenon, timeBegin, timeEnd]

.validateListOrDfColOfStrings <- function(los, argName) {
  if (is.data.frame(los)) {
    stopifnot(ncol(los) > 0)
    if (ncol(los) > 1)
      warning(paste0("Using the first column of '", argName, "' as filter."))
    los <- los[,1]
  }

  stopifnot(is.list(los) || is.vector(los))

  if (length(los) > 0) {
    stopifnot(all(sapply(los, is.character)))
    if (any(is.na(los))) {
      stop(paste0("The argument '", argName, "' must not contain any NAs."))
    }
  } else {
    stop(paste0("The argument '", argName, "' must not be empty."))
  }

  return(los)
}

siteList <- function(sos,
                     empty=FALSE, # filter
                     timeInterval=NA_character_,  # filter
                     includePhenomena=FALSE, # meta data
                     includeTemporalBBox=FALSE, # meta data
                     phenomena=list()) { # filter
  stopifnot(inherits(sos, "SOS_2.0.0"))
  stopifnot(is.logical(empty))
  stopifnot(is.character(timeInterval))
  stopifnot(is.logical(includePhenomena))
  stopifnot(is.logical(includeTemporalBBox))

  phenomena <- .validateListOrDfColOfStrings(phenomena, "phenomena")

  if (includeTemporalBBox && !includePhenomena) {
    includePhenomena <- TRUE
    warning("'includePhenomena' has been set to 'TRUE' as this is required for 'includeTemporalBBox'.")
  }

}

# sites ####
# ~ us.2.1: List all sites (containing data) ####
# sites(sos)
# → SpatialPointsDataFrame[siteID] + coords
#
# ~ us.2.2: List all sites (also not containing data) ####
# sites(sos, empty=TRUE)
# → SpatialPointsDataFrame[siteID, Empty=logical] + coords
#
# ~ us.2.3: List all sites with data for a given time window ####
# sites(sos, timeInterval=ISO-String)
# → SpatialPointsDataFrame[siteID, Empty=logical] + coords

# ~ us.2.4: List all sites with metadata ####
# sites(sos, includePhenomena=F, includeTemporalBBox=F) = sites(sos)
# → see us.2.1/us.2.2
#
# sites(sos, includePhenomena=T, includeTemporalBBox=F)
# → SpatialPointsDataFrame[siteID, phen_1=logical, …, phen_n=logical]
#
# sites(sos, includePhenomena=T, includeTemporalBBox=T)
# → SpatialPointsDataFrame[phen_1=df[beginTime, endTime], …, phen_n=df[beginTime, endTime]] + coords
#
# ~ us.2.5: List all sites where specified phenomena have been captured ####
# sites(sos, phenomena=[List of phenomena])
# → SpatialPointsDataFrame[phen_1=df[beginTime, endTime], …, phen_n=df[beginTime, endTime]] + coords

sites <- function(sos,
                  empty=FALSE,
                  timeInterval=NA_character_,
                  includePhenomena=FALSE,
                  includeTemporalBBox=FALSE,
                  phenomena=list()) {
  stopifnot(inherits(sos, "SOS_2.0.0"))
  stopifnot(is.logical(empty))
  stopifnot(is.character(timeInterval))
  stopifnot(is.logical(includePhenomena))
  stopifnot(is.logical(includeTemporalBBox))

  phenomena <- .validateListOrDfColOfStrings(phenomena, "phenomena")

  if (includeTemporalBBox && !includePhenomena) {
    includePhenomena <- TRUE
    warning("'includePhenomena' has been set to 'TRUE' as this is required for 'includeTemporalBBox'.")
  }
}

# getData ####
# use Units package!
#
# ~ us.3.1: Retrieve sensor values by phenomenon/a and single site/list of sites ####
# getData(sos, phenomena=[List of phenomena], sites=[List of sites])
# →
# data.frame[siteID, timestamp, phen_1, phen_2, …]
#
# ~ us.3.2: Retrieve sensor values by phenomenon/a and spatial bounding box (in the CRS of the SOS) ####
# getData(sos, phenomena=[List of phenomena], spatialBBox)
# →
# data.frame[siteID, timestamp, phen_1, phen_2, …]
#
# ~ us.3.3: Temporal Filter for us.3.1 and us.3.2 ####
# getData(sos, …, timeInterval=ISO-String)
# →
# data.frame[siteID, timestamp, phen_1, phen_2, …]

getData <- function(sos,
                    phenomena, # no default to force the user to actively pick phenomena
                    sites, # no default to force the user to actively pick sites
                    spatialBBox=NA,
                    timeInterval=NA_character_) {
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

# getDataAsST ####
# use Units package!
#
# ~ us.3.1: Retrieve sensor values by phenomenon/a and single site/list of sites ####
# getDataAsST(sos, phenomena=[List of phenomena], sites=[List of sites])
# →
# STSDF[phen_1, phen_2, …] + coords + time + index
#
# ~ us.3.2: Retrieve sensor values by phenomenon/a and spatial bounding box (in the CRS of the SOS) ####
# getDataAsST(sos, phenomena=[List of phenomena], spatialBBox=matrix/c(minX,minY,maxX,maxY))
# →
# STSDF[phen_1, phen_2, …] + coords + time + index
#
# ~ us.3.3: Temporal Filter for us.3.1 and us.3.2 ####
# getDataAsST(sos, …, timeInterval=ISO-String)
# →
# STSDF[phen_1, phen_2, …] + coords + time + index

getDataAsST <- function(sos,
                        phenomena, # no default to force the user to actively pick phenomena
                        sites, # no default to force the user to actively pick sites
                        spatialBBox=NA,
                        timeInterval=NA_character_) {
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

  if (!is.na(spatialBBox)) {
    if (is.vector(spatialBBox)) {
      stopifnot(length(spatialBBox) == 4)
      spatialBBox <- matrix(spatialBBox, nrow = 2)
      colnames(spatialBBox) <- c("min", "max")
    }
  }

}
