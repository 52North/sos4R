################################################################################
# Copyright (C) 2015 by 52 North                                               #
# Initiative for Geospatial Open Source Software GmbH                          #
#                                                                              #
# Contact: Andreas Wytzisk                                                     #
# 52 North Initiative for Geospatial Open Source Software GmbH                 #
# Martin-Luther-King-Weg 24                                                    #
# 48155 Muenster, Germany                                                      #
# info@52north.org                                                             #
#                                                                              #
# This program is free software; you can redistribute and/or modify it under   #
# the terms of the GNU General Public License version 2 as published by the    #
# Free Software Foundation.                                                    #
#                                                                              #
# This program is distributed WITHOUT ANY WARRANTY; even without the implied   #
# WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU #
# General Public License for more details.                                     #
#                                                                              #
# You should have received a copy of the GNU General Public License along with #
# this program (see gpl-2.0.txt). If not, write to the Free Software           #
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or #
# visit the Free Software Foundation web page, http://www.fsf.org.             #
#                                                                              #
# Author: - Ben Graler (b.graeler@52north.org)                                 #
#         - Eike Hinderk Jürrens (e.h.juerrens@52north.org)                    #
# Created: 2019-01-18                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
################################################################################
## handy top level sos functions

library(sp)

# phenomena ####
# ~ us.1.1 ####
# What phenomena are available from a SOS?
#   phenomena(sos)
# → list[phenomenonId]
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

#
# phenomena - generic method ----
#
if (!isGeneric("phenomena")) {
  setGeneric(name = "phenomena",
             signature = signature("sos",
                                   "includeTemporalBBox",
                                   "includeSiteId"),
             def = function(sos,
                            includeTemporalBBox = FALSE,
                            includeSiteId = FALSE) {
               standardGeneric("phenomena")
             })
}

#
# phenomena - call with sos parameter only ----
#
setMethod(f = "phenomena",
          signature = signature(sos = "SOS_2.0.0"),
          def = function(sos,
                         includeTemporalBBox,
                         includeSiteId) {
            stopifnot(inherits(sos, "SOS_2.0.0"))
            stopifnot(is.logical(includeTemporalBBox))
            stopifnot(is.logical(includeSiteId))
            if (!includeTemporalBBox && !includeSiteId) {
              return(.listPhenomena(sos))
            }
            else if (includeTemporalBBox && !includeSiteId) {
              return(.listPhenomenaWithTemporalBBox(sos))
            }
            else if (!includeTemporalBBox && includeSiteId) {
              stop("return(.listPhenomenaWithSiteIds(sos)) not implemented")
            }
            else if (includeTemporalBBox && includeSiteId) {
              stop("return(.listPhenomenaWithTemporalBBoxAndSiteIds(sos)) not implemented")
            }
          })
#
# phenomena(sos) → data.frame[phenomenon] using GetCapabilities::Contents
#
# see: https://github.com/52North/sos4R/issues/81
#
.listPhenomena <- function(sos) {
  .phenomena <- sosObservableProperties(sos)
  stopifnot(!is.null(.phenomena))
  stopifnot(is.list(.phenomena))
  if (length(unlist(.phenomena)) == 0) {
    .phens <- data.frame("phenomenon" = character(0), stringsAsFactors = FALSE)
  } else {
    .phenomena <- unique(sort(as.vector(unlist(.phenomena))))
    .phens <- data.frame("phenomenon" = .phenomena, stringsAsFactors = FALSE)
  }
  return(.phens)
}

#
# phenomena(sos, includeTemporalBBox=TRUE) → data.frame[phenomenon, timeBegin, timeEnd]
#
# see: https://github.com/52North/sos4R/issues/82
#
# <gda:dataAvailabilityMember gml:id="dam_1">
#   <gda:procedure xlink:href="ws2500" xlink:title="ws2500"/>
#   <gda:observedProperty xlink:href="WindDirection" xlink:title="WindDirection"/>
#   <gda:featureOfInterest xlink:href="elv-ws2500" xlink:title="ELV WS2500"/>
#   <gda:phenomenonTime>
#     <gml:TimePeriod gml:id="tp_1">
#       <gml:beginPosition>2019-03-01T00:30:00.000Z</gml:beginPosition>
#       <gml:endPosition>2019-03-28T23:45:00.000Z</gml:endPosition>
#     </gml:TimePeriod>
#   </gda:phenomenonTime>
# </gda:dataAvailabilityMember>
#
.listPhenomenaWithTemporalBBox <- function(sos) {
  .dams <- getDataAvailability(sos, verbose = sos@verboseOutput)
  stopifnot(!is.null(.dams))
  stopifnot(is.list(.dams))
  if (length(.dams) == 0) {
    .phenomena <- data.frame("phenomenon" = character(0),
                          "timeBegin" = double(0),
                          "timeEnd" = double(0),
                        stringsAsFactors = FALSE)
  }
  else {
    .phenomena <- data.frame("phenomenon" = character(0),
                        "timeBegin" = double(0),
                        "timeEnd" = double(0),
                        stringsAsFactors = FALSE)
    for (.dam in .dams) {
      # check if phenomenon aka observed property is in data.frame
      if (.dam@observedProperty %in% .phenomena[, 1]) {
        # if yes -> merge times
        if (.dam@phenomenonTime@beginPosition@time < .phenomena[.phenomena$phenomenon == .dam@observedProperty, 2]) {
          .phenomena[.phenomena$phenomenon == .dam@observedProperty, 2] <- list(.dam@phenomenonTime@beginPosition@time)
        }
        if (.dam@phenomenonTime@endPosition@time > .phenomena[.phenomena$phenomenon == .dam@observedProperty, 3]) {
          .phenomena[.phenomena$phenomenon == .dam@observedProperty, 3] <- list(.dam@phenomenonTime@endPosition@time)
        }
      }
      else {
        # if not -> append at the end
        .phenomena <- rbind(.phenomena, data.frame("phenomenon" = .dam@observedProperty,
                                         "timeBegin" = .dam@phenomenonTime@beginPosition@time,
                                         "timeEnd" = .dam@phenomenonTime@endPosition@time,
                                         stringsAsFactors = FALSE))
      }
    }
  }
  return(.phenomena)
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

#
# siteList - generic method ----
#
if (!isGeneric("siteList")) {
  setGeneric(name = "siteList",
             signature = signature("sos",
                                   "empty",
                                   "timeInterval",
                                   "includePhenomena",
                                   "includeTemporalBBox",
                                   "phenomena"),
             def = function(sos,
                            empty=FALSE,                 # filter
                            timeInterval=NA_character_,  # filter
                            includePhenomena=FALSE,      # meta data
                            includeTemporalBBox=FALSE,   # meta data
                            phenomena=list()) {          # filter
               standardGeneric("siteList")
             })
}

#
# siteList - call with sos parameter only ----
#
setMethod(f = "siteList",
          signature = signature(sos = "SOS_2.0.0"),
          def = function(sos,
                         empty,
                         timeInterval,
                         includePhenomena,
                         includeTemporalBBox,
                         phenomena) {
            stopifnot(inherits(sos, "SOS_2.0.0"))
            stopifnot(is.logical(empty))
            stopifnot(is.character(timeInterval))
            stopifnot(is.logical(includePhenomena))
            stopifnot(is.logical(includeTemporalBBox))

            .phenomenaSet <- FALSE
            .timeIntervalSet <- FALSE

            # validate input only if given
            if (.isPhenomenaSet(phenomena)) {
              phenomena <- .validateListOrDfColOfStrings(phenomena, "phenomena")
              .phenomenaSet <- TRUE
            }
            if (.isTimeIntervalSet(timeInterval)) {
              .timeInterval <- .validateISO8601String(timeInterval)
              .timeIntervalSet <- TRUE
            }

            if (includeTemporalBBox && !includePhenomena) {
              includePhenomena <- TRUE
              warning("'includePhenomena' has been set to 'TRUE' as this is required for 'includeTemporalBBox'.")
            }

            if (empty && !.phenomenaSet && !.timeIntervalSet && !includePhenomena && !includeTemporalBBox) {
              return(.listSites(sos))
            }

            if (!empty && !.phenomenaSet && !.timeIntervalSet && !includePhenomena && !includeTemporalBBox) {
              return(.listSitesWithData(sos))
            }
          }
)

.isTimeIntervalSet <- function(timeInterval) {
  return(!is.null(timeInterval) &&
           !is.na(timeInterval) &&
           is.character(timeInterval) &&
           length(timeInterval) > 0)
}

.validateISO8601String <- function(timeInterval) {
  stop(".validateISO8601String NOT YET IMPLEMENTED")
}

.isPhenomenaSet <- function(phenomena) {
  return(!is.null(phenomena) &&
           !is.na(phenomena) &&
           is.list(phenomena) &&
           length(phenomena) > 0)
}

.listSites <- function(sos) {
  .features <- getFeatureOfInterest(sos)
  stopifnot(!is.null(.features))
  stopifnot(is.list(.features))
  if (length(unlist(.features)) == 0) {
    .sites <- data.frame("siteID" = character(0), stringsAsFactors = FALSE)
  } else {
    .sites <- data.frame("siteID" = unique(sort(as.vector(unlist(sosFeaturesOfInterest(.features))))), stringsAsFactors = FALSE)
  }
  return(.sites)
}

.listSitesWithData <- function(sos) {
  .dams <- getDataAvailability(sos, verbose = sos@verboseOutput)
  stopifnot(!is.null(.dams))
  stopifnot(is.list(.dams))
  if (length(.dams) == 0) {
    .sites <- data.frame("siteID" = character(0),
                             stringsAsFactors = FALSE)
  }
  else {
    .sites <- data.frame("siteID" = character(0),
                             stringsAsFactors = FALSE)
    for (.dam in .dams) {
      # check if siteID aka observed property is in data.frame
      if (!(.dam@featureOfInterest %in% .sites[, 1])) {
        # if not -> append at the end
        .sites <- rbind(.sites, data.frame("siteID" = .dam@featureOfInterest,
                                                   stringsAsFactors = FALSE))
      }
    }
    .sites <- data.frame("siteID" = .sites[order(.sites$siteID),], stringsAsFactors = FALSE)
  }
  return(.sites)
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

#
# sites - generic method ----
#
if (!isGeneric("sites")) {
  setGeneric(name = "sites",
             signature = signature("sos",
                                   "empty",
                                   "timeInterval",
                                   "includePhenomena",
                                   "includeTemporalBBox",
                                   "phenomena"),
             def = function(sos,
                            empty=FALSE,                 # filter
                            timeInterval=NA_character_,  # filter
                            includePhenomena=FALSE,      # meta data
                            includeTemporalBBox=FALSE,   # meta data
                            phenomena=list()) {          # filter
               standardGeneric("sites")
             })
}

#
# sites - call with sos parameter only ----
#
setMethod(f = "sites",
          signature = signature(sos = "SOS_2.0.0"),
          def = function(sos,
                         empty,
                         timeInterval,
                         includePhenomena,
                         includeTemporalBBox,
                         phenomena) {
            stopifnot(inherits(sos, "SOS_2.0.0"))
            stopifnot(is.logical(empty))
            stopifnot(is.character(timeInterval))
            stopifnot(is.logical(includePhenomena))
            stopifnot(is.logical(includeTemporalBBox))

            .phenomenaSet <- FALSE
            .timeIntervalSet <- FALSE

            # TODO reduce duplicate code @see siteListe method
            # validate input only if given
            if (.isPhenomenaSet(phenomena)) {
              phenomena <- .validateListOrDfColOfStrings(phenomena, "phenomena")
              .phenomenaSet <- TRUE
            }
            if (.isTimeIntervalSet(timeInterval)) {
              .timeInterval <- .validateISO8601String(timeInterval)
              .timeIntervalSet <- TRUE
            }

            if (includeTemporalBBox && !includePhenomena) {
              includePhenomena <- TRUE
              warning("'includePhenomena' has been set to 'TRUE' as this is required for 'includeTemporalBBox'.")
            }

            # print(paste0("empty               : ", empty))
            # print(paste0(".phenomenaSet       : ", .phenomenaSet))
            # print(paste0(".timeIntervalSet    : ", .timeIntervalSet))
            # print(paste0("includePhenomena    : ", includePhenomena))
            # print(paste0("includeTemporalBBox : ", includeTemporalBBox))

            if (empty && !.phenomenaSet && !.timeIntervalSet && !includePhenomena && !includeTemporalBBox) {
              return(.listStationsAsSPDF(sos))
            }

            if (!empty && !.phenomenaSet && !.timeIntervalSet && includePhenomena && !includeTemporalBBox) {
              return(.listStationsWithPhenomenaAsSPDF(sos))
            }
            stop("NOT YET IMPLEMENTED")
          }
)
#
# sites(sos, empty = TRUE) → sp::SPDF[siteID]
#
# See: https://github.com/52North/sos4R/issues/87
#
# Used SOS operations/requests
#  - GetFeatureOfInterest without parameter
#
.listStationsAsSPDF <- function(sos) {
  # get all stations
  .sites <- getFeatureOfInterest(sos)
  if (is.null(.sites) || !is.list(.sites) || length(.sites) < 1) {
    stop("Continue implementation here: handySOS4Rfunctions.R:442: return empty dataframe")
  }
  stop("Continue implementation here: handySOS4Rfunctions.R:444: return filled dataframe")
}
#
# sites(sos, includePhenomena=TRUE, includeTemporalBBox=FALSE) → sp::SPDF[siteID, phen_1=logical, …, phen_n=logical]
#
# See: https://github.com/52North/sos4R/issues/92
#
# Used SOS operations/requests
# - GetDataAvailability v1.0
# - GetFeatureOfInterest w/feature list
#
.listStationsWithPhenomenaAsSPDF <- function(sos) {
  # get all phenomena
  .phenomena <- as.list(.listPhenomena(sos)[, 1])
  if (is.null(.phenomena) || !is.list(.phenomena) || length(.phenomena) < 1) {
    return(.listStationsAsSPDF(sos))
  }
  # get all stations
  .sites <- getFeatureOfInterest(sos)
  if (is.null(.sites) || !is.list(.sites) || length(.sites) < 1) {
    return(.listStationsAsSPDF(sos))
  }
  # get data availability
  .dams <- getDataAvailability(sos)
  if (is.null(.dams) || is.na(.dams) || !is.list(.dams)) {
    .dams <- list()
  }
  # set-up base dataframe
  nrows <- length(.sites)
  .sitesDataFrame <- st_sf(geometry = st_sfc(lapply(1:nrows, function(x) st_geometrycollection())))
  # set values for phenomena at stations
  # for each station
  for (.site in .sites) {
    # add siteID
    .siteRow <- c(setNames(c(.site@feature@identifier, use.names = TRUE), "siteID"))
    # add phenomena
    # for each phenomenon
    for (.phenomenon in .phenomena) {
      .damFound <- FALSE
      # for each dam
      for (.dam in .dams) {
        # is phenomenon available at the station
        if (.dam@featureOfInterest == .site@feature@identifier && .dam@observedProperty == .phenomenon) {
          # .damFound := TRUE
          # if yes -> set value to TRUE
          .damFound <- TRUE
          .siteRow <- c(.siteRow, setNames(c(TRUE, use.names = TRUE), .phenomenon), use.names = TRUE)
        }
      }
      # if dam not found
      if (!.damFound) {
        # apppend false to list for phen
        .siteRow <- c(.siteRow, setNames(c(FALSE, use.names = TRUE), .phenomenon), use.names = TRUE)
      }
    }
    # add geometry
    .siteRow <- c(.siteRow, setNames(c("geometry", use.names = TRUE), as(.site@feature@shape@point), "SpatialPoints"), use.names = TRUE)
    print("bla")
    # append new row at dataframe
    #.sites <- rbind(.sites, data.frame("siteID" = .dam@featureOfInterest,
    #                                   stringsAsFactors = FALSE))
      # row := siteID, phen_1=logical, …, phen_n=logical
  }
  # set projection for .sitesDataFrame
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
