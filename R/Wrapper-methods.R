############################################################################## #
# Copyright (C) 2019 by 52 North                                               #
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
#         - Daniel Nüst (daniel.nuest@uni-muenster.de)                         #
# Created: 2019-01-18                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

#
# phenomena ----
#

# ~ us.1.1 ----
# What phenomena are available from a SOS?
#   phenomena(sos)
# → data.frame[phenomenonId]
# GetCapabilities::Contents
#
# ~ us.1.2 ----
# What phenomena are available from a SOS and what are their temporal ranges?
#   phenomena(sos, includeTemporalBBox=TRUE)
# → data.frame[phenomena, timeBegin, timeEnd]
# GetDataAvailability v1.0
#
# ~ us.1.4 ----
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
                            includeSiteId = FALSE,
                            ...) {
               standardGeneric("phenomena")
             })
}

#
# phenomena - call with sos parameter only ----
#
setMethod(f = "phenomena",
          signature = signature(sos = "SOS_2.0.0"),
          definition = function(sos,
                         includeTemporalBBox,
                         includeSiteId,
                         ...) {
            stopifnot(inherits(sos, "SOS_2.0.0"))
            stopifnot(is.logical(includeTemporalBBox))
            stopifnot(is.logical(includeSiteId))
            if (!includeTemporalBBox && !includeSiteId) {
              return(.listPhenomena(sos, ...))
            }
            else if (includeTemporalBBox && !includeSiteId) {
              return(.listPhenomenaWithTemporalBBox(sos, ...))
            }
            else if (includeSiteId) {
              return(.listPhenomenaWithMetadata(sos, includeTemporalBBox, ...))
            }
          })
#
# phenomena(sos) → data.frame[phenomenon] using GetCapabilities::Contents
#
# see: https://github.com/52North/sos4R/issues/81
#
.listPhenomena <- function(sos, ...) {
  observableProperties <- sosObservableProperties(sos, ...)
  stopifnot(!is.null(observableProperties))
  stopifnot(is.list(observableProperties))
  if (length(unlist(observableProperties)) == 0) {
    phenomena <- data.frame("phenomenon" = character(0), stringsAsFactors = FALSE)
  } else {
    observableProperties <- unique(sort(as.vector(unlist(observableProperties))))
    phenomena <- data.frame("phenomenon" = observableProperties, stringsAsFactors = FALSE)
  }
  return(phenomena)
}

.listPhenomenaWithMetadata <- function(sos, includeTemporalBBox, ...) {
  dams <- getDataAvailability(sos, verbose = sos@verboseOutput, ...)
  stopifnot(!is.null(dams))
  stopifnot(is.list(dams))
  if (includeTemporalBBox) {
    phenomena <- data.frame("phenomenon" = character(0),
                            "siteID" = character(0),
                            "timeBegin" = double(0),
                            "timeEnd" = double(0),
                            stringsAsFactors = FALSE)
  } else {
    phenomena <- data.frame("phenomenon" = character(0),
                            "siteID" = character(0),
                            stringsAsFactors = FALSE)
  }
  if (length(dams) > 0) {
    for (dam in dams) {
      # check if phenomenon aka observed property is in data.frame and the feature, too
      observedProperty <- dam@observedProperty
      featureOfInterest <- dam@featureOfInterest
      damBegin <- dam@phenomenonTime@beginPosition@time
      damEnd <- dam@phenomenonTime@endPosition@time
      if (nrow(phenomena[which(phenomena$phenomenon == observedProperty & phenomena$siteID == featureOfInterest), ]) == 0) {
        # if not -> append at the end
        if (includeTemporalBBox) {
          phenomena <- rbind(phenomena, data.frame("phenomenon" = observedProperty,
                                                   "siteID" = featureOfInterest,
                                                   "timeBegin" = damBegin,
                                                   "timeEnd" = damEnd,
                                                   stringsAsFactors = FALSE))
        } else {
          phenomena <- rbind(phenomena, data.frame("phenomenon" = observedProperty,
                                                   "siteID" = featureOfInterest,
                                                   stringsAsFactors = FALSE))
        }
      }
      # merge time intervals
      else if (includeTemporalBBox &&
               nrow(phenomena[which(phenomena$phenomenon == observedProperty & phenomena$siteID == featureOfInterest), ]) == 1) {
        if (damBegin < phenomena[which(phenomena$phenomenon == observedProperty & phenomena$siteID == featureOfInterest), "timeBegin"]) {
          phenomena[which(phenomena$phenomenon == observedProperty & phenomena$siteID == featureOfInterest), "timeBegin"] <- damBegin
        }
        if (phenomena[which(phenomena$phenomenon == observedProperty & phenomena$siteID == featureOfInterest), "timeEnd"] < damEnd) {
          phenomena[which(phenomena$phenomenon == observedProperty & phenomena$siteID == featureOfInterest), "timeEnd"] <- damEnd
        }
      }
    }
  }
  return(phenomena[order(phenomena$phenomenon),])
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
.listPhenomenaWithTemporalBBox <- function(sos, ...) {
  dams <- getDataAvailability(sos, verbose = sos@verboseOutput, ...)
  stopifnot(!is.null(dams))
  stopifnot(is.list(dams))
  phenomena <- data.frame("phenomenon" = character(0),
                          "timeBegin" = double(0),
                          "timeEnd" = double(0),
                          stringsAsFactors = FALSE)
  if (length(dams) > 0) {
    for (dam in dams) {
      # check if phenomenon aka observed property is in data.frame
      if (dam@observedProperty %in% phenomena[, 1]) {
        # if yes -> merge times
        if (dam@phenomenonTime@beginPosition@time < phenomena[phenomena$phenomenon == dam@observedProperty, 2]) {
          phenomena[phenomena$phenomenon == dam@observedProperty, 2] <- list(dam@phenomenonTime@beginPosition@time)
        }
        if (dam@phenomenonTime@endPosition@time > phenomena[phenomena$phenomenon == dam@observedProperty, 3]) {
          phenomena[phenomena$phenomenon == dam@observedProperty, 3] <- list(dam@phenomenonTime@endPosition@time)
        }
      }
      else {
        # if not -> append at the end
        phenomena <- rbind(phenomena, data.frame("phenomenon" = dam@observedProperty,
                                         "timeBegin" = dam@phenomenonTime@beginPosition@time,
                                         "timeEnd" = dam@phenomenonTime@endPosition@time,
                                         stringsAsFactors = FALSE))
      }
    }
  }
  return(phenomena)
}

#
# siteList ----
#

#
# ~ us.2.1: List all sites (containing data) ----
#   siteList(sos)
# → data.frame[siteID]
#
# ~ us.2.2: List all sites (also not containing data) ----
#   siteList(sos, empty=TRUE)
# → data.frame[siteID]
#
# ~ us.2.3: List all sites w/wo data for a given time window ----
#   siteList(sos, begin=POSIXct, end=POSIXct)
# → data.frame[siteID]
#
# ~ us.2.4: List all sites with metadata ----
# siteList(sos, includePhenomena=T|F, includeTemporalBBox=T|F, ...)
# → data.frame[siteid, phenomenon, beginTime, endTime]
#
# ~ us.2.5: List all sites where specified phenomena have been captured ----
#   siteList(sos, phenomena=[List of phenomena])
# → data.frame[siteID, phenomenon, timeBegin, timeEnd]

.validateListOrDfColOfStrings <- function(los, argName) {
  if (is(los, "SpatialPointsDataFrame")) {
    los <- los@data
  }
  if (is.data.frame(los)) {
    stopifnot(ncol(los) > 0)
    if (ncol(los) > 1)
      warning(paste0("Using the first column of '", argName, "' as filter."))
    los <- los[,1]
  }
  if (is.character(los) && is.vector(los) && length(los) == 1) {
    stopifnot(nchar(los) > 0)
    los <- list(los)
  }
  if (is.character(los) && is.vector(los) && length(los) > 1) {
    los <- as.list(los)
  }

  stopifnot(is.list(los))

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
                                   "begin",
                                   "end",
                                   "phenomena",
                                   "includePhenomena",
                                   "includeTemporalBBox"),
             def = function(sos,
                            empty=FALSE,                 # filter
                            begin=NA,                    # filter
                            end=NA,                      # filter
                            phenomena=list(),            # filter
                            includePhenomena=FALSE,      # meta data
                            includeTemporalBBox=FALSE,   # meta data
                            ...) {
               standardGeneric("siteList")
             })
}

#
# siteList - call with sos parameter only ----
#
setMethod(f = "siteList",
          signature = signature(sos = "SOS_2.0.0"),
          definition = function(sos,
                         empty,
                         begin,
                         end,
                         phenomena,
                         includePhenomena,
                         includeTemporalBBox,
                         ...) {
            stopifnot(inherits(sos, "SOS_2.0.0"))
            stopifnot(is.logical(empty))
            stopifnot(is.logical(includePhenomena))
            stopifnot(is.logical(includeTemporalBBox))

            if (empty) {
              return(.listSites(sos, ...))
            }
            else {
              return(.listSitesWithData(sos,
                                        begin,
                                        end,
                                        phenomena,
                                        includePhenomena,
                                        includeTemporalBBox,
                                        ...))
            }
          }
)

.isTimeIntervalSet <- function(begin, end) {
  return(!is.null(begin) &&
           !is.null(end) &&
           inherits(begin, "POSIXct") &&
           inherits(end, "POSIXct"))
}

.isPhenomenaSet <- function(phenomena) {
  if (is.null(phenomena)) {
    return(FALSE)
  }
  if (all(is(phenomena, "character")) && all(nchar(phenomena) > 0)) {
    return(TRUE)
  }
  return(is(phenomena, "list") && length(phenomena) > 0)
}

#
# siteList(sos, empty=TRUE) → data.frame[siteID]
#
# see: https://github.com/52North/sos4R/issues/86
#
.listSites <- function(sos, ...) {
  features <- getFeatureOfInterest(sos, ...)
  stopifnot(!is.null(features))
  stopifnot(is.list(features))
  if (length(unlist(features)) == 0) {
    sites <- data.frame("siteID" = character(0), stringsAsFactors = FALSE)
  } else {
    sites <- data.frame("siteID" = sort(unique(sosFeatureIds(features)), na.last = NA), stringsAsFactors = FALSE)
  }
  return(sites)
}

#
# siteList(sos) → data.frame[siteID]
# siteList(sos, EMPTY = FALSE) → data.frame[siteID]
# siteList(sos, begin = POSIXct, end = POSIXct) → data.frame[siteID]
#
# see: https://github.com/52North/sos4R/issues/84
#
.listSitesWithData <- function(sos,
                               begin,
                               end,
                               phenomena,
                               includePhenomena,
                               includeTemporalBBox,
                               ...) {
  dams <- .getDataAvailabilityMember(sos, phenomena, begin, end, ...)

  if (includeTemporalBBox && !includePhenomena) {
    includePhenomena <- TRUE
    warning("'includePhenomena' has been set to 'TRUE' as this is required for 'includeTemporalBBox'.")
  }

  if (length(dams) == 0) {
    sites <- data.frame("siteID" = character(0),
                             stringsAsFactors = FALSE)
  }
  else {
    sites <- data.frame("siteID" = character(0),
                             stringsAsFactors = FALSE)
    for (dam in dams) {
      # check if siteID is already in data.frame
      if (!(dam@featureOfInterest %in% sites[, 1])) {
        # if not -> append at the end
        sites <- rbind(sites, data.frame("siteID" = dam@featureOfInterest,
                                                   stringsAsFactors = FALSE))
      }
    }
    sites <- data.frame("siteID" = sites[order(sites$siteID),], stringsAsFactors = FALSE)
  }
  return(sites)
}

.getDataAvailabilityMember <- function(sos, phenomena, begin, end, ...) {
  if (.isPhenomenaSet(phenomena)) {
    # validate input only if given
    phenomena <- .validateListOrDfColOfStrings(phenomena, "phenomena")
    dams <- getDataAvailability(sos, observedProperties = phenomena, verbose = sos@verboseOutput, ...)
  } else {
    dams <- getDataAvailability(sos, verbose = sos@verboseOutput, ...)
  }
  stopifnot(!is.null(dams))
  stopifnot(is.list(dams))

  if (.isTimeIntervalSet(begin, end)) {
    stopifnot(begin < end)
    # filter returned dams by given temporal filter
    dams <- .filterDAMsByTime(dams, begin, end)
  }
  return(dams)
}

#
# ~ us.2.3: List all sites w/wo data for a given time window ----
#   siteList(sos, begin = POSIXct, end = POSIXct) → data.frame[siteID]
#           123456789012345678901234
# ts1     : *   *   *  *     *
# ts2     : +  + +
# ts3     :     " " "
# ts4     :        = =  =  =  =
# ts5     :                ~ ~   ~
# ts6     :    ..........
# ts7     : ° °
# ts8     : #              #
# ^ we cannot identify such cases and exclude ts8
#   because of the limitations of sos and GDA operation
# interval:    ||||||||||
#
# result 1: ts1, ts2, ts3, ts4, ts6, ts8 <-- we implement this solution
# result 2: ts3, ts6
# result 3: ts6
#
# see OGC#09-001 figure 8 (http://www.opengeospatial.org/standards/swes)
#
# Limitations:
# - we cannot identify ts8 cases
# - start and end are compared with >= and <=
# - length of requested interval is smaller than measure frequency (see ts8 above)
#
# see: https://github.com/52North/sos4R/issues/88
#
# Potential performance improvement (causing more complex logic):
# Use offering metadata from capabilites to request GDA only for offerings "touching" the timeinterval
.filterDAMsByTime <- function(dams, begin, end) {
  filteredDams <- list()
  for (dam in dams) {
    # 1 Check 6 cases for each site and add if one is matching
    damBegin <- dam@phenomenonTime@beginPosition@time
    damEnd <- dam@phenomenonTime@endPosition@time
    # 1.1 before
    if (damEnd < begin) next
    # 1.2 after
    if (end < damBegin) next
    if (
      # 1.3 contains
      (damBegin < begin && end < damEnd)
      ||
      # 1.4 begins/during/equals/ends
      (begin <= damBegin && damEnd <= end)
      ||
      # 1.5 Overlaps
      (damBegin < begin && begin < damEnd && damEnd < end)
      ||
      # 1.6 OverlappedBy
      (begin < damBegin &&  damBegin < end && end < damEnd)
    ) {
      filteredDams <- c(filteredDams, dam)
    }
  }
  return(filteredDams)
}

#
# sites ----
#

# ~ us.2.1: List all sites (containing data) ----
# sites(sos)
# → SpatialPointsDataFrame[siteID] + coords
#
# ~ us.2.2: List all sites (also not containing data) ----
# sites(sos, empty=TRUE)
# → SpatialPointsDataFrame[siteID, Empty=logical] + coords
#
# ~ us.2.3: List all sites with data for a given time window ----
# sites(sos, begin=POSIXct, end=POSIXct)
# → SpatialPointsDataFrame[siteID, Empty=logical] + coords

# ~ us.2.4: List all sites with metadata ----
# sites(sos, includePhenomena=F, includeTemporalBBox=F) = sites(sos)
# → see us.2.1/us.2.2
#
# sites(sos, includePhenomena=T, includeTemporalBBox=F)
# → SpatialPointsDataFrame[siteID, phen_1=logical, …, phen_n=logical]
#
# sites(sos, includePhenomena=T, includeTemporalBBox=T)
# → SpatialPointsDataFrame[phen_1=df[beginTime, endTime], …, phen_n=df[beginTime, endTime]] + coords
#
# ~ us.2.5: List all sites where specified phenomena have been captured ----
# sites(sos, phenomena=[List of phenomena])
# → SpatialPointsDataFrame[phen_1=df[beginTime, endTime], …, phen_n=df[beginTime, endTime]] + coords

#
# sites - generic method ----
#
if (!isGeneric("sites")) {
  setGeneric(name = "sites",
             signature = signature("sos",
                                   "begin",
                                   "end",
                                   "phenomena",
                                   "empty",
                                   "includePhenomena",
                                   "includeTemporalBBox"),
             def = function(sos,
                            begin=NA,                  # filter
                            end=NA,                    # filter
                            phenomena=list(),          # filter
                            empty=FALSE,               # filter + meta data
                            includePhenomena=FALSE,    # meta data
                            includeTemporalBBox=FALSE, # meta data
                            ...) {
               standardGeneric("sites")
             })
}

#
# sites - call with sos parameter only ----
#
setMethod(f = "sites",
          signature = signature(sos = "SOS_2.0.0"),
          definition = function(sos,
                         begin,
                         end,
                         phenomena,
                         empty = FALSE,
                         includePhenomena = FALSE,
                         includeTemporalBBox = FALSE,
                         ...) {
            stopifnot(is.logical(empty))
            stopifnot(is.logical(includePhenomena))
            stopifnot(is.logical(includeTemporalBBox))

            if (!any(includePhenomena, includeTemporalBBox, empty)) {
              return(.sitesAsSPDF(sos, begin, end, phenomena, ...))
            }
            else {
              return(.sitesWithDataAsSPDF(sos,
                                          begin,
                                          end,
                                          phenomena,
                                          empty,
                                          includePhenomena,
                                          includeTemporalBBox,
                                          ...))
            }
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
.sitesAsSPDF <- function(sos, begin = NA, end = NA, phenomena = NA, ...) {
  # get all stations
  sites <- getFeatureOfInterest(sos, verbose = sos@verboseOutput, ...)
  if (is.null(sites) || is.list(sites) && length(sites) < 1) {
    return(SpatialPointsDataFrame(coords = SpatialPoints(data.frame(x = 0, y = 0))[-1,],
                                  data = data.frame("siteID" = character(0), stringsAsFactors = FALSE),
                                  match.ID = FALSE))
  }
  sitesSPDF <- .asSpatialPointsDataFrame(sites)
  return(sitesSPDF)
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
.sitesWithDataAsSPDF <- function(sos,
                                 begin = NA,
                                 end = NA,
                                 phenomena = NULL,
                                 empty = FALSE,
                                 includePhenomena = FALSE,
                                 includeTemporalBBox = FALSE,
                                 ...) {
  if (includeTemporalBBox && !includePhenomena) {
    includePhenomena <- TRUE
    warning("'includePhenomena' has been set to 'TRUE' as this is required for 'includeTemporalBBox'.")
  }
  # get all phenomena
  phenomenaOfSos <- as.list(.listPhenomena(sos)[, 1])
  if (is.null(phenomenaOfSos) || is.list(phenomenaOfSos) && length(phenomenaOfSos) == 0) {
    return(.sitesAsSPDF(sos))
  }
  # get data availability
  if (.isPhenomenaSet(phenomena)) {
    phenomena <- .validateListOrDfColOfStrings(phenomena, "phenomena")
  }
  stopifnot(all(phenomena %in% phenomenaOfSos))
  #
  dams <- .getDataAvailabilityMember(sos, phenomena, begin, end, ...)

  # get sites
  if (length(dams) > 1 && !empty) {
    sites <- getFeatureOfInterest(sos, featureOfInterest = unique(sosFeatureIds(dams)), verbose = sos@verboseOutput, ...)
  }
  else {
    sites <- getFeatureOfInterest(sos)
  }
  if (is.null(sites) || is.list(sites) && length(sites) < 1) {
    return(.sitesAsSPDF(sos))
  }
  sitesSPDF <- .asSpatialPointsDataFrame(sites)
  # extend spdf@data with information about the available phenomena
  if (includePhenomena) {
    if (.isPhenomenaSet(phenomena)) {
      phenomenaFilter <- .validateListOrDfColOfStrings(phenomena, "phenomena")
    } else {
      phenomenaFilter <- phenomenaOfSos
    }
  }
  if (includePhenomena && !includeTemporalBBox) {
    sitesSPDF@data <- .addMetadataAboutPhenomena(sitesSPDF@data, phenomenaFilter, dams)
  }
  if (includeTemporalBBox) {
    sitesSPDF@data <- .addPhenomenaTemporalBBoxes(sitesSPDF@data, phenomenaFilter, dams, empty, sites)
  }
  if (empty && !includePhenomena) {
    sitesSPDF <- .addEmptyColumn(sos, sitesSPDF, phenomena = phenomena, dams = dams)
  }
  return(sitesSPDF)
}
#
#
#
.addEmptyColumn <- function(sos, sitesSPDF, phenomena = NA, dams = list()) {
  data <- sitesSPDF@data
  # one column dataframe with empty column
  dataframeToAdd <- data.frame("empty" = logical(0), stringsAsFactors = FALSE)
  if (length(dams) < 1) {
    # all empty
  } else {
    # process dams
    for (row in 1:nrow(data)) {
      siteId <- data[row, "siteID"]
      # check if any dam is matching the "siteID"
      found <- FALSE
      for (dam in dams) {
        if (dam@featureOfInterest == siteId) {
          dataframeToAdd <- rbind(dataframeToAdd, c(FALSE))
          found <- TRUE
          break
        }
      }
      if (!found) {
        dataframeToAdd <- rbind(dataframeToAdd, c(TRUE))
      }
      colnames(dataframeToAdd) <- c("empty")
    }
    # column to sitesSPDF
    data <- cbind(data, dataframeToAdd)
    sitesSPDF@data <- data
  }
  return(sitesSPDF)
}
#
# https://github.com/52North/sos4R/issues/93
#
.addPhenomenaTemporalBBoxes <- function(dataframe, allPhenomena, dams = list(), empty = FALSE, sites = list()) {
  # store setting of stringsAsFactors to reset after this function
  tmpStringsAsFactors <- getOption("stringsAsFactors")
  options(stringsAsFactors = FALSE)
  # create initial dataframe
  newDataFrame <- data.frame(matrix(ncol = length(allPhenomena), nrow = 0), stringsAsFactors = FALSE)
  colnames(newDataFrame) <- c(allPhenomena)
  # for each row in dataframe
  for (row in 1:nrow(dataframe)) {
    # get all available phen via dams
    siteId <- dataframe[row, "siteID"]
    phenomenaAvailability <- c()
    index <- 1
    for (phenomenon in allPhenomena) {
      found <- FALSE
      for (dam in dams) {
        featureOfInterest <- dam@featureOfInterest
        observedProperty <- dam@observedProperty
        if (class(phenomenaAvailability) != "list") {
          class(phenomenaAvailability) <- "list"
        }
        if (featureOfInterest == siteId && observedProperty == phenomenon) {
          if ( !found) {
            timeBegin <- dam@phenomenonTime@beginPosition@time
            timeEnd <- dam@phenomenonTime@endPosition@time
            phenomenaAvailability[[index]] <- data.frame("timeBegin" = timeBegin,
                                                  "timeEnd" = timeEnd,
                                                  stringsAsFactors = FALSE)
            found <- TRUE
          }
          else {
            if (timeBegin < phenomenaAvailability[[index]]$timeBegin) {
              phenomenaAvailability[[index]]$timeBegin <- timeBegin
            }
            if (phenomenaAvailability[[index]]$timeEnd < timeEnd) {
              phenomenaAvailability[[index]]$timeEnd <- timeEnd
            }
          }
        }
      }
      if ( !found) {
        phenomenaAvailability[[index]] <- NA
      }
      index <- index + 1
    }
    tmpDataFrame <- data.frame(matrix(ncol = length(allPhenomena), nrow = 0), stringsAsFactors = FALSE)
    tmpDataFrame <- rbind(c(phenomenaAvailability))
    colnames(tmpDataFrame) <- c(allPhenomena)
    # create new row for new dataframe
    # utils::type.convert((newDataFrame[,2]))
    newDataFrame <- rbind.data.frame(newDataFrame, tmpDataFrame, stringsAsFactors = FALSE)
  }
  # add siteIds
  newDataFrame <- cbind("siteID" = dataframe[["siteID"]], newDataFrame)
  options(stringsAsFactors = tmpStringsAsFactors)
  return(newDataFrame)
}

#
#
#
.addMetadataAboutPhenomena <- function(dataframe, allPhenomena, dams) {
  # store setting of stringsAsFactors to reset after this function
  tmpStringsAsFactors <- getOption("stringsAsFactors")
  options(stringsAsFactors = FALSE)
  # create initial dataframe
  newDataFrame <- data.frame(matrix(ncol = length(allPhenomena), nrow = 0), stringsAsFactors = FALSE)
  colnames(newDataFrame) <- c(allPhenomena)
  # for each row in dataframe
  for (row in 1:nrow(dataframe)) {
    # get all available phen via dams
    siteId <- dataframe[row, "siteID"]
    phenomenaAvailability <- c()
    for (phenomenon in allPhenomena) {
      found <- FALSE
      for (dam in dams) {
        if (dam@featureOfInterest == siteId &&
            dam@observedProperty == phenomenon) {
          phenomenaAvailability <- c(phenomenaAvailability, TRUE)
          found <- TRUE
          break
        }
      }
      if ( !found) {
        phenomenaAvailability <- c(phenomenaAvailability, FALSE)
      }
    }
    tmpDataFrame <- data.frame(matrix(ncol = length(allPhenomena), nrow = 0), stringsAsFactors = FALSE)
    tmpDataFrame <- rbind(c(phenomenaAvailability))
    colnames(tmpDataFrame) <- c(allPhenomena)
    # create new row for new dataframe
    # utils::type.convert((newDataFrame[,2]))
    newDataFrame <- rbind.data.frame(newDataFrame, tmpDataFrame, stringsAsFactors = FALSE)
  }
  # add siteIds
  newDataFrame <- cbind("siteID" = dataframe[["siteID"]], newDataFrame)
  options(stringsAsFactors = tmpStringsAsFactors)
  return(newDataFrame)
}

#
#
#
.asSpatialPointsDataFrame <- function(sites) {
  if (!all(sapply(sites, inherits, what = "GmlFeatureProperty"))) {
    stop(paste0("Received not supported input. Required: List of GmlFeatureProperty objects.\n",
                "Received the following classes:\n",
                paste(sapply(sites, class), collapse = ", ")
    ))
  }
  coords <- data.frame()
  siteIDs <- list()
  proj4string <- NA
  for (site in sites) {
    if (!hasMethod("sosFeatureIds", class(site))) {
      stop(paste0("Input type not supported because it has no method sosFeatureIds: ", class(site)))
    }
    siteIDs <- c(siteIDs, sosFeatureIds(site))
    if (!canCoerce(site, "SpatialPoints")) {
      stop(paste0("Input type not supported because it cannot be coerced to SpatialPoints: ", class(site@feature)))
    }
    spatialPoints <- as(site@feature, "SpatialPoints")
    coords <- rbind(coords, coordinates(spatialPoints))
    if (isS4(proj4string) && !identical(proj4string, spatialPoints@proj4string)){
      print(proj4string)
      print(spatialPoints@proj4string)
      stop("Found features with different CRS, hence stopped.")
    }
    proj4string <- spatialPoints@proj4string
  }
  return(SpatialPointsDataFrame(coords = coords,
                                data = data.frame("siteID" = unlist(siteIDs), stringsAsFactors = FALSE),
                                match.ID = FALSE,
                                proj4string = proj4string))
}

#
# getData ----
#

# ~ us.3.1: Retrieve sensor values by phenomenon/a and single site/list of sites ----
# getData(sos, phenomena=[List of phenomena], sites=[List of sites])
# →
# data.frame[siteID, timestamp, phen_1, phen_2, …]
#
# ~ us.3.2: Retrieve sensor values by phenomenon/a and spatial bounding box (in the CRS of the SOS) ----
# getData(sos, phenomena=[List of phenomena], spatialBBox)
# →
# data.frame[siteID, timestamp, phen_1, phen_2, …]
#
# ~ us.3.3: Temporal Filter for us.3.1 and us.3.2 ----
# getData(sos, …, begin=POSIXct, end=POSIXct)
# →
# data.frame[siteID, timestamp, phen_1, phen_2, …]
getData <- function(sos,
                    phenomena,
                    sites,
                    spatialBBox = NA,
                    begin = NA,
                    end = NA,
                    ...) {
  stopifnot(inherits(sos, "SOS_2.0.0"))
  stopifnot(!is.null(phenomena), length(phenomena) > 0, !any(is.na(phenomena)))
  stopifnot(!is.null(sites), length(sites) > 0)

  if (missing(sites) && is.na(spatialBBox))
    stop("Either 'sites' or 'spatialBBox' must be provided.")

  if (!missing(sites)) {
    sites <- .validateListOrDfColOfStrings(sites, "sites")
    if (!is.na(spatialBBox)) {
      spatialBBox <- NA
      warning("'spatialBBox' has been ignored and 'sites' is used instead.")
    }
  }
  if (!missing(phenomena)) {
    phenomena <- .validateListOrDfColOfStrings(phenomena, "phenomena")
  }

  time <- list()
  if (!is.na(begin) && !is.na(end))
    time <- list(sosCreateTimePeriod(sos = sos,
                                begin = begin,
                                end = end))
  observations <- getObservation(sos = sos,
                                 offering = list(),            # "all"
                                 observedProperty = phenomena, # phenomena
                                 featureOfInterest = sites,    # sites
                                 eventTime = time,             #
                                 BBOX = NA_character_,
                                 ... = ...)

  if (length(observations) < 1)
    return(data.frame())
  #
  # convert WmlMeasurementTimeseries in each observation to data.frame
  #
  if (any(sapply(sapply(observations, slot, "result"), is, "WmlMeasurementTimeseries"))) {
    for (i in 1:length(observations)) {
      observation <- observations[[i]]
      if (is(observation@result, "WmlMeasurementTimeseries")) {
        observation@result <- as(observation@result, "data.frame")
        stopifnot(is.null(observation@observedProperty@phenomenon),
                  !is.null(observation@observedProperty@href),
                  nchar(observation@observedProperty@href) > 0)
        colnames(observation@result)[2] <- observation@observedProperty@href
        observations[[i]] <- observation
      }
    }
  }
  # TODO document that we use the phentime or phentime.end
  if (ncol(observations[[1]]@result) == 1) {
    observations[[1]]@result <- cbind("timestamp" = .sosTimeStamp(observations[[1]]), observations[[1]]@result)
  }
  obsAttributes <- attributes(observations[[1]]@result)
  obsAttributes <- obsAttributes[names(obsAttributes) %in% c("names", "row.names", "class") == FALSE]
  result <- cbind("siteID" = sosFeatureIds(observations[[1]]), observations[[1]]@result)
  attributes(result[[3]]) <- append(attributes(result[[3]]), obsAttributes)
  if (length(observations) == 1) {
    names(result)[[2]] <- "timestamp"
    return(result)
  }
  #
  # this loops starts with the SECOND element
  #
  for (observation in observations[-1]) {
    if (ncol(observation@result) == 1) {
      observation@result <- cbind("timestamp" = .sosTimeStamp(observation), observation@result)
    }
    obsAttributes <- attributes(observation@result)
    obsAttributes <- obsAttributes[names(obsAttributes) %in% c("names", "row.names", "class") == FALSE]
    newColumn <- cbind("siteID" = sosFeatureIds(observation), observation@result)
    attributes(newColumn[[3]]) <- append(attributes(newColumn[[3]]), obsAttributes)
    if (!sos@verboseOutput) {
     suppressMessages({
       suppressWarnings({
         result <- dplyr::full_join(result, newColumn)
       })
     })
    } else {
      result <- dplyr::full_join(result, newColumn)
    }
  }
  #
  # set timestamp column name correct
  #
  names(result)[[2]] <- "timestamp"
  #
  # in the end, we want siteIDs as factors (during the join, it produces error messages)
  #
  result[["siteID"]] <- as.factor(result[["siteID"]])
  return(result)
}

.sosTimeStamp <- function(observation = NULL) {
  stopifnot(!is.null(observation))
  if (inherits(observation@phenomenonTime, "GmlTimeInstant")) {
    timestamp <- observation@phenomenonTime@timePosition@time
  } else {
    timestamp <- observation@phenomenonTime@endPosition@time
  }
  return(timestamp)
}
