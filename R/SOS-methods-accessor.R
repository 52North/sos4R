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
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)                          #
# Created: 2011-03-03                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

############################################################################## #
# accessor functions
if (!isGeneric("sosCaps"))
  setGeneric(name = "sosCaps", def = function(sos) {
    standardGeneric("sosCaps")
  })
setMethod(f = "sosCaps", signature = signature(sos = "SOS"),
          definition = function(sos) {
            return(sos@capabilities)
          })

if (!isGeneric("sosFilter_Capabilities"))
  setGeneric(name = "sosFilter_Capabilities", def = function(sos) {
    standardGeneric("sosFilter_Capabilities")
  })
setMethod(f = "sosFilter_Capabilities", signature = signature(sos = "SOS"),
          definition = function(sos) {
            return(sos@capabilities@filterCapabilities)
          })

if (!isGeneric("sosServiceIdentification"))
  setGeneric(name = "sosServiceIdentification", def = function(sos) {
    standardGeneric("sosServiceIdentification")
  })
setMethod(f = "sosServiceIdentification", signature = signature(sos = "SOS"),
          definition = function(sos) {
            return(sos@capabilities@identification)
          })

if (!isGeneric("sosServiceProvider"))
  setGeneric(name = "sosServiceProvider", def = function(sos) {
    standardGeneric("sosServiceProvider")
  })
setMethod(f = "sosServiceProvider", signature = signature(sos = "SOS"),
          definition = function(sos) {
            return(sos@capabilities@provider)
          })

if (!isGeneric("sosOperationsMetadata"))
  setGeneric(name = "sosOperationsMetadata", def = function(sos) {
    standardGeneric("sosOperationsMetadata")
  })
setMethod(f = "sosOperationsMetadata", signature = signature(sos = "SOS"),
          definition = function(sos) {
            return(sos@capabilities@operations)
          })

if (!isGeneric("sosOperations"))
  setGeneric(name = "sosOperations", def = function(obj) {
    standardGeneric("sosOperations")
  })
setMethod(f = "sosOperations", signature = signature(obj = "SOS"),
          definition = function(obj) {
            return(sosOperations(obj@capabilities))
          })
setMethod(f = "sosOperations",
          signature = signature(obj = "SosCapabilities_1.0.0"),
          definition = function(obj) {
            if (!is.null(obj@operations))
              return(obj@operations@operations)
            return(NA_character_)
          })
setMethod(f = "sosOperations",
          signature = signature(obj = "SosCapabilities_2.0.0"),
          definition = function(obj) {
            if (!is.null(obj@operations))
              return(obj@operations@operations)
            return(NA_character_)
          })
# required to handle the first capabilities request:
setMethod(f = "sosOperations",
          signature = signature(obj = "OwsCapabilities"),
          definition = function(obj) {
            return(NULL)
          })

if (!isGeneric("sosContents"))
  setGeneric(name = "sosContents", def = function(sos) {
    standardGeneric("sosContents")
  })
setMethod(f = "sosContents", signature = signature(sos = "SOS"),
          definition = function(sos) {
            return(sosCaps(sos)@contents)
          })

if (!isGeneric("sosUrl"))
  setGeneric(name = "sosUrl", def = function(sos) {
    standardGeneric("sosUrl")
  })
setMethod(f = "sosUrl", signature = signature(sos = "SOS_1.0.0"),
          definition = function(sos) {
            return(sos@url)
          })
setMethod(f = "sosUrl", signature = signature(sos = "SOS_2.0.0"),
          definition = function(sos) {
            return(sos@url)
          })

if (!isGeneric("sosVersion"))
  setGeneric(name = "sosVersion", def = function(sos) {
    standardGeneric("sosVersion")
  })
setMethod(f = "sosVersion", signature = signature(sos = "SOS"),
          definition = function(sos) {
            return(sos@version)
          })
if (!isGeneric("sosSwitchCoordinates"))
  setGeneric(name = "sosSwitchCoordinates", def = function(sos) {
    standardGeneric("sosSwitchCoordinates")
  })
setMethod(f = "sosSwitchCoordinates", signature = signature(sos = "SOS"),
          definition = function(sos) {
            return(sos@switchCoordinates)
          })


if (!isGeneric("sosBinding"))
  setGeneric(name = "sosBinding", def = function(sos) {
    standardGeneric("sosBinding")
  })
setMethod(f = "sosBinding", signature = signature(sos = "SOS_1.0.0"),
          definition = function(sos) {
            return(sos@binding)
          })
setMethod(f = "sosBinding", signature = signature(sos = "SOS_2.0.0"),
          definition = function(sos) {
            return(sos@binding)
          })

if (!isGeneric("sosProcedures"))
  setGeneric(name = "sosProcedures", def = function(obj) {
    standardGeneric("sosProcedures")
  })
setMethod(f = "sosProcedures", signature = signature(obj = "SOS"),
          definition = function(obj) {
            offerings <- sosOfferings(obj)
            if (length(offerings) == 1 && is.na(offerings))
              return(NA_character_)

            p <- sapply(offerings, sosProcedures)
            names(p) <- names(offerings)
            return(p)
          })
setMethod(f = "sosProcedures",
          signature = signature(obj = "SosObservationOffering"),
          definition = function(obj) {
            p <- as.character(obj@procedure)
            return(p)
          })
setMethod(f = "sosProcedures",
          signature = signature(obj = "SosObservationOffering_2.0.0"),
          definition = function(obj) {
            p <- as.character(obj@procedure)
            return(p)
          })
setMethod(f = "sosProcedures",
          signature = signature(obj = "list"),
          definition = function(obj) {
            p <- sapply(obj, sosProcedures)
            return(p)
          })
setMethod(f = "sosProcedures",
          signature = signature(obj = "OmObservationCollection"),
          definition = function(obj) {
            p <- sapply(obj@members, sosProcedures)
            return(p)
          })
setMethod(f = "sosProcedures",
          signature = signature(obj = "OmObservation"),
          definition = function(obj) {
            p <- as.character(obj@procedure)
            return(p)
          })
setMethod(f = "sosProcedures",
          signature = signature(obj = "OmOM_Observation"),
          definition = function(obj) {
            p <- as.character(obj@procedure)
            return(p)
          })
setMethod(f = "sosProcedures",
          signature = signature(obj = "OmMeasurement"),
          definition = function(obj) {
            p <- as.character(obj@procedure)
            return(p)
          })
setMethod(f = "sosProcedures",
          signature = signature(obj = "DataAvailabilityMember"),
          definition = function(obj) {
            return(obj@procedure)
          })

if (!isGeneric("sosObservedProperties"))
  setGeneric(name = "sosObservedProperties", def = function(obj, ...) {
    standardGeneric("sosObservedProperties")
  })
setMethod(f = "sosObservedProperties", signature = signature(obj = "SOS"),
          definition = function(obj) {
            offerings <- sosOfferings(obj)
            if (length(offerings) == 1 && is.na(offerings))
              return(NA_character_)

            op <- sapply(offerings, sosObservedProperties)
            return(op)
          })
setMethod(f = "sosObservedProperties", signature = signature(obj = "SosObservationOffering"),
          definition = function(obj) {
            op <- obj@observedProperty
            return(op)
          })
setMethod(f = "sosObservedProperties", signature = signature(obj = "OmObservationCollection"),
          definition = function(obj) {
            op <- sapply(obj@members, sosObservedProperties)
            return(op)
          })
setMethod(f = "sosObservedProperties", signature = signature(obj = "list"),
          definition = function(obj) {
            op <- sapply(obj, sosObservedProperties)
            return(op)
          })
setMethod(f = "sosObservedProperties", signature = signature(obj = "OmObservation"),
          definition = function(obj) {
            if (is.null(obj@observedProperty))
              return(NULL)

            op <- sosObservedProperties(obj@observedProperty)
            return(op)
          })
setMethod(f = "sosObservedProperties", signature = signature(obj = "OmOM_Observation"),
          definition = function(obj) {
            if (is.null(obj@observedProperty))
              return(NULL)

            op <- sosObservedProperties(obj@observedProperty)
            return(op)
          })
setMethod(f = "sosObservedProperties", signature = signature(obj = "SwePhenomenonProperty"),
          definition = function(obj) {
            if (!is.na(obj@href)) {
              return(obj@href)
            }
            else {
              op <- sosObservedProperties(obj@phenomenon)
              return(op)
            }
          })
setMethod(f = "sosObservedProperties", signature = signature(obj = "SweCompositePhenomenon"),
          definition = function(obj) {
            op <- sapply(obj@components, sosObservedProperties)
            return(op)
          })
setMethod(f = "sosObservedProperties", signature = signature(obj = "SwePhenomenonProperty"),
          definition = function(obj) {
            return(obj@href)
          })
setMethod(f = "sosObservedProperties", signature = signature(obj = "DataAvailabilityMember"),
          definition = function(obj) {
            return(obj@observedProperty)
          })

if (!isGeneric("sosObservableProperties"))
  setGeneric(name = "sosObservableProperties", def = function(obj, ...) {
    standardGeneric("sosObservableProperties")
  })
setMethod(f = "sosObservableProperties", signature = signature(obj = "SOS"),
          definition = function(obj) {
            offerings <- sosOfferings(obj)
            if (length(offerings) == 1 && is.na(offerings))
              return(NA_character_)

            op <- sapply(offerings, sosObservableProperties)
            return(op)
          })
setMethod(f = "sosObservableProperties", signature = signature(obj = "SosObservationOffering_2.0.0"),
          definition = function(obj) {
            op <- obj@observableProperty
            return(op)
          })
setMethod(f = "sosObservableProperties", signature = signature(obj = "OmObservationCollection"),
          definition = function(obj) {
            op <- sapply(obj@members, sosObservableProperties)
            return(op)
          })
setMethod(f = "sosObservableProperties", signature = signature(obj = "list"),
          definition = function(obj) {
            op <- sapply(obj, sosObservableProperties)
            return(op)
          })
setMethod(f = "sosObservableProperties", signature = signature(obj = "OmObservation"),
          definition = function(obj) {
            if (is.null(obj@observableProperty))
              return(NULL)

            op <- sosObservableProperties(obj@observedProperty)
            return(op)
          })
setMethod(f = "sosObservableProperties", signature = signature(obj = "SwePhenomenonProperty"),
          definition = function(obj) {
            if (!is.na(obj@href)) {
              return(obj@href)
            }
            else {
              op <- sosObservableProperties(obj@phenomenon)
              return(op)
            }
          })
setMethod(f = "sosObservableProperties", signature = signature(
  obj = "SweCompositePhenomenon"),
  def = function(obj) {
    op <- sapply(obj@components, sosObservableProperties)
    return(op)
  })
setMethod(f = "sosObservableProperties", signature = signature(obj = "SwePhenomenonProperty"),
          definition = function(obj) {
            return(obj@href)
          })

if (!isGeneric("sosBoundedBy"))
  setGeneric(name = "sosBoundedBy", def = function(obj, ...) {
    standardGeneric("sosBoundedBy")
  })
setMethod(f = "sosBoundedBy",
          signature = signature(obj = "SosObservationOffering"),
          definition = function(obj, bbox = FALSE) {
            boundedBy <- .boundedBy(obj, bbox)
            return(boundedBy)
          })
setMethod(f = "sosBoundedBy",
          signature = signature(obj = "SosObservationOffering_2.0.0"),
          definition = function(obj, bbox = FALSE) {
            boundedBy <- .boundedBy(obj, bbox)
            return(boundedBy)
          })
setMethod(f = "sosBoundedBy",
          signature = signature(obj = "list"),
          definition = function(obj, bbox = FALSE) {
            bb <- lapply(obj, sosBoundedBy, bbox = bbox)
            return(bb)
          })
setMethod(f = "sosBoundedBy",
          signature = signature(obj = "OmObservationCollection"),
          definition = function(obj, bbox = FALSE) {
            boundedBy <- .boundedBy(obj, bbox)
            return(boundedBy)
          })
.boundedBy <- function(obj, bbox) {
  bb <- NA

  if (methods::.hasSlot(object = obj, name = "observedArea"))
    bounds <- obj@observedArea
  else if (methods::.hasSlot(object = obj, name = "boundedBy"))
    bounds <- obj@boundedBy
  else stop("No supported slots to extract bounds from found!")

  if (bbox) {
    lC <- strsplit(x = bounds[["lowerCorner"]], split = " ")[[1]]
    .uC <- strsplit(x = bounds[["upperCorner"]], split = " ")[[1]]

    warning <- FALSE
    if ((length(lC) < 2)) {
      min1 <- 0
      min2 <- 0
      warning <- TRUE
    }
    else {
      min1 <- as.double(lC[[1]])
      min2 <- as.double(lC[[2]])
    }
    if ((length(.uC) < 2)) {
      max1 <- 0
      max2 <- 0
      warning <- TRUE
    }
    else {
      max1 <- as.double(.uC[[1]])
      max2 <- as.double(.uC[[2]])
    }

    if (warning) warning(paste("No valid bounding box found for", sosId(obj)))

    bb <- matrix(c(min2, min1, max2, max1),
                 ncol = 2,
                 dimnames = list(c("coords.lon", "coords.lat"),
                                 c("min", "max")))
  }
  else {
    bb <- bounds
  }

  return(bb)
}

if (!isGeneric("sosOfferings"))
  setGeneric(name = "sosOfferings", def = function(obj, ...) {
    standardGeneric("sosOfferings")
  })
setMethod(f = "sosOfferings", signature = signature(obj = "SOS"),
          definition = function(obj, offeringIDs = c(), name = NA) {
            contents <- sosContents(obj)
            if (is.null(contents))
              return(NA_character_)

            offerings <- contents@observationOfferings
            if (!is.na(name)) {
              for (o in offerings) {
                currentName <- sosName(o)
                if (currentName == name)
                  return(o)
              }
            }
            if (length(offeringIDs) > 0)
              return(offerings[offeringIDs])

            return(offerings)
          })

if (!isGeneric("sosOfferingIds"))
  setGeneric(name = "sosOfferingIds", def = function(sos) {
    standardGeneric("sosOfferingIds")
  })
setMethod(f = "sosOfferingIds", signature = signature(sos = "SOS"),
          definition = function(sos) {
            offerings <- sosOfferings(sos)
            return(names(offerings))
          })

if (!isGeneric("sosFeaturesOfInterest"))
  setGeneric(name = "sosFeaturesOfInterest", def = function(obj, ...) {
    standardGeneric("sosFeaturesOfInterest")
  })
setMethod(f = "sosFeaturesOfInterest", signature = signature(obj = "SOS"),
          definition = function(obj, offerings = sosOfferingIds(obj)) {
            # via observation offering
            offs <- sosOfferings(obj)
            offs <- offs[offerings]
            wantedOfferings <- lapply(offs, slot, name = "featureOfInterest")
            return(wantedOfferings)
          })
setMethod(f = "sosFeaturesOfInterest", signature = signature(obj = "SosObservationOffering"),
          definition = function(obj) {
            return(obj@featureOfInterest)
          })
setMethod(f = "sosFeaturesOfInterest", signature = signature(obj = "OmObservation"),
          definition = function(obj) {
            .foi <- obj@featureOfInterest
            if (is.list(.foi) && length(.foi) == 1)
              return(.foi[[1]])
            return(.foi)
          })
setMethod(f = "sosFeaturesOfInterest", signature = signature(obj = "OmOM_Observation"),
          definition = function(obj) {
            .foi <- obj@featureOfInterest
            if (is.list(.foi) && length(.foi) == 1)
              return(.foi[[1]])
            return(.foi)
          })
setMethod(f = "sosFeaturesOfInterest", signature = signature(obj = "OmMeasurement"),
          definition = function(obj) {
            .foi <- obj@featureOfInterest
            if (is.list(.foi) && length(.foi) == 1)
              return(.foi[[1]])
            return(.foi)
          })
setMethod(f = "sosFeaturesOfInterest", signature = signature(obj = "OmObservationCollection"),
          definition = function(obj) {
            fois <- lapply(obj@members, sosFeaturesOfInterest)
            return(fois)
          })
setMethod(f = "sosFeaturesOfInterest", signature = signature(obj = "list"),
          definition = function(obj) {
            fois <- lapply(obj, sosFeaturesOfInterest)
            return(fois)
          })
setMethod(f = "sosFeaturesOfInterest", signature = signature(obj = "GmlFeatureCollection"),
          definition = function(obj) {
            fois <- lapply(obj@featureCollection, sosFeaturesOfInterest)
            return(fois)
          })
setMethod(f = "sosFeaturesOfInterest", signature = signature(obj = "DataAvailabilityMember"),
          definition = function(obj) {
            return(obj@featureOfInterest)
          })
setMethod(f = "sosFeaturesOfInterest", signature = signature(obj = "GmlFeatureProperty"),
          definition = function(obj) {
            if (!is.null(obj@feature)) {
              return(sosFeaturesOfInterest(obj@feature))
            }
            else {
              return(obj@href)
            }
          })
setMethod(f = "sosFeaturesOfInterest", signature = signature(obj = "SaSamplingPoint"),
          definition = function(obj) {
            return(obj)
          })
setMethod(f = "sosFeaturesOfInterest", signature = signature(obj = "SamsSamplingFeature"),
          definition = function(obj) {
            return(obj)
          })

if (!isGeneric("sosFeatureIds"))
  setGeneric(name = "sosFeatureIds", def = function(obj, ...) {
    standardGeneric("sosFeatureIds")
  })
setMethod(f = "sosFeatureIds",
          signature = signature(obj = "list"),
          definition = function(obj) {
            fois <- sapply(obj, sosFeatureIds)
            return(fois)
          })
setMethod(f = "sosFeatureIds",
          signature = signature(obj = "OmObservationCollection"),
          definition = function(obj) {
            fois <- sapply(obj@members, sosFeatureIds)
            return(fois)
          })
setMethod(f = "sosFeatureIds",
          signature = signature(obj = "OmObservation"),
          definition = function(obj) {
            fois <- sosFeatureIds(obj@featureOfInterest)
            return(fois)
          })
setMethod(f = "sosFeatureIds",
          signature = signature(obj = "OmOM_Observation"),
          definition = function(obj) {
            fois <- sosFeatureIds(obj@featureOfInterest)
            return(fois)
          })
setMethod(f = "sosFeatureIds",
          signature = signature(obj = "OmMeasurement"),
          definition = function(obj) {
            fois <- sosFeatureIds(obj@featureOfInterest)
            return(fois)
          })
setMethod(f = "sosFeatureIds",
          signature = signature(obj = "GmlFeatureCollection"),
          definition = function(obj) {
            fois <- sosFeatureIds(obj@featureMembers)
            return(fois)
          })
setMethod(f = "sosFeatureIds",
          signature = signature(obj = "GmlFeatureProperty"),
          definition = function(obj) {
            if (!is.null(obj@feature)) {
              id <- sosFeatureIds(obj@feature)
              return(id)
            }
            else {
              return(obj@href)
            }
          })
setMethod(f = "sosFeatureIds",
          signature = signature(obj = "SaSamplingPoint"),
          definition = function(obj) {
            return(obj@id)
          })
setMethod(f = "sosFeatureIds",
          signature = signature(obj = "SamsSamplingFeature"),
          definition = function(obj) {
            return(obj@identifier)
          })
setMethod(f = "sosFeatureIds",
          signature = signature(obj = "DataAvailabilityMember"),
          definition = function(obj) {
            return(obj@featureOfInterest)
          })

if (!isGeneric("sosOperation"))
  setGeneric(name = "sosOperation", def = function(sos, operationName) {
    standardGeneric("sosOperation")
  })
setMethod(f = "sosOperation",
          signature = signature(sos = "SOS", operationName = "character"),
          definition = function(sos, operationName) {
            caps <- sosCaps(sos)
            return(caps@operations@operations[[operationName]])
          })

if (!isGeneric("sosResponseFormats"))
  setGeneric(name = "sosResponseFormats", def = function(obj, ...) {
    standardGeneric("sosResponseFormats")
  })
setMethod(f = "sosResponseFormats", signature = signature(obj = "SOS"),
          definition = function(obj, unique = FALSE, removeNulls = TRUE) {
            formats <- sapply(sosOperations(obj), sosResponseFormats)
            if (unique) {
              combined <- do.call(c, formats)
              formats <- unique(combined)
            }
            if (removeNulls) {
              formats <- Filter(Negate(is.null), formats)
            }
            return(formats)
          })
setMethod(f = "sosResponseFormats",
          signature = signature(obj = "SosObservationOffering"),
          definition = function(obj) {
            return(obj@responseFormat)
          })
setMethod(f = "sosResponseFormats",
          signature = signature(obj = "SosObservationOffering_2.0.0"),
          definition = function(obj) {
            return(obj@responseFormat)
          })
setMethod(f = "sosResponseFormats",
          signature = signature(obj = "OwsOperation"),
          definition = function(obj) {
            return(obj@parameters$responseFormat)
          })

if (!isGeneric("sosOutputFormats"))
  setGeneric(name = "sosOutputFormats", def = function(obj, ...) {
    standardGeneric("sosOutputFormats")
  })
setMethod(f = "sosOutputFormats", signature = signature(obj = "SOS"),
          definition = function(obj, unique = FALSE, removeNulls = TRUE) {
            formats <- sapply(sosOperations(obj), sosOutputFormats)
            if (unique) {
              combined <- do.call(c, formats)
              formats <- unique(combined)
            }
            if (removeNulls) {
              formats <- Filter(Negate(is.null), formats)
            }
            return(formats)
          })
setMethod(f = "sosOutputFormats",
          signature = signature(obj = "OwsOperation"),
          definition = function(obj) {
            return(obj@parameters$outputFormat)
          })

if (!isGeneric("sosResponseMode"))
  setGeneric(name = "sosResponseMode", def = function(obj, ...) {
    standardGeneric("sosResponseMode")
  })
setMethod(f = "sosResponseMode", signature = signature(obj = "SOS"),
          definition = function(obj, unique = FALSE) {
            rf <- sapply(sosOperations(obj), sosResponseMode)
            if (unique) {
              combined <- do.call(c, rf)
              rf <- unique(combined)
            }
            return(rf)
          })
setMethod(f = "sosResponseMode",
          signature = signature(obj = "SosObservationOffering"),
          definition = function(obj) {
            return(obj@responseMode)
          })
setMethod(f = "sosResponseMode",
          signature = signature(obj = "OwsOperation"),
          definition = function(obj) {
            return(obj@parameters$responseMode)
          })

if (!isGeneric("sosResultModels"))
  setGeneric(name = "sosResultModels", def = function(obj, ...) {
    standardGeneric("sosResultModels")
  })
setMethod(f = "sosResultModels", signature = signature(obj = "SOS"),
          definition = function(obj, unique = FALSE) {
            #			caps <- sosCaps(obj)
            #			.getOb <- caps@operations@operations[[sosGetObservationName]]
            #			return(.getOb@parameters$resultModel)
            .rf <- sapply(sosOperations(obj), sosResultModels)
            if (unique) {
              .c <- do.call(c, .rf)
              .rf <- unique(.c)
            }
            return(.rf)
          })
setMethod(f = "sosResultModels",
          signature = signature(obj = "SosObservationOffering"),
          definition = function(obj) {
            return(obj@resultModel)
          })
setMethod(f = "sosResultModels",
          signature = signature(obj = "OwsOperation"),
          definition = function(obj) {
            return(obj@parameters$resultModel)
          })

if (!isGeneric("sosTime"))
  setGeneric(name = "sosTime", def = function(obj, ...) {
    standardGeneric("sosTime")
  })
setMethod(f = "sosTime", signature = signature(obj = "SOS"),
          definition = function(obj, convert = TRUE) {
            times <- lapply(sosOfferings(obj), sosTime, convert = convert)
            return(times)
          })
setMethod(f = "sosTime", signature = signature(obj = "SosObservationOffering"),
          definition = function(obj, convert = TRUE) {
            if (!convert)
              return(obj@time)

            .time <- obj@time
            if (is(.time, "GmlTimePeriod")) {
              return(sosTime(.time))
            }

            warning("Could not convert time to R objects.")
            return(obj@time)
          })
setMethod(f = "sosTime", signature = signature(obj = "SosObservationOffering_2.0.0"),
          definition = function(obj, convert = TRUE) {
            if (!convert)
              return(data.frame("resultTime" = obj@resultTime, "phenomenonTime" = obj@phenomenonTime))

            if (is(obj@resultTime, "GmlTimePeriod") && is(obj@phenomenonTime, "GmlTimePeriod")) {
              return(data.frame("resultTime" = sosTime(obj@resultTime), "phenomenonTime" = sosTime(obj@phenomenonTime)))
            }

            warning("Could not convert time to R objects.")
            return(data.frame("resultTime" = obj@resultTime, "phenomenonTime" = obj@phenomenonTime))
          })
setMethod(f = "sosTime", signature = signature(obj = "GmlTimePeriod"),
          definition = function(obj, ...) {
            start <- NA
            end <- NA

            if (!is.null(obj@begin) && !is.null(obj@end)) {
              start <- sosTime(obj@begin)
              end <- sosTime(obj@end)
            }

            if (!is.null(obj@beginPosition) && !is.null(obj@endPosition)) {
              start <- sosTime(obj@beginPosition)
              end <- sosTime(obj@endPosition)
            }

            period <- list(start, end)
            names(period) <- c("begin", "end")
            return(period)
          })
setMethod(f = "sosTime", signature = signature(obj = "GmlTimePosition"),
          definition = function(obj, ...) {
            time <- obj@time
            if (!is.na(obj@indeterminatePosition) && obj@indeterminatePosition == "now") {
              time <- Sys.time()
            }

            newAttrs <- list("frame" = obj@frame,
                             "calendarEraName" = obj@calendarEraName,
                             "indeterminatePosition" = obj@indeterminatePosition)
            attributes(time) <- c(attributes(time), newAttrs)

            return(time)
          })
setMethod(f = "sosTime", signature = signature(obj = "GmlTimeInstantProperty"),
          definition = function(obj, ...) {
            if (is.na(obj@href))
              return(obj@href)

            if (!is.null(obj@time))
              return(sosTime(obj@time))

            return(NA)
          })
setMethod(f = "sosTime", signature = signature(obj = "GmlTimeInstant"),
          definition = function(obj, ...) {
            return(sosTime(obj@timePosition))
          })
setMethod(f = "sosTime", signature = signature(obj = "list"),
          definition = function(obj, convert = TRUE) {
            return(lapply(X = obj, FUN = sosTime, convert = convert))
          })
setMethod(f = "sosTime", signature = signature(obj = "OmOM_Observation"),
          definition = function(obj, convert = TRUE) {
            times <- list(resultTime = sosTime(obj@resultTime),
                          phenomenonTime = sosTime(obj@phenomenonTime))
            return(times)
          })
setMethod(f = "sosTime", signature = signature(obj = "DataAvailabilityMember"),
          definition = function(obj, ...) {
            return(sosTime(obj@phenomenonTime))
          })
setMethod(f = "sosTime", signature = signature(obj = "SensorML"),
          definition = function(obj, ...) {
            if (!is.null(obj@validTime))
              return(sosTime(obj@validTime))
            else return(NULL)
          })

if (!isGeneric("sosTimeFormat"))
  setGeneric(name = "sosTimeFormat", def = function(sos) {
    standardGeneric("sosTimeFormat")
  })
setMethod(f = "sosTimeFormat", signature = signature(sos = "SOS"),
          definition = function(sos) {
            return(sos@timeFormat)
          })

if (!isGeneric("sosParsers"))
  setGeneric(name = "sosParsers", def = function(sos) {
    standardGeneric("sosParsers")
  })
setMethod(f = "sosParsers", signature = signature(sos = "SOS"),
          definition = function(sos) {
            return(sos@parsers)
          })
setMethod(f = "sosParsers", signature = signature(sos = "SOS_Test"),
          definition = function(sos) {
            return(sos@parsers)
          })

if (!isGeneric("sosResult"))
  setGeneric(name = "sosResult", def = function(obj, coordinates = FALSE,
                                                ...) {
    standardGeneric("sosResult")
  })
setMethod(f = "sosResult", signature = signature(obj = "OmObservation"),
          definition = function(obj, coordinates = FALSE) {
            if (coordinates){
              coords <- sosCoordinates(obj)
              data <- merge(x = obj@result, y = coords)
              return(data)
            }
            return(obj@result)
          })
setMethod(f = "sosResult", signature = signature(obj = "OmOM_Observation"),
          definition = function(obj, coordinates = FALSE) {
            if (coordinates){
              coords <- sosCoordinates(obj)
              if (is.data.frame(obj@result)) {
                data <- merge(x = obj@result, y = coords)
                return(data)
              } else {
                stop("Unsupported result object, cannot retrieve coordinates: ",
                     toString(obj@result))
              }
            }
            if (is.data.frame(obj@result)) {
              return(obj@result)
            } else {
              return(sosResult(obj@result))
            }
          })
setMethod(f = "sosResult", signature = signature(obj = "OmMeasurement"),
          definition = function(obj, coordinates = FALSE) {

            obsProp <- sosObservedProperties(obj)
            value <- obj@result@value
            uom <- obj@result@uom

            result <- data.frame(value)
            names(result) <- obsProp
            attributes(result) <- c(attributes(result), list("uom" = uom))

            if (coordinates){
              coords <- sosCoordinates(obj)
              data <- merge(x = result, y = coords)
              return(data)
            }
            else return(result)
          })
setMethod(f = "sosResult", signature = signature(obj = "OmObservationProperty"),
          definition = function(obj, coordinates = FALSE) {
            if (!is.na(obj@href))
              return(c(href = obj@href))
            else if (!is.null(obj@obs))
              return(sosResult(obj = obj@obs, coordinates = coordinates))
            else return(NA)
          })
setMethod(f = "sosResult", signature = signature(obj = "OmObservationCollection"),
          definition = function(obj, coordinates = FALSE, bind = TRUE) {
            l <- lapply(obj@members, sosResult, coordinates = coordinates)
            if (bind)
              result <- do.call(rbind, l)
            else result <- l
            return(result)
          })
setMethod(f = "sosResult", signature = signature(obj = "list"),
          definition = function(obj, coordinates = FALSE) {
            l <- lapply(obj, sosResult, coordinates = coordinates)
            result <- do.call(rbind, l)
            return(result)
          })
setMethod(f = "sosResult", signature = signature(obj = "OwsExceptionReport"),
          definition = function(obj, coordinates = FALSE) {
            warning("OwsExceptionReport does not have a result set.")
            return(toString(obj))
          })
setMethod(f = "sosResult", signature = signature(obj = "character"),
          definition = function(obj, coordinates = FALSE) {
            warning(paste("No processable result given: ", obj))
            return(toString(obj))
          })
# just returns the data.frame again, allows using the binding facilities of
# the sosResult(list) function
setMethod(f = "sosResult", signature = signature(obj = "data.frame"),
          definition = function(obj, coordinates = FALSE) {
            return(obj)
          })

if (!isGeneric("sosCoordinates"))
  setGeneric(name = "sosCoordinates", def = function(obj, ...) {
    standardGeneric("sosCoordinates")
  })
setMethod(f = "sosCoordinates",
          signature = signature(obj = "SosObservationOffering"),
          definition = function(obj) {
            off.spatial <- as(obj, "Spatial")
            coords <- sp::coordinates(off.spatial)
            return(coords)
          })
setMethod(f = "sosCoordinates",
          signature = signature(obj = "OmObservationCollection"),
          definition = function(obj) {
            coords <- sosCoordinates(obj = obj@members)
            return(coords)
          })
setMethod(f = "sosCoordinates", signature = signature(obj = "OmObservation"),
          definition = function(obj) {
            coords <- sosCoordinates(obj = obj@featureOfInterest)
            return(coords)
          })
setMethod(f = "sosCoordinates", signature = signature(obj = "OmObservationProperty"),
          definition = function(obj) {
            warning("No coordinates in om:ObservationProperty ", obj@href, "\n")
            coords <- as.data.frame(list(NA, NA, NA))
            names(coords) <- c(sosDefaultColumnNameLon,
                               sosDefaultColumnNameLat,
                               sosDefaultColumnNameSRS)
            return(coords)
          })
setMethod(f = "sosCoordinates", signature = signature(obj = "OmOM_Observation"),
          definition = function(obj) {
            coords <- sosCoordinates(obj = obj@featureOfInterest)
            return(coords)
          })
setMethod(f = "sosCoordinates", signature = signature(obj = "GmlFeatureCollection"),
          definition = function(obj) {
            l <- lapply(obj@featureMembers, sosCoordinates)
            coords <- do.call(rbind, l)
            return(coords)
          })
setMethod(f = "sosCoordinates",
          signature = signature(obj = "GmlFeatureProperty"),
          definition = function(obj) {
            if (!is.null(obj@feature)) {
              coords <- sosCoordinates(obj = obj@feature)
              return(coords)
            }
            else {
              warning("[sosCoordinates] Can only return coordinates if GmlFeatureProperty directly contains a feature.")
              return(NA)
            }
          })
setMethod(f = "sosCoordinates", signature = signature(obj = "SaSamplingPoint"),
          definition = function(obj) {
            coords <- sosCoordinates(obj = obj@position)
            names <- names(coords)
            coords[, ncol(coords) + 1] <- sosId(obj)
            names(coords) <- c(names, sosDefaultColumnNameFeatureIdentifier)
            return(coords)
          })
setMethod(f = "sosCoordinates", signature = signature(obj = "SamsSamplingFeature"),
          definition = function(obj) {
            coords <- sosCoordinates(obj = obj@shape)
            return(coords)
          })
setMethod(f = "sosCoordinates", signature = signature(obj = "SamsShape"),
          definition = function(obj) {
            coords <- sosCoordinates(obj = obj@point)
            return(coords)
          })
setMethod(f = "sosCoordinates", signature = signature(obj = "GmlPointProperty"),
          definition = function(obj) {
            coords <- sosCoordinates(obj = obj@point)
            return(coords)
          })
setMethod(f = "sosCoordinates", signature = signature(obj = "GmlPoint"),
          definition = function(obj) {
            coords <- sosCoordinates(obj = obj@pos)
            return(coords)
          })
setMethod(f = "sosCoordinates",
          signature = signature(obj = "GmlDirectPosition"),
          definition = function(obj) {
            coordinateDoubles <- as.double(strsplit(x = obj@pos, split = " ")[[1]])
            coords <- as.data.frame(list(coordinateDoubles[[2]],
                                         coordinateDoubles[[1]],
                                         sosSrsName(obj)))
            names(coords) <- c(sosDefaultColumnNameLon,
                               sosDefaultColumnNameLat,
                               sosDefaultColumnNameSRS)
            return(coords)
          })
setMethod(f = "sosCoordinates",
          signature = signature(obj = "list"),
          definition = function(obj, sos = NULL, verbose = FALSE) {
            if (is.null(sos))
              allCoords <- lapply(obj, sosCoordinates)
            else allCoords <- lapply(obj, sosCoordinates, sos = sos)

            coords <- do.call(rbind, allCoords)
            return(coords)
          })

if (!isGeneric("sosSrsName"))
  setGeneric(name = "sosSrsName", def = function(obj) {
    standardGeneric("sosSrsName")
  })
setMethod(f = "sosSrsName", signature = signature(obj = "SOS"),
          definition = function(obj) {
            caps <- sosCaps(obj)
            getOb <- caps@operations@operations[[sosGetObservationName]]
            return(getOb@parameters$srsName)
          })
setMethod(f = "sosSrsName",
          signature = signature(obj = "GmlDirectPosition"),
          definition = function(obj) {
            return(obj@srsName)
          })
setMethod(f = "sosSrsName",
          signature = signature(obj = "GmlPoint"),
          definition = function(obj) {
            self <- obj@srsName
            if (is.na(self)) {
              return(sosSrsName(obj@pos))
            }
            return(self)
          })

if (!isGeneric("sosId"))
  setGeneric(name = "sosId", def = function(obj) {
    standardGeneric("sosId")
  })
setMethod(f = "sosId", signature = signature(obj = "GmlFeature"),
          definition = function(obj) {
            return(obj@id)
          })
setMethod(f = "sosId", signature = signature(obj = "GmlFeatureProperty"),
          definition = function(obj) {
            if (!is.na(obj@href)) return(obj@href)
            else return(sosId(obj@feature))
          })
setMethod(f = "sosId", signature = signature(obj = "SosObservationOffering"),
          definition = function(obj) {
            return(obj@id)
          })
setMethod(f = "sosId", signature = signature(obj = "SosObservationOffering_2.0.0"),
          definition = function(obj) {
            return(obj@id)
          })
setMethod(f = "sosId", signature = signature(obj = "list"),
          definition = function(obj) {
            return(sapply(obj, sosId))
          })

if (!isGeneric("sosName"))
  setGeneric(name = "sosName", def = function(obj) {
    standardGeneric("sosName")
  })
setMethod(f = "sosName", signature = signature(obj = "list"),
          definition = function(obj) {
            sapply(obj, sosName)
          })
setMethod(f = "sosName", signature = signature(obj = "SosObservationOffering"),
          definition = function(obj) {
            return(obj@name)
          })
setMethod(f = "sosName", signature = signature(obj = "SosObservationOffering_2.0.0"),
          definition = function(obj) {
            return(obj@name)
          })
setMethod(f = "sosName", signature = signature(obj = "SosGetFeatureOfInterest_2.0.0"),
          definition = function(obj) {
            return(sosGetFeatureOfInterestName)
          })
setMethod(f = "sosName", signature = signature(obj = "OwsServiceProvider"),
          definition = function(obj) {
            return(obj@providerName)
          })
setMethod(f = "sosName", signature = signature(obj = "OwsOperation"),
          definition = function(obj) {
            return(obj@name)
          })
setMethod(f = "sosName", signature = signature(obj = "SosDescribeSensor"),
          definition = function(obj) {
            return(sosDescribeSensorName)
          })
setMethod(f = "sosName", signature = signature(obj = "SosGetObservation"),
          definition = function(obj) {
            return(sosGetObservationName)
          })
setMethod(f = "sosName", signature = signature(obj = "SosGetObservation_2.0.0"),
          definition = function(obj) {
            return(sosGetObservationName)
          })
setMethod(f = "sosName", signature = signature(obj = "SosGetObservationById"),
          definition = function(obj) {
            return(sosGetObservationByIdName)
          })
setMethod(f = "sosName", signature = signature(obj = "OwsGetCapabilities"),
          definition = function(obj) {
            return(sosGetCapabilitiesName)
          })
setMethod(f = "sosName", signature = signature(obj = "SamsSamplingFeature"),
          definition = function(obj) {
            return(obj@name)
          })
setMethod(f = "sosName", signature = signature(obj = "WmlMonitoringPoint"),
          definition = function(obj) {
            return(obj@names)
          })
setMethod(f = "sosName", signature = signature(obj = "GmlFeatureProperty"),
          definition = function(obj) {
            if (!is.null(obj@feature))
              return(sosName(obj@feature))
            else return(obj@href)
          })

if (!isGeneric("sosTitle"))
  setGeneric(name = "sosTitle", def = function(obj) {
    standardGeneric("sosTitle")
  })
setMethod(f = "sosTitle", signature = signature(obj = "SOS"),
          definition = function(obj) {
            if (!is.null(sosServiceIdentification(obj)))
              s <- sosTitle(sosServiceIdentification(obj))
            else s <- NA_character_

            return(s)
          })
setMethod(f = "sosTitle",
          signature = signature(obj = "OwsServiceIdentification"),
          definition = function(obj) {
            return(toString(obj@title))
          })

if (!isGeneric("sosAbstract"))
  setGeneric(name = "sosAbstract", def = function(obj) {
    standardGeneric("sosAbstract")
  })
setMethod(f = "sosAbstract", signature = signature(obj = "SOS"),
          definition = function(obj) {
            if (!is.null(sosServiceIdentification(obj)))
              s <- sosAbstract(sosServiceIdentification(obj))
            else s <- NA_character_

            return(s)
          })
setMethod(f = "sosAbstract",
          signature = signature(obj = "OwsServiceIdentification"),
          definition = function(obj) {
            return(toString(obj@abstract))
          })

if (!isGeneric("sosEncoders"))
  setGeneric(name = "sosEncoders", def = function(sos) {
    standardGeneric("sosEncoders")
  })
setMethod(f = "sosEncoders", signature = signature(sos = "SOS"),
          definition = function(sos) {
            return(sos@encoders)
          })
if (!isGeneric("sosDataFieldConverters"))
  setGeneric(name = "sosDataFieldConverters", def = function(sos) {
    standardGeneric("sosDataFieldConverters")
  })
setMethod(f = "sosDataFieldConverters", signature = signature(sos = "SOS"),
          definition = function(sos) {
            return(sos@dataFieldConverters)
          })
setMethod(f = "sosDataFieldConverters", signature = signature(sos = "SOS_Test"),
          definition = function(sos) {
            return(sos@dataFieldConverters)
          })

#
#
#
if (!isGeneric("sosUOM"))
  setGeneric(name = "sosUOM", def = function(obj) {
    standardGeneric("sosUOM")
  })
setMethod(f = "sosUOM",
          signature = c(obj = "list"),
          definition = function(obj) {
            crs <- sapply(X = obj, FUN = sosUOM)
            return(crs)
          }
)
setMethod(f = "sosUOM",
          signature = c(obj = "GmlMeasure"),
          definition = function(obj) {
            return(obj@uom)
          }
)
setMethod(f = "sosUOM",
          signature = c(obj = "OmMeasurement"),
          definition = function(obj) {
            return(obj@result@uom)
          }
)
setMethod(f = "sosUOM",
          signature = c(obj = "OmObservation"),
          definition = function(obj) {
            result <- sosResult(obj)
            uom <- sosUOM(result)
            return(uom)
          }
)
setMethod(f = "sosUOM",
          signature = c(obj = "OmObservationCollection"),
          definition = function(obj) {
            uom <- sosUOM(obj@members)
            return(uom)
          }
)
setMethod(f = "sosUOM",
          signature = c(obj = "WmlDefaultTVPMeasurementMetadata"),
          definition = function(obj) {
            return(obj@uom)
          }
)
setMethod(f = "sosUOM",
          signature = c(obj = "data.frame"),
          definition = function(obj) {
            names <- names(obj)

            uom <- c()
            for (x in names) {
              # get attribute for column
              u <- attributes(obj[[x]])[["unit of measurement"]]
              if (!is.null(u)) {
                names(u) <- x
                uom <- c(uom, u)
              }
            }

            return(uom)
          }
)

#
# sosGetDCP - get distributed computing point ----
#
setMethod(f = "sosGetDCP",
          signature = c(sos = "SOS", operation = "character"),
          definition = function(sos, operation, type, verbose) {
            ops <- sosOperations(sos)
            if (is.null(ops)) return(NULL)

            dcps <- ops[[operation]]@DCPs

            if (!is.na(type)) {
              filteredDcps <- list()
              for (dcp in dcps) {
                if (verbose) cat(paste0("DCP::type: '", dcp[[owsDcpHttpMethodIndex]], "'"))
                if (dcp[[owsDcpHttpMethodIndex]] == type) {
                  filteredDcps[[length(filteredDcps) + 1]] <- dcp
                }
              }
              return(filteredDcps)
            }
            else return(dcps)
          }
)
