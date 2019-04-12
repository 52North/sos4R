################################################################################
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
# Created: 2010-09-21                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
################################################################################

#
# conversion methods ----
#
sosConvertTime <- function(x, sos) {
  .t <- as.POSIXct(x = strptime(x = x, format = sosTimeFormat(sos = sos)))
  return(.t)
}

sosConvertDouble <- function(x, sos) {
  return(as.double(x = x))
}

sosConvertString <- function(x, sos) {
  return(as.character(x = x))
}

sosConvertLogical <- function(x, sos) {
  return(as.logical(x = x))
}


#
# convenience functions time class creation ----
#
setMethod(f = "sosCreateTimeInstant",
          signature = signature(sos = "SOS", time = "POSIXt"),
          definition = function(sos, time, frame, calendarEraName,
                         indeterminatePosition) {
            #			.time <- format(time, sosTimeFormat(sos))
            .timePos <- GmlTimePosition(
              #					time = strptime(.time, sosTimeFormat(sos)),
              time = time,
              frame = frame, calendarEraName = calendarEraName,
              indeterminatePosition = indeterminatePosition)
            .ti <- GmlTimeInstant(timePosition = .timePos)
            return(.ti)
          }
)

setMethod(f = "sosCreateTimePeriod",
          signature = signature(sos = "SOS", begin = "POSIXt", end = "POSIXt"),
          definition = function(sos, begin, end, frame, calendarEraName,
                         indeterminatePosition, duration, timeInterval) {
            .beginPos <- GmlTimePosition(
              time = begin,
              frame = frame, calendarEraName = calendarEraName,
              indeterminatePosition = indeterminatePosition
            )
            .endPos <- GmlTimePosition(
              time = end,
              frame = frame, calendarEraName = calendarEraName,
              indeterminatePosition = indeterminatePosition
            )
            .tp <- GmlTimePeriod(beginPosition = .beginPos,
                                 endPosition = .endPos,
                                  duration = duration,
                                 timeInterval = timeInterval)
            return(.tp)
          }
)

setMethod(f = "sosCreateEventTimeList",
          signature = signature(time = "GmlTimeGeometricPrimitive"),
          definition = function(time, operator) {
            .et <- list(sosCreateEventTime(time = time, operator = operator))
            return(.et)
          }
)

setMethod(f = "sosCreateTime",
          signature = signature(sos = "SOS", time = "character"),
          definition = function(sos, time, operator) {
            .l <- NULL
            if (regexpr(pattern = "::", text = time) > -1) {
              .l <- .sosCreateEventTimeListFromPeriod(sos = sos, time = time,
                                                      operator = operator, seperator = "::")
            }
            else if (regexpr(pattern = "P", text = time) > -1) {
              .l <- .sosCreateEventTimeListFromISOPeriod(sos = sos,
                                                         time = time, operator = operator)
            }
            else if (regexpr(pattern = "/", text = time) > -1) {
              .l <- .sosCreateEventTimeListFromPeriod(sos = sos, time = time,
                                                      operator = operator, seperator = "/")
            }
            else {
              # not a period
              .l <- .sosCreateEventTimeListFromInstance(sos = sos, time = time,
                                                        operator = operator)
            }

            if (is.null(.l)) stop("[sosCreateTime] could not create time.")

            return(.l)
          }
)

#
# test: encodeXML(.sosCreateEventTimeListFromInstance(sos = sos, time = "2011-01-01", operator = SosSupportedTemporalOperators()[["TM_Equals"]])[[1]], sos = sos)
#
.sosCreateEventTimeListFromInstance <- function(sos, time,
                                                operator = SosSupportedTemporalOperators()[["TM_Equals"]]) {
  .ti <- sosCreateTimeInstant(sos = sos, time = as.POSIXct(time))
  .l <- sosCreateEventTimeList(time = .ti,
                               operator = SosSupportedTemporalOperators()[[operator]])

  return(.l)
}

.sosCreateEventTimeListFromPeriod <- function(sos, time, operator, seperator) {
  .times <- strsplit(x = time, split = seperator)[[1]]
  .start <- .times[[1]]
  if (length(.times) > 1)
    .end <- .times[[2]]
  else .end <- NULL

  if (is.null(.start) && is.null(.end)) {
    warning("Both start and endtime are null based on given time. Returning empty list!")
    return(list())
  }
  else {
    if (is.null(.end)) {
      # no end time:
      .ti <- sosCreateTimeInstant(sos = sos, time = as.POSIXct(.start))
      .l <- sosCreateEventTimeList(time = .ti,
                                   operator = SosSupportedTemporalOperators()[[ogcTempOpTMAfterName]])
    }
    else if (nchar(.start) > 0) {
      .tp <- sosCreateTimePeriod(sos = sos, begin = as.POSIXct(.start),
                                 end = as.POSIXct(.end))
      .l <- sosCreateEventTimeList(.tp)
    }
    else if (nchar(.start) < 1) {
      # no start time:
      .ti <- sosCreateTimeInstant(sos = sos, time = as.POSIXct(.end))
      .l <- sosCreateEventTimeList(time = .ti,
                                   operator = SosSupportedTemporalOperators()[[ogcTempOpTMBeforeName]])
    }
  }
  return(.l)
}

.sosCreateEventTimeListFromISOPeriod <- function(sos, time, operator) {
  #	* 2005-08-09T18:31:42P3Y6M4DT12H30M17S: bestimmt eine Zeitspanne von 3 Jahren, 6 Monaten, 4 Tagen 12 Stunden, 30 Minuten und 17 Sekunden ab dem 9. August 2005 "kurz nach halb sieben Abends".
  #	* P1D: "Bis morgen zur jetzigen Uhrzeit." Es koennte auch "PT24H" verwendet werden, doch erstens waeren es zwei Zeichen mehr, und zweitens wuerde es bei der Zeitumstellung nicht mehr zutreffen.
  #	* P0003-06-04T12:30:17
  #	* P3Y6M4DT12H30M17S: gleichbedeutend mit dem ersten Beispiel, allerdings ohne ein bestimmtes Startdatum zu definieren
  #	* PT72H: "Bis in 72 Stunden ab jetzt."
  #	* 2005-08-09P14W: "Die 14 Wochen nach dem 9. August 2005."
  #	* 2005-08-09/2005-08-30
  #	* 2005-08-09--30
  #	* 2005-08-09/30: "Vom 9. bis 30. August 2005."

  warning("Function .sosCreateEventTimeListFromISOPeriod not implemented yet!")
}

setMethod(f = "sosCreateEventTime",
          signature = signature(time = "GmlTimeGeometricPrimitive"),
          definition = function(time, operator) {

            if (operator == ogcTempOpTMAfterName) {
              .tOps <- TM_After(time = time)
            }
            else if (operator == ogcTempOpTMBeforeName) {
              .tOps <- TM_Before(time = time)
            }
            else if (operator == ogcTempOpTMDuringName) {
              .tOps <- TM_During(time = time)
            }
            else if (operator == ogcTempOpTMEqualsName) {
              .tOps <- TM_Equals(time = time)
            }
            else {
              stop(paste("Given operator", operator, "is not supported,",
                         "choose one of",
                         toString(SosSupportedTemporalOperators())))
            }

            .et <- SosEventTime(.tOps)
            return(.et)
          }
)

#
# convenience function FOI ----
#
setMethod(f = "sosCreateFeatureOfInterest",
          signature = signature(),
          definition = function(objectIDs, spatialOps, bbox, srsName) {
            # switch cases, either objectIDs or one of the spatialOps shortcuts
            if (!any(is.na(objectIDs))) {
              .foi <- SosFeatureOfInterest(objectIDs = objectIDs)
            }
            else if (!is.null(spatialOps)) {
              .foi <- SosFeatureOfInterest(spatialOps = spatialOps)
            }
            else if (!is.null(bbox)) {
              if (is.matrix(bbox)) {
                .env <- GmlEnvelope(
                  lowerCorner = GmlDirectPositionLatLon(lat = bbox[2,1],
                                                        lon = bbox[1,1]),
                  upperCorner = GmlDirectPositionLatLon(lat = bbox[2,2],
                                                        lon = bbox[1,2]),
                  srsName = srsName)
                .bbox <- OgcBBOX(envelope = .env)
                .foi <- SosFeatureOfInterest(spatialOps = .bbox)
              }
              else {
                stop("bbox must be matrix!")
              }
            }
            else {
              stop("At least one of objectIDs or spatialOps has to be set!")
            }

            return(.foi)
          }
)

#
# convenience functions BBOX ----
#
setMethod(f = "sosCreateBBOX",
          signature = signature(lowLat = "numeric", lowLon = "numeric",
                                uppLat = "numeric", uppLon = "numeric"),
          definition = function(lowLat, lowLon, uppLat, uppLon, srsName,
                                srsDimension = NA_integer_, axisLabels = NA_character_,
                                uomLabels = NA_character_,
                                propertyName = sosDefaultSpatialOpPropertyName) {
            .env <- GmlEnvelope(
              lowerCorner = GmlDirectPosition(
                pos = paste(lowLat, lowLon, sep = " ")),
              upperCorner = GmlDirectPosition(
                pos = paste(uppLat, uppLon, sep = " ")),
              srsName = srsName, srsDimension = srsDimension,
              axisLabels = axisLabels, uomLabels = uomLabels)

            .bbox <- OgcBBOX(propertyName = propertyName, envelope = .env)
            return(.bbox)
          }
)

setMethod(f = "sosCreateBBoxMatrix",
          signature = signature(lowLat = "numeric", lowLon = "numeric",
                                uppLat = "numeric", uppLon = "numeric"),
          definition = function(lowLat, lowLon, uppLat, uppLon) {
            .m <- matrix(data = c(lowLon, lowLat, uppLon, uppLat),
                         nrow = 2, ncol = 2,
                         dimnames = list(
                           c("longitude", "latitude"),
                           c("lowerCorner", "upperCorner")))
            return(.m)
          }
)

#
# helpers capabilities ----
#
setMethod(f = "sosCapabilitiesDocumentOriginal",
          signature = signature(sos = "SOS"),
          definition = function(sos, verbose = FALSE) {
            .verbose <- sos@verboseOutput || verbose
            .gc <- OwsGetCapabilities(service = sosService,
                                      acceptVersions = c(sos@version))
            .response = sosRequest(sos = sos,
                                   request = .gc,
                                   verbose = .verbose,
                                   inspect = FALSE)
            return(.response)
          }
)

setMethod(f = "sosCapabilitiesUrl",
          signature = signature(sos = "SOS"),
          definition = function(sos) {
            .gc <- OwsGetCapabilities(service = sosService,
                                      acceptVersions = c(sos@version))
            .request <- paste0(sosUrl(sos), "?", encodeRequestKVP(.gc, sos))
            return(.request)
          }
)

#
# helpers for exception response handling ----
#
.isExceptionReport <- function(obj) {
  if (inherits(obj, "xml_document")) {
    name <- xml2::xml_name(x = xml2::xml_root(x = obj)) # intentionally without namespaces
    return(owsExceptionReportNameOnly == name)
  }
  else if (is.character(obj) && startsWith(obj, "<?xml")) {
    return(grepl(obj, "ExceptionReport"))
  }
  else if (is.list(obj)) {
    return(!is.null(obj[["exceptions"]]))
  }
  stop("Unsupported input for function .isExceptionReport")
}

.handleExceptionReport <- function(sos, obj) {
  if (sos@verboseOutput) warning("Received ExceptionReport!")
  .parsingFunction <- sosParsers(sos)[[owsExceptionReportName]]
  .er <- .parsingFunction(obj)
  if (any(class(.er) == "OwsExceptionReport"))
    warning(toString(.er))
  return(.er)
}

setMethod(f = "sosExceptionCodeMeaning",
          signature = c(exceptionCode = "character"),
          definition = function(exceptionCode) {
            .meaning <- as.character(
              .owsStandardExceptions[
                .owsStandardExceptions$exceptionCode==exceptionCode,
                2])
            return(.meaning)
          }
)

#
# encoding functions ----
#
setMethod(f = "encodeXML", signature = signature(obj = "xml_node", sos = "SOS"),
          definition = function(obj, sos, verbose = FALSE) {
            if (verbose) cat("[encodeXML] from xml_node: just returning it.\n")
            return(obj)
          }
)

setMethod(f = "encodeXML", signature = signature(obj = "xml_document", sos = "SOS"),
          definition = function(obj, sos, verbose = FALSE) {
            if (verbose) cat("[encodeXML] from xml_document: just returning it.\n")
            return(obj)
          }
)

setMethod(f = "encodeXML", signature = signature(obj = "character", sos = "SOS"),
          definition = function(obj, sos, verbose = FALSE) {
            if (verbose) cat("[encodeXML] from character string\n")
            .xml <- xml2::read_xml(x = obj, options = SosDefaultParsingOptions())
            if (verbose) cat("[encodeXML] Created XML from string:\n", toString(.xml))
            return(.xml)
          }
)

#
# convenience function CRS ----
#
setMethod(f = "sosGetCRS",
          signature = c(obj = "character"),
          definition = function(obj, verbose = FALSE) {
            if (verbose) cat("[sosGetCRS] from '", obj, "'\n", sep = "")

            .epsg <- NA
            # URN
            if (grepl(pattern = "urn:ogc", x = obj)) {
              .epsg <- sub(pattern = "(.*)epsg:[0-9]*(:?)",
                           replacement = "",
                           x = tolower(obj))[[1]]
            }
            # URL
            if (grepl(pattern = "opengis.net", x = obj)) {
              .epsg <- sub(pattern = "(.*)epsg/[0-9]*(/?)",
                           replacement = "",
                           x = tolower(obj))[[1]]
            }

            if (is.na(.epsg)) {
              warning("Could not create CRS from string ", obj)
              return(NA)
            }

            .initString <- paste("+init=epsg", .epsg, sep = ":")

            if (verbose) cat("[sosGetCRS] .initString:", .initString, "\n")

            .rgdal <- requireNamespace("rgdal", quietly = TRUE)
            if (!.rgdal)
              # if (!("rgdal" %in% .packages())) does only check loaded pkgs
              warning("[sosGetCRS] rgdal not present: CRS values will not be validated.",
                      immediate. = TRUE)
            else
              if (verbose) cat("[sosGetCRS] rgdal loaded! \n")

            .crs <- NULL
            tryCatch({
              .crs <- CRS(.initString)
            }, error = function(err) {
              warning("[sosGetCRS] error was detected, probably the ",
                      "EPSG code ", .epsg, " is not recognized ",
                      "(returning NULL):", toString(err))
            })


            if (verbose) {
              cat("[sosGetCRS] found: ")
              show(.crs)
            }

            return(.crs)
          }
)
setMethod(f = "sosGetCRS",
          signature = c(obj = "OmObservationCollection"),
          definition = function(obj, verbose = FALSE) {
            .l <- lapply(X = obj, FUN = sosGetCRS, verbose = verbose)
            .l <- unique(.l)

            if (length(.l) == 1)
              return(.l[[1]])
            else return(.l)
          }
)
setMethod(f = "sosGetCRS",
          signature = c(obj = "OmObservation"),
          definition = function(obj, verbose = FALSE) {
            .crs <- .getCRSfromOM(obj)
            return(.crs)
          }
)
setMethod(f = "sosGetCRS",
          signature = c(obj = "OmMeasurement"),
          definition = function(obj, verbose = FALSE) {
            .crs <- .getCRSfromOM(obj)
            return(.crs)
          }
)
setMethod(f = "sosGetCRS",
          signature = c(obj = "SosObservationOffering"),
          definition = function(obj, verbose = FALSE) {
            .srsName <- sosBoundedBy(obj)[["srsName"]]
            if (is.null(.srsName))
              .crs <- NULL
            else .crs <- sosGetCRS(.srsName, verbose = verbose)
            return(.crs)
          }
)
setMethod(f = "sosGetCRS",
          signature = c(obj = "SOS"),
          definition = function(obj, verbose = FALSE) {
            .offs <- sosOfferings(obj)
            .crss <- lapply(.offs, sosGetCRS, verbose = verbose)
            if (length(.crss) == 1)
              return(.crss[[1]])
            return(.crss)
          }
)
setMethod(f = "sosGetCRS",
          signature = c(obj = "list"),
          definition = function(obj, verbose = FALSE) {
            .crs <- lapply(X = obj, FUN = sosGetCRS, verbose = verbose)
            return(.crs)
          }
)

.getCRSfromOM <- function(obj) {
  .char <- as.vector(sosCoordinates(obj)[[sosDefaultColumnNameSRS]])
  .l <- sapply(X = .char, FUN = sosGetCRS)
  .l <- unique(.l)

  if (length(.l) == 1)
    return(.l[[1]])
  else return(.l)
}


#
# internal utils ----
#
.cleanupFileName <- function(obj) {
  # cleans up ", *, :, /, <, >, ?, \, and |
  .clean <- gsub(
    pattern = "[\\/:\"|?<>*,]+",
    x = obj,
    replacement = "_")
  return(.clean)
}

.illegalColumnNameCharacters <- list("\\[", "\\]", "@", "\\$", "~",
                                     "\\+", "-", "\\*")
.illegalColumnNameEscapeCharacter <- "."

.cleanupColumnName <- function(name) {
  # replace illegal characters
  .name <- name

  for (.x in .illegalColumnNameCharacters) {
    # replace multiple escape characters with one
    .name <- gsub(pattern = .x,
                  replacement = .illegalColumnNameEscapeCharacter,
                  x = .name)
  }

  .name <- gsub(pattern = paste("(\\",
                                .illegalColumnNameEscapeCharacter, ")+", sep = ""),
                replacement = .illegalColumnNameEscapeCharacter, x = .name)
  return(.name)
}

.sosFilterDCPs <- function(dcp, pattern, verbose = FALSE) {

  if (length(pattern) == 0) {
    if (verbose)
      cat("[.sosFilterDCPs] Pattern is empty (for this binding), returning DCPs unchanged.\n")
    return(dcp)
  }

  if (verbose)
    cat("[.sosFilterDCPs] Applying pattern", toString(pattern), "to",
        toString(dcp), "\n")

  .idx <- grep(pattern = pattern, x = dcp)
  .filtered <- dcp[.idx]
  if (verbose)
    cat("[.sosFilterDCPs] Filtered from\n\t", toString(dcp), "\n\tto\n\t",
        toString(.filtered), "\n")

  return(.filtered)
}

.encodeAdditionalKVPs <- function(kvps) {
  .kvpsString <- ""
  for (i in seq(1:length(kvps))) {
    .kvp <- paste(names(kvps)[[i]], kvps[[i]], sep = "=")
    .kvpsString <- paste(.kvpsString, .kvp, sep = "&")
  }
  # remove starting &
  .kvpsString <- substring(.kvpsString, 2, nchar(.kvpsString))

  return(.kvpsString)
}


#
# cheat sheet ----
#
sosCheatSheet <- function() {
  .path <- file.path(find.package("sos4R", lib.loc = NULL),
                     .sosCheatSheetDocumentName)

  # see code of 'vignette' function
  .z <- list(file = .sosCheatSheetDocumentName, PDF = .path)
  .z$topic <- "sos4R Cheat Sheet"
  class(.z) <- "vignette"

  return(.z)
}
