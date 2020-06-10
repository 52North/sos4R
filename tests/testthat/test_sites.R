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
# Author: Eike Hinderk Jürrens (e.h.juerrens@52north.org)                      #
# Created: 2019-04-04                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
library(webmockr)
library(httr)
library(stringr)
webmockr::enable("httr")
webmockr::httr_mock()
context("convenience layer -> sites()")

.checkEmptySitesDataFrame <- function(sitesDataFrame) {
  expect_false(is.null(sitesDataFrame))
  expect_true(inherits(sitesDataFrame, "SpatialPointsDataFrame"))
  expect_false(is.null(sitesDataFrame@data))
  expect_length(colnames(sitesDataFrame@data), 1)
  expect_equal(colnames(sitesDataFrame@data)[[1]], "siteID", info = "correct column name")
  expect_equal(nrow(sitesDataFrame@data), 0, info = "number of unique sites")
  expect_length(sitesDataFrame@coords.nrs, 0)
}

.checkSitesDataFrameWithIdsAndCoords <- function(sitesDataFrame) {
  expect_false(is.null(sitesDataFrame))
  expect_true(inherits(sitesDataFrame, "SpatialPointsDataFrame"))
  data <- sitesDataFrame@data
  expect_length(colnames(data), 1)
  expect_equal(colnames(data)[[1]], "siteID", info = "correct column name")
  expect_equal(nrow(sitesDataFrame), 4, info = "number of unique sites")
  # check all values
  expect_equal("elv-ws2500",          data[ 1, 1])
  expect_equal("elv-ws2500-internal", data[ 2, 1])
  expect_equal("vaisala-wxt520",      data[ 3, 1])
  expect_equal("wwu-ws-kli-hsb",      data[ 4, 1])
  expect_match(sitesDataFrame@proj4string@projargs, "+proj=longlat", info = "CRS defined")
  expect_equivalent(class(sitesDataFrame@proj4string), "CRS")
  coords <- sitesDataFrame@coords
  expect_equal("lon", colnames(coords)[[1]])
  expect_equal("lat", colnames(coords)[[2]])
  expect_equal(4, nrow(coords))
  expect_true(7.652428150177   == coords[1,1])
  expect_true(51.934814453125  == coords[1,2])
  expect_true(7.65234184265137 == coords[2,1])
  expect_true(51.934814453125  == coords[2,2])
  expect_true(7.65237522125244 == coords[3,1])
  expect_true(51.9347763061523 == coords[3,2])
  expect_true(7.59587907791138 == coords[4,1])
  expect_true(51.9692611694336 == coords[4,2])
}

.checkSitesDataFrameWithIdsEmptyAndCoords <- function(sitesDataFrame) {
  expect_false(is.null(sitesDataFrame))
  expect_true(inherits(sitesDataFrame, "SpatialPointsDataFrame"))
  data <- sitesDataFrame@data
  expect_length(colnames(data), 2)
  expect_equal(colnames(data)[[1]], "siteID", info = "correct column name")
  expect_equal(colnames(data)[[2]], "empty", info = "correct column name")
  expect_equal(nrow(sitesDataFrame), 4, info = "number of unique sites")
  # check all values
  expect_equal("elv-ws2500",          data[ 1, 1])
  expect_equal("elv-ws2500-internal", data[ 2, 1])
  expect_equal("vaisala-wxt520",      data[ 3, 1])
  expect_equal("wwu-ws-kli-hsb",      data[ 4, 1])
  expect_false(data[ 1, 2])
  expect_false(data[ 2, 2])
  expect_false(data[ 3, 2])
  expect_true(data[ 4, 2])
  expect_match(sitesDataFrame@proj4string@projargs, "+proj=longlat", info = "CRS defined")
  expect_equivalent(class(sitesDataFrame@proj4string), "CRS")
  coords <- sitesDataFrame@coords
  expect_equal("lon", colnames(coords)[[1]])
  expect_equal("lat", colnames(coords)[[2]])
  expect_equal(4, nrow(coords))
  expect_true(7.652428150177   == coords[1,1])
  expect_true(51.934814453125  == coords[1,2])
  expect_true(7.65234184265137 == coords[2,1])
  expect_true(51.934814453125  == coords[2,2])
  expect_true(7.65237522125244 == coords[3,1])
  expect_true(51.9347763061523 == coords[3,2])
  expect_true(7.59587907791138 == coords[4,1])
  expect_true(51.9692611694336 == coords[4,2])
}

.checkSitesDataFrameWithIdsAndCoordsAndPhenomenaInformation <- function(sitesDataFrame) {
  expect_false(is.null(sitesDataFrame))
  expect_true(inherits(sitesDataFrame, "SpatialPointsDataFrame"))
  data <- sitesDataFrame@data
  expect_length(colnames(data), 34)
  expect_equal(colnames(data)[[1]], "siteID", info = "correct column name")
  expect_equal(nrow(sitesDataFrame), 4, info = "number of unique sites")
  # check all values
  expect_equal("elv-ws2500",          data[ 1, 1])
  expect_equal("elv-ws2500-internal", data[ 2, 1])
  expect_equal("vaisala-wxt520",      data[ 3, 1])
  expect_equal("wwu-ws-kli-hsb",      data[ 4, 1])
  # check phenomenon availability
  expect_true(data[data$siteID == "elv-ws2500","WindSpeed"])
  expect_true(data[data$siteID == "elv-ws2500","Sunshine"])
  expect_true(data[data$siteID == "elv-ws2500","Windchill"])
  expect_false(data[data$siteID == "elv-ws2500","RainfallDuration"])
  expect_false(data[data$siteID == "elv-ws2500","WindSpeedMinimum"])
  expect_true(data[data$siteID == "elv-ws2500-internal","Humidity"])
  expect_false(data[data$siteID == "elv-ws2500-internal","HailAccumulated"])
  expect_true(data[data$siteID == "wwu-ws-kli-hsb","AthmosphericPressure"])
  expect_true(data[data$siteID == "wwu-ws-kli-hsb","ShortwaveRadiation"])
  expect_false(data[data$siteID == "wwu-ws-kli-hsb","WindSpeedMinimum"])
  expect_false(data[data$siteID == "wwu-ws-kli-hsb","WindSpeedMaximum"])
  expect_match(sitesDataFrame@proj4string@projargs, "+proj=longlat", info = "CRS defined")
  expect_equivalent(class(sitesDataFrame@proj4string), "CRS")
  coords <- sitesDataFrame@coords
  expect_equal("lon", colnames(coords)[[1]])
  expect_equal("lat", colnames(coords)[[2]])
  expect_equal(4, nrow(coords))
  expect_true(7.652428150177   == coords[1,1])
  expect_true(51.934814453125  == coords[1,2])
  expect_true(7.65234184265137 == coords[2,1])
  expect_true(51.934814453125  == coords[2,2])
  expect_true(7.65237522125244 == coords[3,1])
  expect_true(51.9347763061523 == coords[3,2])
  expect_true(7.59587907791138 == coords[4,1])
  expect_true(51.9692611694336 == coords[4,2])
}


.checkSitesDataFrameWithIdsAndCoordsAndPhenomenaInformationWithEmpty <- function(sitesDataFrame) {
  expect_false(is.null(sitesDataFrame))
  expect_true(inherits(sitesDataFrame, "SpatialPointsDataFrame"))
  data <- sitesDataFrame@data
  expect_length(colnames(data), 34)
  expect_equal(colnames(data)[[1]], "siteID", info = "correct column name")
  expect_equal(nrow(sitesDataFrame), 6, info = "number of unique sites")
  # check all values
  expect_equal(data$siteID, c("elv-ws2500", "elv-ws2500-internal", "vaisala-wxt520", "wwu-ws-kli-hsb",
                              "empty-feature-1", "empty-feature-2"))
  # check phenomenon availability
  expect_true(data[data$siteID == "elv-ws2500","WindSpeed"])
  expect_true(data[data$siteID == "elv-ws2500","Sunshine"])
  expect_true(data[data$siteID == "elv-ws2500","Windchill"])
  expect_false(data[data$siteID == "elv-ws2500","RainfallDuration"])
  expect_false(data[data$siteID == "elv-ws2500","WindSpeedMinimum"])
  expect_true(data[data$siteID == "elv-ws2500-internal","Humidity"])
  expect_false(data[data$siteID == "elv-ws2500-internal","HailAccumulated"])
  expect_true(data[data$siteID == "wwu-ws-kli-hsb","AthmosphericPressure"])
  expect_true(data[data$siteID == "wwu-ws-kli-hsb","ShortwaveRadiation"])
  expect_false(data[data$siteID == "wwu-ws-kli-hsb","WindSpeedMinimum"])
  expect_false(data[data$siteID == "wwu-ws-kli-hsb","WindSpeedMaximum"])
  expect_match(sitesDataFrame@proj4string@projargs, "+proj=longlat", info = "CRS defined")
  expect_equivalent(class(sitesDataFrame@proj4string), "CRS")
  coords <- sitesDataFrame@coords
  expect_equal("lon", colnames(coords)[[1]])
  expect_equal("lat", colnames(coords)[[2]])
  expect_equal(6, nrow(coords))
  expect_true(7.652428150177   == coords[1,1])
  expect_true(51.934814453125  == coords[1,2])
  expect_true(7.65234184265137 == coords[2,1])
  expect_true(51.934814453125  == coords[2,2])
  expect_true(7.65237522125244 == coords[3,1])
  expect_true(51.9347763061523 == coords[3,2])
  expect_true(7.59587907791138 == coords[4,1])
  expect_true(51.9692611694336 == coords[4,2])
  expect_true(-7.2             == coords[6,1])
  expect_true(-52.2            == coords[6,2])
  for (j in 5:6) {
    for (i in 2:34) {
      expect_false(data[j, i])
    }
  }
}

.checkSitesDataFrameWithIdsAndCoordsAndPhenomenaInformationAndTemporalBBox <- function(sitesDataFrame) {
  expect_false(is.null(sitesDataFrame))
  expect_true(inherits(sitesDataFrame, "SpatialPointsDataFrame"))
  data <- sitesDataFrame@data
  colnames <- colnames(data)
  expect_length(colnames, 34)
  # sort order can vary accross systems, probably due to locale
  expect_setequal(colnames[colnames != "siteID"], c("AirTemperature",
                                 "AthmosphericPressure",
                                 "Dewpoint",
                                 "HailAccumulated",
                                 "HailDuration",
                                 "HailIntensity",
                                 "HailPeakIntensity",
                                 "Humidity",
                                 "InSystemTemperature",
                                 "Luminance",
                                 "RainfallAccumulated",
                                 "RainfallDuration",
                                 "RainfallIntensity",
                                 "RainfallPeakIntensity",
                                 "ShortwaveRadiation",
                                 "Sunshine",
                                 "Visibility",
                                 "WeatherCode",
                                 "WeatherCodeText",
                                 "Windchill",
                                 "WindDirection",
                                 "WindDirectionAverage",
                                 "WindDirectionMaximum",
                                 "WindDirectionMinimum",
                                 "WindDirectionText",
                                 "WindMaxGust",
                                 "WindSpeed",
                                 "WindSpeedAverage",
                                 "WindSpeedBft",
                                 "WindSpeedKmh",
                                 "WindSpeedMaximum",
                                 "WindSpeedMinimum",
                                 "WindSpeedMperSec"))
  expect_equal(nrow(sitesDataFrame), 4, info = "number of unique sites")
  # check all values#
  expect_equal("elv-ws2500",          data[ 1, 1])
  expect_equal("elv-ws2500-internal", data[ 2, 1])
  expect_equal("vaisala-wxt520",      data[ 3, 1])
  expect_equal("wwu-ws-kli-hsb",      data[ 4, 1])
  # check phenomenon availability
  expect_equal(parsedate::format_iso_8601(data[1, 2]$AirTemperature$timeBegin), "2019-03-01T00:30:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 2]$AirTemperature$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 3]$AthmosphericPressure$timeBegin), "2019-03-01T00:30:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 3]$AthmosphericPressure$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 4]$Dewpoint$timeBegin), "2019-03-01T00:30:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 4]$Dewpoint$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_true(is.na(data[1, 5]))
  expect_true(is.na(data[1, 6]))
  expect_true(is.na(data[1, 7]))
  expect_true(is.na(data[1, 8]))
  expect_equal(parsedate::format_iso_8601(data[1, 9]$Humidity$timeBegin), "2019-03-01T00:30:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 9]$Humidity$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_true(is.na(data[1, 10]))
  expect_equal(parsedate::format_iso_8601(data[1, 11]$Luminance$timeBegin), "2019-03-01T00:30:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 11]$Luminance$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 12]$RainfallAccumulated$timeBegin), "2019-03-01T00:30:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 12]$RainfallAccumulated$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_true(is.na(data[1, 13]))
  expect_true(is.na(data[1, 14]))
  expect_true(is.na(data[1, 15]))
  expect_true(is.na(data[1, 16]))
  expect_equal(parsedate::format_iso_8601(data[1, 17]$Sunshine$timeBegin), "2019-03-01T00:30:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 17]$Sunshine$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_true(is.na(data[1, 18]))
  expect_true(is.na(data[1, 19]))
  expect_true(is.na(data[1, 20]))
  #expect_equal(parsedate::format_iso_8601(data[1, 21]$Windchill$timeBegin), "2019-03-01T00:30:00+00:00")
  #expect_equal(parsedate::format_iso_8601(data[1, 21]$Windchill$timeEnd), "2019-03-28T23:45:00+00:00")
  #expect_equal(parsedate::format_iso_8601(data[1, 22]$WindDirection$timeBegin), "2019-03-01T00:30:00+00:00")
  #expect_equal(parsedate::format_iso_8601(data[1, 22]$WindDirection$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_true(is.na(data[1, 23]))
  expect_true(is.na(data[1, 24]))
  expect_true(is.na(data[1, 25]))
  expect_true(is.na(data[1, 26]))
  #expect_true(is.na(data[1, 27]))
  #expect_equal(parsedate::format_iso_8601(data[1, 28]$WindSpeed$timeBegin), "2019-03-01T00:30:00+00:00")
  #expect_equal(parsedate::format_iso_8601(data[1, 28]$WindSpeed$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_true(is.na(data[1, 29]))
  expect_true(is.na(data[1, 30]))
  expect_true(is.na(data[1, 31]))
  expect_true(is.na(data[1, 32]))
  expect_true(is.na(data[1, 33]))
  #expect_true(is.na(data[1, 34]))
  #
  expect_equal(parsedate::format_iso_8601(data[2, 2]$AirTemperature$timeBegin), "2019-03-01T00:30:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[2, 2]$AirTemperature$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_true(is.na(data[2, 3]))
  expect_true(is.na(data[2, 4]))
  expect_true(is.na(data[2, 5]))
  expect_true(is.na(data[2, 6]))
  expect_true(is.na(data[2, 7]))
  expect_true(is.na(data[2, 8]))
  expect_equal(parsedate::format_iso_8601(data[2, 9]$Humidity$timeBegin), "2019-03-01T00:30:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[2, 9]$Humidity$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_true(is.na(data[2, 10]))
  expect_true(is.na(data[2, 11]))
  expect_true(is.na(data[2, 12]))
  expect_true(is.na(data[2, 13]))
  expect_true(is.na(data[2, 14]))
  expect_true(is.na(data[2, 15]))
  expect_true(is.na(data[2, 16]))
  expect_true(is.na(data[2, 17]))
  expect_true(is.na(data[2, 18]))
  expect_true(is.na(data[2, 19]))
  expect_true(is.na(data[2, 20]))
  expect_true(is.na(data[2, 21]))
  expect_true(is.na(data[2, 22]))
  expect_true(is.na(data[2, 23]))
  expect_true(is.na(data[2, 24]))
  expect_true(is.na(data[2, 25]))
  expect_true(is.na(data[2, 26]))
  expect_true(is.na(data[2, 27]))
  expect_true(is.na(data[2, 28]))
  expect_true(is.na(data[2, 29]))
  expect_true(is.na(data[2, 30]))
  expect_true(is.na(data[2, 31]))
  expect_true(is.na(data[2, 32]))
  expect_true(is.na(data[2, 33]))
  expect_true(is.na(data[2, 34]))
  expect_match(sitesDataFrame@proj4string@projargs, "+proj=longlat", info = "CRS defined")
  expect_equivalent(class(sitesDataFrame@proj4string), "CRS")
  coords <- sitesDataFrame@coords
  expect_equal("lon", colnames(coords)[[1]])
  expect_equal("lat", colnames(coords)[[2]])
  expect_equal(4, nrow(coords))
  expect_true(7.652428150177   == coords[1,1])
  expect_true(51.934814453125  == coords[1,2])
  expect_true(7.65234184265137 == coords[2,1])
  expect_true(51.934814453125  == coords[2,2])
  expect_true(7.65237522125244 == coords[3,1])
  expect_true(51.9347763061523 == coords[3,2])
  expect_true(7.59587907791138 == coords[4,1])
  expect_true(51.9692611694336 == coords[4,2])
}
#
#
#
.checkSitesDataFrameWithIdsAndCoordsAndPhenomenaInformationAndTemporalBBoxAndEmptySites <- function(sitesDataFrame) {
  expect_false(is.null(sitesDataFrame))
  expect_true(inherits(sitesDataFrame, "SpatialPointsDataFrame"))
  data <- sitesDataFrame@data
  colnames <- colnames(data)
  expect_length(colnames, 34)
  expect_equal(nrow(sitesDataFrame), 6, info = "number of unique sites")
  # check all values
  expect_equal("elv-ws2500",          data[ 1, 1])
  expect_equal("elv-ws2500-internal", data[ 2, 1])
  expect_equal("vaisala-wxt520",      data[ 3, 1])
  expect_equal("wwu-ws-kli-hsb",      data[ 4, 1])
  expect_equal("empty-feature-1",     data[ 5, 1])
  expect_equal("empty-feature-2",     data[ 6, 1])
  # check phenomenon availability
  expect_equal(parsedate::format_iso_8601(data[1, 2]$AirTemperature$timeBegin), "2019-03-01T00:30:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 2]$AirTemperature$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 3]$AthmosphericPressure$timeBegin), "2019-03-01T00:30:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 3]$AthmosphericPressure$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 4]$Dewpoint$timeBegin), "2019-03-01T00:30:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 4]$Dewpoint$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_true(is.na(data[1, 5]))
  expect_true(is.na(data[1, 6]))
  expect_true(is.na(data[1, 7]))
  expect_true(is.na(data[1, 8]))
  expect_equal(parsedate::format_iso_8601(data[1, 9]$Humidity$timeBegin), "2019-03-01T00:30:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 9]$Humidity$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_true(is.na(data[1, 10]))
  expect_equal(parsedate::format_iso_8601(data[1, 11]$Luminance$timeBegin), "2019-03-01T00:30:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 11]$Luminance$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 12]$RainfallAccumulated$timeBegin), "2019-03-01T00:30:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 12]$RainfallAccumulated$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_true(is.na(data[1, 13]))
  expect_true(is.na(data[1, 14]))
  expect_true(is.na(data[1, 15]))
  expect_true(is.na(data[1, 16]))
  expect_equal(parsedate::format_iso_8601(data[1, 17]$Sunshine$timeBegin), "2019-03-01T00:30:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[1, 17]$Sunshine$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_true(is.na(data[1, 18]))
  expect_true(is.na(data[1, 19]))
  expect_true(is.na(data[1, 20]))
  #expect_equal(parsedate::format_iso_8601(data[1, 21]$Windchill$timeBegin), "2019-03-01T00:30:00+00:00")
  #expect_equal(parsedate::format_iso_8601(data[1, 21]$Windchill$timeEnd), "2019-03-28T23:45:00+00:00")
  #expect_equal(parsedate::format_iso_8601(data[1, 22]$WindDirection$timeBegin), "2019-03-01T00:30:00+00:00")
  #expect_equal(parsedate::format_iso_8601(data[1, 22]$WindDirection$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_true(is.na(data[1, 23]))
  expect_true(is.na(data[1, 24]))
  expect_true(is.na(data[1, 25]))
  expect_true(is.na(data[1, 26]))
  #expect_true(is.na(data[1, 27]))
  #expect_equal(parsedate::format_iso_8601(data[1, 28]$WindSpeed$timeBegin), "2019-03-01T00:30:00+00:00")
  #expect_equal(parsedate::format_iso_8601(data[1, 28]$WindSpeed$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_true(is.na(data[1, 29]))
  expect_true(is.na(data[1, 30]))
  expect_true(is.na(data[1, 31]))
  expect_true(is.na(data[1, 32]))
  expect_true(is.na(data[1, 33]))
  #expect_true(is.na(data[1, 34]))
  #
  expect_equal(parsedate::format_iso_8601(data[2, 2]$AirTemperature$timeBegin), "2019-03-01T00:30:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[2, 2]$AirTemperature$timeEnd), "2019-03-28T23:45:00+00:00")
  expect_true(is.na(data[2, 3]))
  expect_true(is.na(data[2, 4]))
  expect_true(is.na(data[2, 5]))
  expect_true(is.na(data[2, 6]))
  expect_true(is.na(data[2, 7]))
  expect_true(is.na(data[2, 8]))
  expect_equal(parsedate::format_iso_8601(data[2, 9]$Humidity$timeBegin), "2019-03-01T00:30:00+00:00")
  expect_equal(parsedate::format_iso_8601(data[2, 9]$Humidity$timeEnd), "2019-03-28T23:45:00+00:00")
  for (i in 10:34) {
    expect_true(is.na(data[2, i]))
  }
  expect_match(sitesDataFrame@proj4string@projargs, "+proj=longlat", info = "CRS defined")
  expect_equivalent(class(sitesDataFrame@proj4string), "CRS")
  coords <- sitesDataFrame@coords
  expect_equal("lon", colnames(coords)[[1]])
  expect_equal("lat", colnames(coords)[[2]])
  expect_equal(6, nrow(coords))
  expect_true(7.652428150177   == coords[1,1])
  expect_true(51.934814453125  == coords[1,2])
  expect_true(7.65234184265137 == coords[2,1])
  expect_true(51.934814453125  == coords[2,2])
  expect_true(7.65237522125244 == coords[3,1])
  expect_true(51.9347763061523 == coords[3,2])
  expect_true(7.59587907791138 == coords[4,1])
  expect_true(51.9692611694336 == coords[4,2])
  expect_true(-7.5             == coords[5,1])
  expect_true(-51.9            == coords[5,2])
  expect_true(-7.2             == coords[6,1])
  expect_true(-52.2            == coords[6,2])
  for (j in 5:6) {
    for (i in 2:34) {
      expect_true(is.na(data[j, i]))
    }
  }
}
#
# test KVP: empty SPDF when SOS empty ----
#
test_that("KVP::sites(sos, empty = TRUE|FALSE) returns an empty list of sites as SpatialPointsDataFrame if the SOS is empty", {
  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://example.com/sos-list-phenomena?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
    webmockr::wi_th(
      headers = list("Accept" = "application/xml")
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/Capabilities_200_Example.com_empty.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  webmockr::stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetFeatureOfInterest') %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetFeatureOfInterest_200_Example.com_empty.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")

  sitesDataFrame <- sites(sos, empty = TRUE)
  .checkEmptySitesDataFrame(sitesDataFrame)

  sitesDataFrame <- sites(sos)
  .checkEmptySitesDataFrame(sitesDataFrame)
})

#
# test KVP: sites(sos,empty=TRUE) ----
#
test_that("KVP::sites(sos, empty = TRUE) returns a list of stations as SpatialPointsDataFrame that contain siteIDs and coordinates", {
  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://example.com/sos-list-phenomena?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
    webmockr::wi_th(
      headers = list("Accept" = "application/xml")
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/Capabilities_200_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetFeatureOfInterest') %>%
    wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetFeatureOfInterest_200_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetDataAvailability') %>%
    wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetDataAvailability_100_Example.com_OneEmpty.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")

  sitesDataFrame <- sites(sos, empty = TRUE)
  .checkSitesDataFrameWithIdsEmptyAndCoords(sitesDataFrame)
})

#
# test KVP: sites(sos,empty=FALSE) ----
#
test_that("KVP::sites(sos, empty = FALSE) returns a list of stations as SpatialPointsDataFrame that contain siteIDs and coordinates", {
  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://example.com/sos-list-phenomena?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
    webmockr::wi_th(
      headers = list("Accept" = "application/xml")
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/Capabilities_200_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetFeatureOfInterest') %>%
    wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetFeatureOfInterest_200_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetDataAvailability') %>%
    wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetDataAvailability_100_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")
  sitesDataFrame <- sites(sos)
  .checkSitesDataFrameWithIdsAndCoords(sitesDataFrame)
})
#
# test KVP: sites(sos,includePhenomena=TRUE,...) ----
#
test_that("KVP: sites(sos,includePhenomena=TRUE,...) returns a list of stations as SpatialPointsDataFrame that contain siteIDs and coordinates", {
  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://example.com/sos-list-phenomena?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
    webmockr::wi_th(
      headers = list("Accept" = "application/xml")
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/Capabilities_200_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  stub_request('get', uri = paste0('http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetFeatureOfInterest&',
                                   'featureOfInterest=elv-ws2500%2Cwwu-ws-kli-hsb%2Celv-ws2500-internal%2Cvaisala-wxt520')) %>%
    wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetFeatureOfInterest_200_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetDataAvailability') %>%
    wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetDataAvailability_100_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")

  sitesDataFrame <- sites(sos, includePhenomena = TRUE)
  .checkSitesDataFrameWithIdsAndCoordsAndPhenomenaInformation(sitesDataFrame)
})
#
# test KVP: sites(sos,includePhenomena=TRUE,includeTemporalBbox=TRUE,...) ----
#
test_that("KVP: sites with metadata", {
  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://example.com/sos-list-phenomena?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
    webmockr::wi_th(
      headers = list("Accept" = "application/xml")
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/Capabilities_200_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  stub_request('get', uri = paste0('http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetFeatureOfInterest&',
                                   'featureOfInterest=elv-ws2500%2Cwwu-ws-kli-hsb%2Celv-ws2500-internal%2Cvaisala-wxt520')) %>%
    wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetFeatureOfInterest_200_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetDataAvailability') %>%
    wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetDataAvailability_100_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")

  sitesDataFrame <- sites(sos, includePhenomena = TRUE, includeTemporalBBox = TRUE)
  .checkSitesDataFrameWithIdsAndCoordsAndPhenomenaInformationAndTemporalBBox(sitesDataFrame)

  expect_warning(sitesDataFrame <- sites(sos, includeTemporalBBox = TRUE), "'includePhenomena' has been set to 'TRUE' as this is required for 'includeTemporalBBox'.")
  .checkSitesDataFrameWithIdsAndCoordsAndPhenomenaInformationAndTemporalBBox(sitesDataFrame)
})
#
# test KVP: sites(sos,empty=TRUE,includePhenomena=TRUE,...) ----
#
test_that("KVP::sites(sos, empty=TRUE, includePhenomena = TRUE,...)", {
  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://example.com/sos-list-phenomena?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
    webmockr::wi_th(
      headers = list("Accept" = "application/xml")
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/Capabilities_200_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetFeatureOfInterest') %>%
    wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetFeatureOfInterest_200_ForSitesWithEmpty.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetDataAvailability') %>%
    wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetDataAvailability_100_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")

  sitesDataFrame <- sites(sos, empty = TRUE, includePhenomena = TRUE)

  .checkSitesDataFrameWithIdsAndCoordsAndPhenomenaInformationWithEmpty(sitesDataFrame)
})
#
# test KVP: sites(sos,empty=TRUE,includePhenomena=TRUE,includeTemporalBbox=TRUE,...) ----
#
test_that("KVP::sites(sos, empty=TRUE, includePhenomena = TRUE, includeTemporalBbox = TRUE,...)", {
  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://example.com/sos-list-phenomena?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
    webmockr::wi_th(
      headers = list("Accept" = "application/xml")
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/Capabilities_200_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetFeatureOfInterest') %>%
    wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetFeatureOfInterest_200_ForSitesWithEmpty.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetDataAvailability') %>%
    wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetDataAvailability_100_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")
  sitesDataFrame <- sites(sos, empty = TRUE, includePhenomena = TRUE, includeTemporalBBox = TRUE)
  .checkSitesDataFrameWithIdsAndCoordsAndPhenomenaInformationAndTemporalBBoxAndEmptySites(sitesDataFrame)
})

#
# test KVP filter phenomena ----
#
test_that("KVP::sites(sos, phenomena = list of phenomenon)", {
  webmockr::stub_registry_clear()
  webmockr::stub_request("get", uri = "http://example.com/sos-list-phenomena?service=SOS&request=GetCapabilities&acceptVersions=2.0.0&sections=All&acceptFormats=text%2Fxml") %>%
    webmockr::wi_th(
      headers = list("Accept" = "application/xml")
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/Capabilities_200_Example.com.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  webmockr::stub_request('get', uri = 'http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetFeatureOfInterest') %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetFeatureOfInterest_200_ForGdaWithPhenomenaFiter.xml"),
      headers = list("Content-Type" = "application/xml")
    )
  webmockr::stub_request('get', uri = paste0('http://example.com/sos-list-phenomena?service=SOS&version=2.0.0&request=GetDataAvailability&',
                                   'observedProperty=WindDirection')) %>%
    webmockr::wi_th(
      headers = list('Accept' = 'application/xml')
    ) %>%
    webmockr::to_return(
      status = 200,
      body = readr::read_file("../responses/GetDataAvailability_100_ForPhenomenaFilter.xml"),
      headers = list("Content-Type" = "application/xml")
    )

  sos <- SOS(version = sos200_version, url = "http://example.com/sos-list-phenomena", binding = "KVP")

  sitesDataFrame <- sites(sos, phenomena = "WindDirection")

  data <- sitesDataFrame@data
  expect_length(data[,1], 2)
  expect_equal(data[1,1], "feature-1")
  expect_equal(data[2,1], "feature-2")
  coords <- sitesDataFrame@coords
  expect_equal("lon", colnames(coords)[[1]])
  expect_equal("lat", colnames(coords)[[2]])
  expect_equal(2, nrow(coords))
  expect_true(7.5   == coords[1,1])
  expect_true(51.0  == coords[1,2])
  expect_true(7.5 == coords[2,1])
  expect_true(52.0  == coords[2,2])
})

webmockr::disable("httr")
