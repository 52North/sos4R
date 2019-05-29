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
# Created: 2019-05-21                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
context("sosConvertTime: correct timezone handling")
#
# timezones are parsed correctly ----
#
test_that("timezones are parsed correctly", {
  sos <- SOS_Test(name = "test_sos_time_functions", version = sos200_version)
  convertedTime <- sosConvertTime("2000-01-02T13:00:00.000+13:00", sos = sos)
  expect_equal(parsedate::format_iso_8601(convertedTime), "2000-01-02T00:00:00+00:00")
  expect_equal(attr(convertedTime, "original_value"), "2000-01-02T13:00:00.000+13:00")
  convertedTime <- sosConvertTime("2000-01-02T10:12:34.567+13:00", sos = sos)
  expect_equal(parsedate::format_iso_8601(convertedTime), "2000-01-01T21:12:34+00:00")
  expect_equal(attr(convertedTime, "original_value"), "2000-01-02T10:12:34.567+13:00")
})
context("encoding(KVP|XML): POSIXt objects")
#
# POSIXct xml encoding
#
test_that("POSIXct is correct encoded in XML strings", {
  sos <- SOS_Test(name = "test_sos_time_functions", version = sos200_version)
  encoded <- encodeXML(parsedate::parse_iso_8601("2000-01-02T13:00:00.000+13:00"), sos = sos)
  expect_equal(encoded, "2000-01-02T00:00:00+00:00")
})

#
# POSIXct KVP encoding
#
test_that("POSIXct is correct encoded in KVP strings", {
  sos <- SOS_Test(name = "test_sos_time_functions", version = sos200_version)
  encoded <- encodeKVP(parsedate::parse_iso_8601("2000-01-02T13:00:00.000+13:00"), sos = sos)
  expect_equal(encoded, "2000-01-02T00:00:00+00:00")
})