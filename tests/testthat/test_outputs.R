############################################################################## #
# Copyright (C) 2019 by 52°North                                               #
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
# Created: 2019-05-16                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #

context("output: verbose and inspect")

test_that("verbose output works (tested with KVP 1.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service", binding = "KVP")
  off.1 <- sosOfferings(mySOS)[[1]]
  output <- capture_output(obs.1 <- getObservation(sos = mySOS,
                                                   offering = off.1,
                                                   procedure = sosProcedures(off.1)[[1]],
                                                   observedProperty = sosObservedProperties(off.1)[1],
                                                   eventTime = sosCreateTime(sos = mySOS,
                                                                             time = "2017-12-19::2017-12-20"),
                                                   responseFormat = "text/xml; subtype=\"om/1.0.0\"",
                                                   verbose = TRUE)
  )
  expect_s4_class(obs.1, "OmObservationCollection")
  expect_match(output, "Requesting offering ws2500")
  expect_match(output, "New attributes list is AirTemperature, AirTemperature, degC")
  expect_match(output, "Finished getObservation to http://sensorweb.demo")
})

test_that("inspect output works (tested with KVP 1.0.0)", {
  skip_on_cran()

  mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service", binding = "KVP")
  off.1 <- sosOfferings(mySOS)[[1]]
  output <- capture_output(obs.1 <- getObservation(sos = mySOS,
                                                   offering = off.1,
                                                   procedure = sosProcedures(off.1)[[1]],
                                                   observedProperty = sosObservedProperties(off.1)[1],
                                                   eventTime = sosCreateTime(sos = mySOS,
                                                                             time = "2017-12-19::2017-12-20"),
                                                   responseFormat = "text/xml; subtype=\"om/1.0.0\"",
                                                   inspect = TRUE)
  )
  expect_s4_class(obs.1, "OmObservationCollection")
  # no verbose output:
  expect_false(grepl("Requesting offering ws2500", output))
  expect_false(grepl("New attributes list is AirTemperature, AirTemperature, degC", output))
  expect_false(grepl("Finished getObservation to http://sensorweb.demo", output))
  # request and response doc
  expect_match(output, "REQUEST:")
  expect_match(output, "RESPONSE DOC:\n\\{xml_document\\}")
})
