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
# Created: 2019-03-29                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
context("extensions: integration tests\n")

test_that("replacing a parsing function", {
  skip_on_cran()

  myER <- function(xml, sos, verbose) {
    return("EXCEPTION!!!11")
  }
  myParsers <- SosParsingFunctions("ows:ExceptionReport" = myER)
  cat("\n")
  exceptionParserSOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/pox",
                            parsers = myParsers,
                            binding = "POX", useDCPs = FALSE)
  erroneousResponse <- getObservation(exceptionParserSOS,
                                      offering = sosOfferings(exceptionParserSOS)[[1]],
                                      observedProperty = list("Bazinga!"))
  expect_equal(erroneousResponse, "EXCEPTION!!!11")
})

test_that("disabling all parsing functions", {
  skip_on_cran()

  disabledParserSOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/pox",
                           parsers = SosDisabledParsers(),
                           binding = "POX", useDCPs = FALSE)
  unparsed <- getObservation(disabledParserSOS,
                             offering = sosOfferings(disabledParserSOS)[[1]],
                             observedProperty = list("Bazinga!"))
  expect_s3_class(unparsed, c("xml_document", "xml_node"))
})

