################################################################################
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
# Created: 2013-03-06                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
################################################################################

context("utils")

test_that(".sosFilterDCPs works", {
	dcps <- c("ows:Post" = "http://url/with/endpoint/one",
			"ows:Post" = "url.to/endpoint/two",
			"ows:Get" = "some.thing.com/different/")

	expect_equal(length(sos4R:::.sosFilterDCPs(dcp = dcps, pattern = "*")), 3)
	expect_equivalent(sos4R:::.sosFilterDCPs(dcp = dcps, pattern = list("POX" = "/endpoint"))[[2]], "url.to/endpoint/two")
	expect_equal(sos4R:::.sosFilterDCPs(dcp = dcps, pattern = list("POX" = "/endpoint")),
	             c("ows:Post" = "http://url/with/endpoint/one", "ows:Post" = "url.to/endpoint/two"))
	expect_equivalent(sos4R:::.sosFilterDCPs(dcps, list("POX" = "/one")), "http://url/with/endpoint/one")
})

test_that("addional KVPs are concatenated correctly", {
  expected <- "this=is&working=correctly"
  actual <- list("this" = "is", "working" = "correctly")
  expect_equal(.encodeAdditionalKVPs(actual), expected)
})

test_that("can get CRS from URN", {
  obj <- sosGetCRS("urn:ogc:def:crs:EPSG::4326")
  expect_s4_class(obj, "CRS")
  expect_match(obj@projargs, "init=epsg:4326")
})

test_that("can get CRS from lowercase URN", {
  obj <- sosGetCRS("urn:ogc:def:crs:epsg::4326")
  expect_s4_class(obj, "CRS")
  expect_match(obj@projargs, "init=epsg:4326")
})

test_that("can get CRS from URN with version", {
  obj <- sosGetCRS("urn:ogc:def:crs:EPSG:99:4326")
  expect_s4_class(obj, "CRS")
  expect_match(obj@projargs, "init=epsg:4326")
})

test_that("can get CRS from URL", {
  obj <- sosGetCRS("http://www.opengis.net/def/crs/EPSG/0/4326")
  expect_s4_class(obj, "CRS")
  expect_match(obj@projargs, "init=epsg:4326")
})
