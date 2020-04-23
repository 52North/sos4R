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
# Created: 2019-07-04                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
context("R-helper::sosIsListFieldAvailable")
#
# test sosIsListFieldAvailable ----
#
test_that("test sosIsListFieldAvailable", {
  expect_false(sosIsListFieldAvailable(NA))
  expect_false(sosIsListFieldAvailable(NULL))
  expect_false(sosIsListFieldAvailable("you shall return false"))
  expect_false(sosIsListFieldAvailable(list(NA,"", "you shall return false, again")))
  expect_false(sosIsListFieldAvailable(list(2.0,"", "you shall return false, again")))
  expect_false(sosIsListFieldAvailable(list("")))
  expect_true(sosIsListFieldAvailable(list("a")))
  expect_true(sosIsListFieldAvailable(list("a","b")))
})