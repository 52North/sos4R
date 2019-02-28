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
# Author: - Daniel Nuest (daniel.nuest@uni-muenster.de)                        #
#         - Jürrens, Eike Hinderk (e.h.juerrens@52north.org)                   #
# Created: 2015-01-27                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################
context("DescribeSensor")

testsos <- SOS_Test(name = "testcaps")
axiomCaps <- parseSosCapabilities(xmlParseDoc("../responses/Capabilities_52N-SOS_Axiom.xml"), testsos)
testsos@capabilities <- axiomCaps

test_that("output format is encoded correctly for POX", {
    describeSensorOp <- sosOperation(testsos, sosDescribeSensorName)
    of <- describeSensorOp@parameters[["outputFormat"]][[1]]
    
    ds <- SosDescribeSensor(service = sosService, version = testsos@version, procedure = sosProcedures(testsos)[[1]], outputFormat = of)
    
    expect_equal(ds@outputFormat, "text/xml; subtype=\"sensorML/1.0.1/profiles/ioos_sos/1.0\"")
    
    xml <- encodeRequestXML(ds, testsos)
    expect_match(toString(xml), "text/xml; subtype=&quot;sensorML/1.0.1/profiles/ioos_sos/1.0&quot;")
})

test_that("output format is encoded correctly for KVP", {
    describeSensorOp <- sosOperation(testsos, sosDescribeSensorName)
    of <- describeSensorOp@parameters[["outputFormat"]][[1]]
    
    ds <- SosDescribeSensor(service = sosService, version = testsos@version, procedure = sosProcedures(testsos)[[1]],
                            outputFormat = of)
    url <- encodeRequestKVP(ds, testsos) # fowards to .sosEncodeRequestKVPDescribeSensor_1.0.0
    expect_match(toString(url), "text%2Fxml%3B%20subtype%3D%22sensorML%2F1.0.1%2Fprofiles%2Fioos_sos%2F1.0%22")
    
    # test different quotation variants and spaces
    ds <- SosDescribeSensor(service = sosService, version = testsos@version, procedure = sosProcedures(testsos)[[1]],
                            outputFormat = 'text/xml;subtype="sensorML/1.0.1"')
    url <- encodeRequestKVP(ds, testsos)
    expect_match(toString(url), "text%2Fxml%3Bsubtype%3D%22sensorML%2F1.0.1%22")

    ds <- SosDescribeSensor(service = sosService, version = testsos@version, procedure = sosProcedures(testsos)[[1]],
                        outputFormat = "text/xml;subtype=\"sensorML/1.0.1\"")
    url <- encodeRequestKVP(ds, testsos)
    expect_match(toString(url), "text%2Fxml%3Bsubtype%3D%22sensorML%2F1.0.1%22")

    ds <- SosDescribeSensor(service = sosService, version = testsos@version, procedure = sosProcedures(testsos)[[1]],
                        outputFormat = "text/xml; subtype=\"sensorML/1.0.1\"")
    url <- encodeRequestKVP(ds, testsos)
    expect_match(toString(url), "text%2Fxml%3B%20subtype%3D%22sensorML%2F1.0.1%22")

})