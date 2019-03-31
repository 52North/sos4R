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
# Author: Benjamin Pross (b.pross@52north.org)                                 #
# Created: 2016-01-27                                                          #
# Project: sos4R - visit the project web page,                                 #
#      http://52north.org/communities/sensorweb/clients/sos4R/                 #
################################################################################

#
# parse gml:pos from wml2:MonitoringPoint
#
parseMonitoringPoint <- function(obj, sos, verbose = FALSE) {
  .sampledFeatures <- xml2::xml_find_all(x = obj, xpath = saSampledFeatureName, ns = SosAllNamespaces())
  .id <-xml2::xml_attr(x = obj, attr = "id", default = NA_character_)

  .names <- xml2::xml_find_all(x = obj, xpath = gmlNameName, ns = SosAllNamespaces())
  .identifier <- xml2::xml_find_all(x = obj, xpath = gmlIdentifierName, ns = SosAllNamespaces())
  .shape <- parseSamsShape(xml2::xml_find_all(x = obj, xpath = samsShapeName, ns = SosAllNamespaces()), sos)

  .mp <- MonitoringPoint(.sampledFeatures, .id, .identifier, .names, .shape)

  return(.mp)
}
