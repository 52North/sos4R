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
# Created: 2019-05-15                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
############################################################################## #
#
# GML Utility Functions and methos
#
#
# Checks if the current node refers to another node by an in-documents reference
#
gmlIsNodeReferenced <- function(sos, node) {
  href <- xml2::xml_attr(x = node, attr = "xlink:href", ns = sos@namespaces)
  return(!is.na(href) && startsWith(href, "#"))
}
#
# Returns the referenced node in doc that is referenced in node
#
gmlGetReferencedNode <- function(sos, doc, node, verbose = FALSE) {
  nodeHref <- xml2::xml_attr(x = node, attr = "xlink:href", ns = sos@namespaces)
  nodeHref <- stringr::str_remove_all(nodeHref, "#")
  if (verbose) cat(paste0("[gmlGetReferencedNode] trying to get referenced node via '", nodeHref, "'\n"))
  
  referencedNode <- xml2::xml_parent(
    xml2::xml_find_first(x = xml2::xml_root(doc), xpath = paste0("//*[@gml:id='", nodeHref, "']"))
  )
  if (is.na(referencedNode)) {
    stop(paste0("[gmlGetReferencedNode] XML document invalid. Node reference '", nodeHref ,"' not in document."))
  }
  if (verbose) cat("[gmlGetReferencedNode] Found node, using the one from ",
                   xml2::xml_attr(x = xml2::xml_parent(referencedNode),
                                  attr = "gml:id", ns = sos@namespaces), "\n")
  return(referencedNode)
}