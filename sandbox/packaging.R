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
# Created: 2010-06-18                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
################################################################################

#
#
#
library("tools")
?codoc # Check Code/Documentation Consistency

################################################################################
# tools::showNonASCII(readLines('sos4R.Rnw')) 
checkNonASCII <- function(pkgPath) {
	require("tools")
	
	# get all files in the workspace
	p <- pkgPath
	ps <- c("", "demo", "inst", "inst/doc", "man", "R", "sandbox", "tests")
	dirs <- paste(p, ps, sep = "/")
	
	?showNonASCII
	?readLines
	?dir
	
	filenames <- lapply(dirs, dir)
	
	filepaths <- list()
	for (i in seq(along = dirs)) {
		.l <- paste(dirs[[i]], filenames[[i]], sep = "/")
		filepaths <- c(filepaths, .l)
	}
	
	# remove some folders
	filepaths <- filepaths[!grepl(pattern = "//", x = filepaths)]
	filepaths <- filepaths[!grepl("/R$", filepaths)]
	filepaths <- filepaths[!grepl("/inst/doc$", filepaths)]
	filepaths <- filepaths[!grepl(".RData$", filepaths)]
	filepaths <- filepaths[!grepl(".pdf$", filepaths)]
	filepaths
	
	# check characters
	for (i in seq(along = filepaths)) {
		cat(filepaths[[i]], "\n")
		.file <- readLines(filepaths[[i]])
		showNonASCII(.file)
	}
}

checkNonASCII(".")

################################################################################
# tools::compactPDF
#?tools::compactPDF

# run this before every commit...
result <- tools::compactPDF(paths = "./inst/doc")
result
# or even better: run R CMB build with option "--compact-vignettes"
