################################################################################
# Copyright (C) 2010 by 52 North                                               #
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
# Created: 2011-02-09                                                          #
# Project: sos4R - visit the project web page, http://www.nordholmen.net/sos4r #
#                                                                              #
################################################################################

#
#
#
plot.SosObservationOffering <- function(x, y, ..., map.database = "world", 
		proj4string = sosGetCRS(x), add = FALSE,
		world.col = "grey50",
		regions = x,
		cities.pch = 19, cities.label = FALSE, cities.col = "black",
		cities.database = "world.cities",
		off.label = TRUE, off.col = "red", off.lwd = 3, off.lty = 1,
		ylabeloffset = 0, # offset of the label
		map.axes = TRUE, map.scale = TRUE, map.cities = FALSE) {
	require("maps")
	require("sp")
	require("maptools") # for pruneMap
	data(cities.database)
	
	.name <- sosName(x)
	.spPoly <- as(x, "SpatialPolygons")
	
	if(!add) {
		# if regions is based on offering, then calculate it, otherwise use given 
		if(is(regions, "SosObservationOffering")) {
			.regions <- map.where(database = map.database, coordinates(.spPoly))
			if(is.na(.regions)) .regions <- "."
			
			warning("Could not determine region based on coordinates, using '.'.")
		}
		else .regions <- regions
		
		.world <- map(database = map.database, region = .regions, plot = FALSE)
		.world.p <- pruneMap(.world)
		.world.sp <- map2SpatialLines(.world.p, proj4string = proj4string)
		
		plot(x = .world.sp, col = world.col)
		par(new =  TRUE)
	}
	
	plot(x = .spPoly, add = TRUE, border = off.col,
			lwd = off.lwd, lty = off.lty)
	par(new =  TRUE)
	
	if(!add) {
		if(map.axes) map.axes()
		if(map.scale) map.scale(metric = TRUE, ratio = FALSE)
		if(map.cities) map.cities(cities.database, label = cities.label,
					pch = cities.pch, col = cities.col)
	}
	
	if(off.label) {
		text(labels = .name, col = off.col,
				x = coordinates(.spPoly)[1],
				y = coordinates(.spPoly)[2] + ylabeloffset)
	}
	
	par(new =  FALSE)
}

setMethod("plot", signature(x = "SosObservationOffering", y = "missing"),
		plot.SosObservationOffering)

# Spatial(bbox = bbox.bom, proj4string = sosGetCRS(rainfall.off.bom))

#
#
#
plot.SOS <- function(x, y, ..., map.database = "world", 
		world.col = "grey50",
		regions = NA,
		cities.pch = 19, cities.label = FALSE, cities.col = "black",
		off.label = TRUE, off.lwd = 3, off.lty = 1,
		map.axes = TRUE, map.scale = TRUE, map.cities = FALSE) {
	require("RColorBrewer")
	
	.offs <- sosOfferings(x)
	.offs.length <- length(.offs)
	.availableColors <- 12
	.colors <- brewer.pal(max(min(.offs.length, .availableColors), 3), "Paired")
	
	for (i in seq(1, .offs.length)) {
		# http://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/
		.col <- .colors[[i %% .availableColors]]
		.off <- .offs[[i]]
		
		# do not add the first time
		.add <- i != 1
		
		if(any(is.na(regions)))
			.regions <- .off
		else .regions <- regions
				
		plot(x = .off, ..., map.database = map.database, 
				add = .add,
				world.col = world.col,
				regions = .regions,
				cities.pch = cities.pch, cities.label = cities.label,
				cities.col = cities.col,
				off.label = off.label, off.lwd = off.lwd, off.lty = off.lty,
				off.col = .col,
				map.axes = map.axes, map.scale = map.scale,
				map.cities = map.cities,
				ylabeloffset = i * 0.5)
	}
	
	title(main = sosTitle(x), sub = sosAbstract(x))
}

setMethod("plot", signature(x = "SOS", y = "missing"), plot.SOS)

# http://r.789695.n4.nabble.com/plotting-points-in-random-but-different-colors-based-on-condition-td907070.html
.gimmeDiffCol <- function(oldcol) {
	rgbcomp <- col2rgb(oldcol)
	
	if(rgbcomp[1,1]<127) newred <- sample(rgbcomp[1,1]:255,1)/255
	else newred <- sample(0:rgbcomp[1,1],1)/255
	
	if(rgbcomp[2,1]<127) newgreen <- sample(rgbcomp[2,1]:255,1)/255
	else newgreen <- sample(0:rgbcomp[2,1],1)/255
	
	if(rgbcomp[3,1]<127) newblue <- sample(rgbcomp[3,1]:255,1)/255
	else newblue <- sample(0:rgbcomp[3,1],1)/255
	
	return(rgb(newred,newgreen,newblue)) 
}
	