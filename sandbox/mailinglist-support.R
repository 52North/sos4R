################################################################################
# This script file collects questions from the mailing list and forum          #
# available at http://geostatistics.forum.52north.org/.                        #
#                                                                              #
# Please note that code is supplied by the respective mailing list or forum    #
# member and only copied here for testing.                                     #
################################################################################

################################################################################
# Re: [52N Geostatistics] about sos4R
# Mon, 22 Nov 2010 10:37:34 +0100 (CET)
# http://geostatistics.forum.52north.org/Re-52N-Geostatistics-about-sos4R-tp1909723p1944283.html

# add a conversion function for the field definition "...:chla_conc"
ise_chla.converters <-
		SosDataFieldConvertingFunctions("urn:ogc:def:property:OGC:1.0.30:chla_conc"
						= sosConvertDouble)
ise <- SOS("http://sos.ise.cnr.it/sos", dataFieldConverters =
				ise_chla.converters)
ise

# Offering
ise.offerings <- sosOfferings(ise)
ise.offerings

# set up request parameters
sosProcedures(ise)
station_ise_chla <- sosProcedures(ise)[[5]] # [[5]] mean second procedures:
total_chla_FP
station_ise_chla
sosOfferings(ise)
Offering_ise_chla <- sosOfferings(ise)[["total_chla_FP"]]
Offering_ise_chla
ise.chla.features <- sosFeaturesOfInterest(ise.offerings[["total_chla_FP"]])
# spatial filtering: check out 
ise.chla.features <- SosFeatureOfInterest(objectIDs =
				ise.chla.features[1:11])
ise.chla.features
ise_chla_time <- sosCreateTimePeriod(sos = ise,
		begin = as.POSIXct("2005-08-30 00:00"),
		end = as.POSIXct("2005-08-30 23:00"))
ise_chla_time

# make the request
obs_ise_chla <- getObservation(sos = ise,
		procedure = station_ise_chla,
		offering = Offering_ise_chla,
		featureOfInterest = ise.chla.features,
		eventTime = sosCreateEventTimeList(ise_chla_time),
		inspect = TRUE)

data_ise_chla <- sosResult(obs_ise_chla)
data_ise_chla

# extract the positions
coords_ise_chla <- sosCoordinates(obs_ise_chla)

# attach the positions to the data frame (attention: matching names different in SVN version)
data_coords_ise_chla <- merge(x = data_ise_chla, y = coords_ise_chla,
		by.x = "feature", by.y = "foi_id")

# after version 0.1-07:
data_coords_ise_chla <- merge(x = data_ise_chla, y = coords_ise_chla)

# some example rows of the merged data.frame
data_coords_ise_chla[c(1:3, 101:103, 301:303),]




