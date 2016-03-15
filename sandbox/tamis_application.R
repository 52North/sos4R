## tamis
library(sos4R)
library(spacetime)

as.Spatial.MonitoringPoint <- function(obj, ...) {
  
  .extractCRS <- function(obj) {
    chars <- strsplit(obj@shape@point@pos@srsName, "/", fixed=T)[[1]]
    stopifnot(any(c("EPSG","epsg") %in% chars))
    CRS(paste("+init=epsg:", tail(chars,1), sep = ""))
  }
  
  if("point" %in% slotNames(obj@shape))
    return(SpatialPoints(matrix(rev(as.numeric(strsplit(obj@shape@point@pos@pos, " ", fixed=T)[[1]])), ncol = 2),
                         proj4string = .extractCRS(obj)))
}

# obs <- wasserstand
# obs <- sickerwasser
as.STFDF.list.Om_OMObservation <- function (obs) {
  sp <- do.call(rbind, lapply(obs, function(x) as.Spatial.MonitoringPoint(x@featureOfInterest@feature)))
  
  res <- lapply(obs, function(x) x@result)

  ids <- lapply(obs, function(x) x@featureOfInterest@feature@id)
  
  if(any(sapply(res, is.null))) {
    warning("The following ids have been dropped as they did not contain any data:", paste(ids[dropIds], "\n"))
    dropIds <- which(sapply(res, is.null))
    res <- res[-dropIds]
    sp <- sp[-dropIds]
    ids <- ids[-dropIds]
  }
    
  data <- res[[1]]
  colnames(data)[-1] <- ids[[1]]
  if(length(res)>1) {
    for (df in 2:length(res)) {
      colnames(res[[df]])[-1] <- ids[[df]]
      
      data <- merge(data, res[[df]])
    }
  }
  
  time <- as.POSIXct(data[,1])
  
  data <- data.frame(as.numeric(t(as.matrix(data[,-1]))))
  colnames(data) <- tail(names(obs[[1]]@result),1)
  
  STFDF(sp, time, data)
}

as.SpatialPointsDataFrame.list.OmOM_Observation <- function (obs) {
  sp <- do.call(rbind, lapply(obs, function(x) as.Spatial.MonitoringPoint(x@featureOfInterest@feature)))
  
  res <- as.numeric(sapply(obs, 
                           function(x) {
                             res <- x@result
                             if(is.null(res))
                               res <- NA_character_
                             res
                           }))
  sp <- addAttrToGeom(sp, data.frame(obs=res))
  colnames(sp@data) <- obs[[1]]@observedProperty@href
  return(sp)
}

## 

mNHNConv <- function(x, sos) {
  as.double(x)
}

source("~//52North//secOpts.R") # sets passwd and user for SOS as curlOptions
testsos <- SOS(url = "http://fluggs.wupperverband.de/sos2-tamis/service",
               # dataFieldConverters = SosDataFieldConvertingFunctions("mNHN" = mNHNConv),  
               version = "2.0.0", binding = "KVP", curlOptions = .opts)
offerings <- sosOfferingIds(testsos)

# debug(testsos@parsers[["values"]])
# debug(sos4R:::.getFeatureOfInterest_2.0.0)

caps <- getCapabilities(testsos)
observation <- getObservation(testsos,
                              offering = offerings[[1]],
                              observedProperty = sosObservableProperties(testsos)[[offerings[[1]]]][9],
                              eventTime = "om:phenomenonTime,2015-08-16T00:00/2015-12-31T23:59", 
                              responseFormat = caps@contents@observationOfferings$Zeitreihen_Handeingabe@responseFormat[[1]],
                              verbose=TRUE, saveOriginal = TRUE)

lapply(observation, function(x) x@result)

spDf <- as.SpatialPointsDataFrame.list.OmOM_Observation(observation)
spplot(spDf, scales=list(draw=T))

grid <- SpatialGrid(GridTopology(spDf@bbox[,1], c(1000, 1000), c(50,25)),
                    proj4string = spDf@proj4string)

library(gstat)
gridDf <- krige(Abfluss ~ 1, spDf, grid)
spplot(gridDf, "var1.pred")

library(rgdal)
writeGDAL(gridDf, "gridDf.tiff", drivername = "GTiff")

library(plotKML)
plotKML(spDf)







#############
library(sos4R)

source("~//52North//secOpts.R") # sets passwd and user for SOS as curlOptions
TaMIS_SOS <- SOS(url = "http://fluggs.wupperverband.de/sos2-tamis/service",
                 version = "2.0.0", binding = "KVP", curlOptions = .opts)

TaMIS_offs <- sosOfferingIds(TaMIS_SOS)
TaMIS_prop <- sosObservableProperties(TaMIS_SOS)
TaMIS_caps <- getCapabilities(TaMIS_SOS)

# ??????????
# sosFeatureIds(TaMIS_SOS)
# SosFeatureOfInterest(TaMIS_SOS)
# sosFeaturesOfInterest(TaMIS_SOS)

# request=GetObservation
# responseformat=http://www.opengis.net/om/2.0
# observedProperty=Wasserstand_im_Damm
# procedure=Handeingabe
# featureOfInterest=Bever-Talsperre_MQA1_Piezometer_Wasserseite_Schuettkoerper
# namespaces=xmlns%28sams%2Chttp%3A%2F%2Fwww.opengis.net%2FsamplingSpati-al%2F2.0%29%2Cxmlns%28om%2Chttp%3A%2F%2Fwww.opengis.net%2Fom%2F2.0%29
# temporalFilter=om%3AphenomenonTime%2C2016-01-01T10:00:00.00Z%2F2016-03-10T13:00:00.000Z

wasserstand <- getObservation(TaMIS_SOS,
                              offering = TaMIS_offs[[1]],
                              observedProperty = TaMIS_prop[[TaMIS_offs[[1]]]][9],
                              eventTime = "om:phenomenonTime,2016-01-01T00:00/2016-03-10T23:59", 
                              responseFormat = TaMIS_caps@contents@observationOfferings$Zeitreihen_Handeingabe@responseFormat[[1]],
                              verbose=TRUE, saveOriginal = TRUE)

lapply(wasserstand, function(x) x@result)
wasserstand_STFDF <- as.STFDF.list.Om_OMObservation(wasserstand)
stplot(wasserstand_STFDF)

# responseformat=http://www.opengis.net/om/2.0
# observedProperty=Schuettmenge
# procedure=Tageswert_Prozessleitsystem
# featureOfInterest=Bever-Talsperre_Sickerwassermessstelle_S2A
# namespaces=xmlns%28sams%2Chttp%3A%2F%2Fwww.opengis.net%2FsamplingSpati-al%2F2.0%29%2Cxmlns%28om%2Chttp%3A%2F%2Fwww.opengis.net%2Fom%2F2.0%29
# temporalFilter=om%3AphenomenonTime%2C2016-01-01T10:00:00.00Z%2F2016-03-10T13:00:00.000Z

sickerwasser <- getObservation(TaMIS_SOS,
                              offering = TaMIS_offs[[4]],
                              observedProperty = TaMIS_prop[[TaMIS_offs[[4]]]][3],
                              eventTime = "om:phenomenonTime,2016-01-01T00:00/2016-03-10T23:59", 
                              responseFormat = TaMIS_caps@contents@observationOfferings$Zeitreihen_Tageswert_Prozessleitsystem@responseFormat[[1]],
                              verbose=TRUE, saveOriginal = TRUE)

lapply(sickerwasser, function(x) x@result)
str(sickerwasser[[1]])

sickerwasser_STFDF <- as.STFDF.list.Om_OMObservation(sickerwasser)
stplot(sickerwasser_STFDF[,1:9])
plot(sickerwasser_STFDF[sample(6,1),])

# http://www.fluggs.de/sos2/sos?service=SOS
# version=2.0.0

FLUGGS_SOS <- SOS(url = "http://www.fluggs.de/sos2/sos",
                  version = "2.0.0", binding = "KVP")#, curlOptions = .opts)


FLUGGS_offs <- sosOfferingIds(FLUGGS_SOS)
FLUGGS_prop <- sosObservableProperties(FLUGGS_SOS)
FLUGGS_caps <- getCapabilities(FLUGGS_SOS)

# responseformat=http://www.opengis.net/om/2.0
# observedProperty=Niederschlagshoehe
# procedure=Tagessumme
# featureOfInterest=Bever-Talsperre
# namespaces=xmlns%28sams%2Chttp%3A%2F%2Fwww.opengis.net%2FsamplingSpatial%2F2.0%29%2Cxmlns%28om%2Chttp%3A%2F%2Fwww.opengis.net%2Fom%2F2.0%29
# temporalFilter=om%3AphenomenonTime%2C2015-03-10T13:58:07.519Z%2F2016-03-10T13:58:07.519Z

niederschlag <- getObservation(FLUGGS_SOS,
                               offering = FLUGGS_offs[[8]],
                               observedProperty = FLUGGS_prop[[FLUGGS_offs[[8]]]],
                               eventTime = "om:phenomenonTime,2016-01-01T00:00/2016-03-10T23:59", 
                               responseFormat = FLUGGS_caps@contents@observationOfferings$Zeitreihen_Tagessumme@responseFormat[[1]],
                               verbose=TRUE)
lapply(niederschlag, function(x) x@result)

# responseformat=http://www.opengis.net/om/2.0
# observedProperty=Speicherfuellstand
# procedure=Einzelwert
# featureOfInterest=Bever-Talsperre_Windenhaus
# namespaces=xmlns%28sams%2Chttp%3A%2F%2Fwww.opengis.net%2FsamplingSpatial%2F2.0%29%2Cxmlns%28om%2Chttp%3A%2F%2Fwww.opengis.net%2Fom%2F2.0%29
# temporalFilter=om%3AphenomenonTime%2C2016-03-01T10:00:00.00Z%2F2016-03-10T13:00:00.000Z

fuellstand <- getObservation(FLUGGS_SOS,
                               offering = FLUGGS_offs[[2]],
                              observedProperty = FLUGGS_prop[[FLUGGS_offs[[2]]]][6],
                               eventTime = "om:phenomenonTime,2016-01-01T00:00/2016-01-10T23:59", 
                               responseFormat = FLUGGS_caps@contents@observationOfferings$Zeitreihen_Einzelwert@responseFormat[[1]],
                               verbose=TRUE)
lapply(fuellstand, function(x) x@result)
