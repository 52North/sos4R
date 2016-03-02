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

as.STFDF.list.Om_OMObservation <- function (obs) {
  sp <- do.call(rbind, lapply(obs, function(x) as.Spatial.MonitoringPoint(x@featureOfInterest@feature)))
  
  res <- lapply(obs, function(x) x@result)
#   stopifnot(all(sapply(res, ncol) == 2))
            
  ids <- lapply(obs, function(x) x@featureOfInterest@feature@id)
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


nsVasl <- c("A, B; C, D")
sosParsers(testsos)[["DataArray"]]
.encParser <- sosParsers(testsos)[[sweEncodingName]]

sos4R::sweTextBlockName
sos4R::sweTextEncodingName

parseTextEncoding(list(blockSeparator=";", tokenSeparator=","))

.encParser(list(ns:TextEncoding blockSeparator=";" tokenSeparator=",")
sosParsers(testsos)[["values"]](nsVals, encoding)


sos4R::parseGetObservationResponse()

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

########
# dump #
########

#################
testsos <- SOS(url = "http://fluggs.wupperverband.de/sos2/sos", version = "2.0.0", binding = "KVP")
observation <- parseGetObservationResponse(xmlRoot(xmlParseDoc("tests/responses/GetObservationResponse.xml")), testsos)

stfdf <- as.STFDF.list.Om_OMObservation(observation)

plot(stfdf[1,])
