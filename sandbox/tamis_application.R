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


as.SpatialPointsDataFrame.list.Om_OMObservation <- function (obs) {
  sp <- do.call(rbind, lapply(obs, function(x) as.Spatial.MonitoringPoint(x@featureOfInterest@feature)))
  
  res <- as.numeric(sapply(obs, function(x) x@result))
  sp <- addAttrToGeom(sp, data.frame(obs=res))
  colnames(sp@data) <- obs[[1]]@observedProperty@href
  return(sp)
}

## 

testsos <- SOS(url = "http://fluggs.wupperverband.de/sos2/sos", version = "2.0.0", binding = "KVP")
observation <- parseGetObservationResponse(xmlRoot(xmlParseDoc("tests/responses/GetObservationResponse_measure_results.xml")), testsos)

spDf <- as.SpatialPointsDataFrame.list.Om_OMObservation(observation)
spplot(spDf, scales=list(draw=T))

grid <- SpatialGrid(GridTopology(spDf@bbox[,1], c(1000, 1000), c(50,25)),
                    proj4string = spDf@proj4string)

library(gstat)
gridDf <- krige(Abfluss ~ 1, spDf, grid)
spplot(gridDf, "var1.pred")

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
