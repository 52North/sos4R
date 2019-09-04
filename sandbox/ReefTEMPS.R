library(sos4R)
sos <- SOS("http://sosoceano.ird.nc/service/", version = "2.0.0", binding = "KVP", inspect=TRUE, verbose=T)

# not working:
sites(sos)

# working:
phens <- phenomena(sos)

# emtpy:
getData(sos, phenomena = phens[[1]][3], sites = "ANSEVA01", verbose=TRUE)

# empty response:
getData(sos, phenomena = phens[[1]][5], sites = "ANSEVA01", verbose=TRUE, inspect=TRUE,
        begin = as.POSIXct("2019-05-01"),
        end = as.POSIXct("2019-05-05"))

# probably not empty, but slooowwww ....

sTime <- Sys.time()
getData(sos, phenomena = phens[[1]][5], sites = "ANSEVA01", verbose=TRUE, inspect=TRUE)
eTime <- Sys.time()

eTime - sTime

