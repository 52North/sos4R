# a global variable to cache objects from documents when parsing GetDataAvailability responses
#cacheVariables <- c("featureCache", "phenTimeCache", "timeObjectCache")
#utils::globalVariables(cacheVariables)
#utils::suppressForeignCheck(cacheVariables)

# https://stackoverflow.com/questions/41954302/where-to-create-package-environment-variables
assign(x = "sos4R_caches", value = new.env())
