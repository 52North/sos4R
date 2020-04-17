# a global variable to cache objects from documents when parsing GetDataAvailability responses
cacheVariables <- c("featureCache", "phenTimeCache", "timeObjectCache")
utils::globalVariables(cacheVariables)
utils::suppressForeignCheck(cacheVariables)
