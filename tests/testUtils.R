# TODO: Add comment
# 
# Author: Daniel
###############################################################################


dcps <- c("POST" = "http://url/with/endpoint/one", "POST" = "url.to/endpoint/two",
		"GET" = "some.thing.com/different/")
		
.sosFilterDCPs(dcp = dcps, pattern = SosDefaultDCPs()[[owsGetName]], TRUE)
dcpsTest <- .sosFilterDCPs(dcp = dcps, pattern = list("POX" = "/endpoint"), TRUE)
dcpsTest2 <- .sosFilterDCPs(dcps, list("POX" = "/one"), TRUE)

dcpsTest; dcpsTest2