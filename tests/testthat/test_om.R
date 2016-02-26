.opts <- curlOptions(header = TRUE, userpwd = "tamis:vmV#GnX?U837.8,?", netrc = TRUE)

testsos <- SOS(url = "http://fluggs.wupperverband.de/sos2-tamis/service", version = "2.0.0", binding = "KVP", curlOptions = .opts)
offerings <- sosOfferingIds(testsos)

observation <- getObservation(testsos,
                              offering = offerings[[1]],
                              observedProperty =sosObservableProperties(testsos)[[offerings[[1]]]][7],
                              eventTime = "om:phenomenonTime,2016-01-01T08:00/2016-01-08T20:00", responseFormat = "http://www.opengis.net/om/2.0",
                              verbose=TRUE)
