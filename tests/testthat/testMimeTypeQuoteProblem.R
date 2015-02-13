library(sos4R)
context("Quoting characters inside MimeTypes")

test_that("No '&quot;' string inside Mimetypes mimeTypeOM and mimeTypeSML", {
	expect_equal(grepl(mimeTypeOM, "quot"), FALSE)
	expect_equal(grepl(mimeTypeOM, "quot"), FALSE)
})

mySOS <- SOS(url = "http://sos.irceline.be/sos")
procs <- unique(unlist(sosProcedures(mySOS)))
S = describeSensor(mySOS,procs[[1]],verbose = T)

test_that("We can run describeSensor()", {
	expect_equal(as.character(class(S)), "SensorML")
})
