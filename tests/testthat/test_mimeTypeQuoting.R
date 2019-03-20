context("Quoting characters inside MimeTypes")

test_that("No '&quot;' string inside Mimetypes mimeTypeOM and mimeTypeSML", {
	expect_equal(grepl(pattern = "quot", x = mimeTypeOM), FALSE)
	expect_equal(grepl(pattern = "quot", x = mimeTypeSML), FALSE)
})

test_that("We can run describeSensor()", {
  skip("re-add after xml2")

  mySOS <- SOS(url = "http://sos.irceline.be/sos")
  procs <- unique(unlist(sosProcedures(mySOS)))
  expect_warning(S <- describeSensor(mySOS,procs[[1]])) #,verbose = T)
	expect_equal(as.character(class(S)), "SensorML")
})
