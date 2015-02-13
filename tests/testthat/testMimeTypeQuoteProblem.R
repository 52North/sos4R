library(sos4R)
context("Quoting characters inside MimeTypes")

test_that("No '&quot;' string inside Mimetypes mimeTypeOM and mimeTypeSML", {
	expect_equal(grepl(mimeTypeOM, "quot"), FALSE)
	expect_equal(grepl(mimeTypeOM, "quot"), FALSE)
})
