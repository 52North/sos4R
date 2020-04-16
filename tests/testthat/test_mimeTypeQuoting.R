context("Quoting characters inside MimeTypes")

test_that("No '&quot;' string inside Mimetypes mimeTypeOM and mimeTypeSML", {
	expect_equal(grepl(pattern = "quot", x = mimeTypeOM), FALSE)
	expect_equal(grepl(pattern = "quot", x = mimeTypeSML), FALSE)
})
