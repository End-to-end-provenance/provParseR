library(provParseR)
library(testthat)

context("Parsing valType")

# no data nodes
test_that("No data nodes", {
	
	json <- system.file("testdata", "empty.json", package = "provParseR")
	prov <- prov.parse(json)
	
	expect_null(get.val.type(prov))
	expect_null(get.val.type(prov, "d1"))
})


