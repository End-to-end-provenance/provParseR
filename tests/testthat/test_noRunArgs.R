library(provParseR)
library(testthat)

context("Backwards Compatibility - no run args")

json <- system.file("testdata", "noRunArgs.json", package = "provParseR")
prov <- prov.parse(json)

test_that("Tool Information", {
	
	tool.info <- get.tool.info(prov)
	
	expect_match(class(tool.info), "data.frame")
	
	expect_match(typeof(tool.info$tool.name), "character")
	expect_match(typeof(tool.info$tool.version), "character")
	expect_match(typeof(tool.info$json.version), "character")
	
	expect_equal(nrow(tool.info), 1)
	expect_equal(ncol(tool.info), 3)
	
	expect_equal (tool.info$tool.name, "rdtLite")
	expect_equal (tool.info$tool.version, "1.1.1")
	expect_equal (tool.info$json.version, "2.2")
})

test_that("Arguments", {
	expect_equivalent(get.args(prov), list())
})