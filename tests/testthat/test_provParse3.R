library(provParseR)
library(testthat)

## Loading test data
test.data <- system.file("testdata", "prov2.json", package = "provParseR")
prov.parse(test.data)

context("Scripts access function")
scripts.df <- get.scripts()
expect_match(class(scripts.df), "data.frame")
expect_equal (nrow(scripts.df), 5)
