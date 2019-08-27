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

json <- system.file("testdata", "valType.json", package = "provParseR")
prov <- prov.parse(json)

# invalid data node
test_that("querying invalid data nodes", {
	
	# querying 1 invalid value
	expect_null(get.val.type(prov, "d25"))
	
	# querying multiple invalid values
	expect_null(get.val.type(prov, c("d100", "d500")))
})

expected.file <- system.file("testexpected", "valTypes.csv", package = "provParseR")
expected <- read.csv(expected.file, header = TRUE, row.names = 1, stringsAsFactors = FALSE)

# valid data nodes
test_that("querying valid data nodes", {
	
	# querying no values (get valTyeps for every node in table)
	c1 <- get.val.type(prov)
	expect_equivalent(c1, expected)
	
	# querying 1 value
	c2 <- get.val.type(prov, "d4")
	c3 <- get.val.type(prov, "d5")
	c4 <- get.val.type(prov, "d9")
	
	e2 <- expected[expected$id == "d4", ]
	e3 <- expected[expected$id == "d5", ]
	e4 <- expected[expected$id == "d9", ]
	
	expect_equivalent(c2, e2)
	expect_equivalent(c3, e3)
	expect_equivalent(c4, e4)
	
	# querying multiple values
	q5 <- c("d3", "d5", "d6", "d7")
	q6 <- c("d1", "d2")
	q7 <- c("d7", "d8", "d9")
	
	c5 <- get.val.type(prov, q5)
	c6 <- get.val.type(prov, q6)
	c7 <- get.val.type(prov, q7)
	
	e5 <- expected[expected$id %in% q5, ]
	e6 <- expected[expected$id %in% q6, ]
	e7 <- expected[expected$id %in% q7, ]
	
	expect_equivalent(c5, e5)
	expect_equivalent(c6, e6)
	expect_equivalent(c7, e7)
})

# mixture of valid and invalid nodes
test_that("querying valid and invalid nodes", {
	
	q8 <- c("d5", "d100")
	q9 <- c("d30", "d5", "d100", "d2")
	
	c8 <- get.val.type(prov, q8)
	c9 <- get.val.type(prov, q9)
	
	e8 <- expected[expected$id == "d5", ]
	e9 <- expected[expected$id %in% c("d5", "d2"), ]
	
	expect_equivalent(c8, e8)
	expect_equivalent(c9, e9)
})