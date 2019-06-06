library(provParseR)
library(testthat)

context("Reading list of sourced scripts")

# cases
c0 <- system.file("testdata/cases", "empty.json", package = "provParseR")
c1 <- system.file("testdata/cases/test_sourcedScripts", "1SourcedScript.json", package = "provParseR")
c4 <- system.file("testdata/cases/test_sourcedScripts", "4SourcedScripts.json", package = "provParseR")

t0 <- get.scripts(prov.parse(c0))
t1 <- get.scripts(prov.parse(c1))
t4 <- get.scripts(prov.parse(c4))

# expected
expected.file <- system.file("testdata/expected", "sourced-scripts.csv", package = "provParseR")

e4 <- read.csv(expected.file, header=TRUE, row.names=1, stringsAsFactors=FALSE)
e0 <- e4[1, ]
e1 <- e4[1:2, ]

# test
expect_equivalent(t0, e0)
expect_equivalent(t1, e1)
expect_equivalent(t4, e4)
