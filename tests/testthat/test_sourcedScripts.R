library(provParseR)
library(testthat)

context("Reading list of sourced scripts")

# function to remove timestamp and file path from data frame comparison
get.filenames <- function(df)
{	
	scripts <- df[ , "script"]
	return(unname(sapply(scripts, basename)))
}

# cases - remove timestamp & file path from sourced script names
c0 <- system.file("testdata", "empty.json", package = "provParseR")
c1 <- system.file("testdata", "sourcescript1.json", package = "provParseR")
c3 <- system.file("testdata", "sourcescript3.json", package = "provParseR")

t0 <- get.filenames(get.scripts(prov.parse(c0)))
t1 <- get.filenames(get.scripts(prov.parse(c1)))
t3 <- get.filenames(get.scripts(prov.parse(c3)))

# expected
expected.file.0 <- system.file("testexpected", "sourcescripts0.csv", package = "provParseR")
expected.file.1 <- system.file("testexpected", "sourcescripts1.csv", package = "provParseR")
expected.file.3 <- system.file("testexpected", "sourcescripts3.csv", package = "provParseR")

e0 <- read.csv(expected.file.0, header=TRUE, row.names=1, stringsAsFactors=FALSE)
e1 <- read.csv(expected.file.1, header=TRUE, row.names=1, stringsAsFactors=FALSE)
e3 <- read.csv(expected.file.3, header=TRUE, row.names=1, stringsAsFactors=FALSE)

e0 <- get.filenames(e0)
e1 <- get.filenames(e1)
e3 <- get.filenames(e3)

# test
expect_equivalent(t0, e0)
expect_equivalent(t1, e1)
expect_equivalent(t3, e3)
