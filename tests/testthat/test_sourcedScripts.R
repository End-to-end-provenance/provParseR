library(provParseR)
library(testthat)

context("Reading list of sourced scripts")

# function to remove timestamp and file path from data frame comparison
process.df <- function(df)
{
	# remove timestamp
	df <- df[ , 1:2]
	
	# remove file paths from source script names
	script <- df["script"]
	sapply(script, basename)
	
	# replace script name column
	df["script"] <- unname(script)
	
	return(df)
}

# cases - remove timestamp & file path from sourced script names
c0 <- system.file("testdata", "empty.json", package = "provParseR")
c1 <- system.file("testdata", "sourcescript1.json", package = "provParseR")
c4 <- system.file("testdata", "sourcescript4.json", package = "provParseR")

t0 <- process.df(get.scripts(prov.parse(c0)))
t1 <- process.df(get.scripts(prov.parse(c1)))
t4 <- process.df(get.scripts(prov.parse(c4)))

# expected
expected.file <- system.file("testexpected", "sourcescripts.csv", package = "provParseR")

e4 <- read.csv(expected.file, header=TRUE, row.names=1, stringsAsFactors=FALSE)
e0 <- e4[1, ]
e1 <- e4[1:2, ]

# test
expect_equivalent(t0, e0)
expect_equivalent(t1, e1)
expect_equivalent(t4, e4)

