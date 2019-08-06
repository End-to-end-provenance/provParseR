library(provParseR)
library(testthat)

context("Reading list of sourced scripts")

# function to remove timestamp and file path from data frame comparison
process.df <- function(df)
{
  scripts <- df$script
  return (unname(sapply (scripts, basename)))
}

# cases - remove timestamp & file path from sourced script names
c0 <- system.file("testdata", "empty.json", package = "provParseR")
c1 <- system.file("testdata", "sourcescript1.json", package = "provParseR")
c4 <- system.file("testdata", "sourcescript4.json", package = "provParseR")

t0 <- process.df(get.scripts(prov.parse(c0)))
t1 <- process.df(get.scripts(prov.parse(c1)))
t4 <- process.df(get.scripts(prov.parse(c4)))

# test
expect_equivalent(t0, "empty.R")
expect_equivalent(t1, c("sourcescript1.R", "source1.r"))
expect_equivalent(t4, c("sourcescript4.R", "source2.r", "source3.r", "source4.r"))

