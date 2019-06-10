library(provParseR)
library(testthat)

context("Parsing elapsedTime strings")

# test cases
t0 <- "1.0"    # normal as.double parse
t1 <- "1,0"    # decimal separator is ','
t2 <- "1,234,56"    # ',' in digit grouping and digit separator
t3 <- "1.234.56"    # '.' in digit grouping and digit separator
t4 <- "1,234,567,891.23"    # multiple use of ',' in digit grouping
t5 <- "1.234.567.891,23"    # '.' in digit grouping, ',' as digit separator
t6 <- "1.234,567.891,23"    # multiple uses of '.' and ','
t7 <- c(t0,t1,t2,t3,t4,t5,t6)

# expected results
e0 <- as.double("1.0")
e1 <- as.double("1234.56")
e2 <- as.double("1234567891.23")
e3 <- unname(sapply(
        c("1.0","1.0","1234.56","1234.56",
          "1234567891.23","1234567891.23","1234567891.23"),
        as.double))

# test
expect_equal(parse.elapsed.time(t0), e0)
expect_equal(parse.elapsed.time(t1), e0)
expect_equal(parse.elapsed.time(t2), e1)
expect_equal(parse.elapsed.time(t3), e1)
expect_equal(parse.elapsed.time(t4), e2)
expect_equal(parse.elapsed.time(t5), e2)
expect_equal(parse.elapsed.time(t6), e2)
expect_equal(parse.elapsed.time(t7), e3)
