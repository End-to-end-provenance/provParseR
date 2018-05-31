context("Environment capture")
library(EnviR)
library(testthat)
library(jsonlite)

## Loading test data
test.data <- system.file("tests", "ddg.json", package = "EnviR")
prov.parse(test.data)

context("Environment access function")
envi.df <- get.environment()
expect_match(class(envi.df), "data.frame")

context("Procedure nodes access function")
proc.df <- get.proc.nodes()
expect_match(class(proc.df), "data.frame")

context("Data nodes access function")
data.df <- get.data.nodes()
expect_match(class(data.df), "data.frame")

context("Function nodes access function")
func.df <- get.func.nodes()
expect_match(class(func.df), "data.frame")

context("Procedure-to-procedure edges access function")
proc.proc.df <- get.proc.proc()
expect_match(class(proc.proc.df), "data.frame")

context("Data-to-procedure edges access function")
data.proc.df <- get.data.proc()
expect_match(class(data.proc.df), "data.frame")

context("Procedure-to-data edges access function")
proc.data.df <- get.proc.data()
expect_match(class(proc.data.df), "data.frame")

context("Function-to-procedure edges access function")
func.proc.df <- get.func.proc()
expect_match(class(func.proc.df), "data.frame")

context("Function-library group nodes access function")
func.lib.df <- get.func.lib()
expect_match(class(func.lib.df), "data.frame")

context("Library nodes access function")
libs.df <- get.libs()
expect_match(class(libs.df), "data.frame")

context("Scripts access function")
scripts.df <- get.scripts()
expect_match(class(scripts.df), "data.frame")
