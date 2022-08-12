library(provParseR)
library(testthat)

## Loading test data
test.data <- system.file("testdata", "prov.json", package = "provParseR")
prov <- prov.parse(test.data)

## Environement
context("Environment access function")
envi.df <- get.environment(prov)
expect_match(class(envi.df), "data.frame")
expect_match(typeof(envi.df$value), "character")
expect_equal(nrow(envi.df), 15)
print (envi.df)
expect_equal(ncol(envi.df), 2)
expect_equal (envi.df$label, c("name", "architecture", "operatingSystem", "language", "langVersion", "ui", "pandoc",
        "script", "scriptTimeStamp", "scriptHash", "totalElapsedTime", 
        "workingDirectory", "provDirectory", "provTimestamp", 
        "hashAlgorithm"))

## Tool information
context ("Tool information")
tool.df <- get.tool.info(prov)
expect_match(class(tool.df), "data.frame")
expect_match(typeof(tool.df$tool.name), "character")
expect_match(typeof(tool.df$tool.version), "character")
expect_match(typeof(tool.df$json.version), "character")
expect_equal(nrow(tool.df), 1)
expect_equal(ncol(tool.df), 3)
expect_equal (tool.df$tool.name, "rdtLite")
expect_equal (tool.df$tool.version, "1.4")
expect_equal (tool.df$json.version, "2.3")

## Arguments
context("Arguments")
args.list <- get.args(prov)

expect_match(class(args.list), "list")
expect_equal(length(args.list), 1)
expect_equivalent(names(args.list), as.character(c(1:length(args.list))))

args.list <- args.list[[1]]
expect_equivalent(names(args.list), c("overwrite", "details", "snapshot.size", "save.debug"))

args.types <- sapply(args.list, class)
expect_equivalent(args.types, c("logical", "logical", "numeric", "logical"))

## Procedure nodes
context("Procedure nodes access function")
proc.df <- get.proc.nodes(prov)
expect_match(class(proc.df), "data.frame")
expect_match(typeof(proc.df$name), "character")
expect_match(typeof(proc.df$type), "character")
expect_match(typeof(proc.df$elapsedTime), "double")
expect_match(typeof(proc.df$scriptNum), "integer")
expect_match(typeof(proc.df$startLine), "integer")
expect_match(typeof(proc.df$startCol), "integer")
expect_match(typeof(proc.df$endLine), "integer")
expect_match(typeof(proc.df$endCol), "integer")
expect_equal(nrow(proc.df), 32)
expect_equal(ncol(proc.df), 9)

## Data nodes
context("Data nodes access function")
data.df <- get.data.nodes(prov)
expect_match(class(data.df), "data.frame")
expect_match(typeof(data.df$name), "character")
expect_match(typeof(data.df$value), "character")
expect_match(typeof(data.df$valType), "character")
expect_match(typeof(data.df$type), "character")
expect_match(typeof(data.df$scope), "character")
expect_match(typeof(data.df$fromEnv), "logical")
expect_match(typeof(data.df$hash), "character")
expect_match(typeof(data.df$timestamp), "character")
expect_match(typeof(data.df$location), "character")
expect_equal(nrow(data.df), 30)
expect_equal(ncol(data.df), 10)

## Exceptions
context("Exception nodes access function")
errors.df <- get.error.nodes(prov)
expect_equal(nrow(errors.df), 1)
expect_equal(errors.df$id, "d30")
expect_equal(errors.df$value, "Error in FUN(newX[, i], ...): invalid 'type' (character) of argument\n")

## Function nodes
context("Function nodes access function")
func.df <- get.func.nodes(prov)
expect_match(class(func.df), "data.frame")
expect_equal(nrow(func.df), 4)
expect_match(typeof(func.df$name), "character")
expect_equal(ncol(func.df), 2)
expect_equal (func.df$name, c("read.csv", "write.csv", "pdf", "dev.off"))

## Procedure-to-procedure edges
context("Procedure-to-procedure edges access function")
proc.proc.df <- get.proc.proc(prov)
expect_match(class(proc.proc.df), "data.frame")
expect_match(typeof(proc.proc.df$informant), "character")
expect_match(typeof(proc.proc.df$informed), "character")
expect_equal(nrow(proc.proc.df), 31)
expect_equal(ncol(proc.proc.df), 3)

## Data-to-procedure edges
context("Data-to-procedure edges access function")
data.proc.df <- get.data.proc(prov)
expect_match(class(data.proc.df), "data.frame")
expect_match(typeof(data.proc.df$entity), "character")
expect_match(typeof(data.proc.df$activity), "character")
expect_equal(nrow(data.proc.df), 23)
expect_equal(ncol(data.proc.df), 3)

## Procedure-to-data edges
context("Procedure-to-data edges access function")
proc.data.df <- get.proc.data(prov)
expect_match(class(proc.data.df), "data.frame")
expect_match(typeof(proc.data.df$entity), "character")
expect_match(typeof(proc.data.df$activity), "character")
expect_equal(nrow(proc.data.df), 28)
expect_equal(ncol(proc.data.df), 3)

## Function-to-procedure edges
context("Function-to-procedure edges access function")
func.proc.df <- get.func.proc(prov)
expect_match(class(func.proc.df), "data.frame")
expect_match(typeof(func.proc.df$"function"), "character")
expect_match(typeof(func.proc.df$activity), "character")
expect_equal(nrow(func.proc.df), 6)
expect_equal(ncol(func.proc.df), 3)

## Function-library grouping
context("Function-library group nodes access function")
func.lib.df <- get.func.lib(prov)
expect_match(class(func.lib.df), "data.frame")
expect_match(typeof(func.lib.df$library), "character")
expect_match(typeof(func.lib.df$"function"), "character")
expect_equal(nrow(func.lib.df), 4)
expect_equal(ncol(func.lib.df), 3)

## Library nodes
context("Library nodes access function")
libs.df <- get.libs(prov)
expect_match(class(libs.df), "data.frame")
expect_match(typeof(libs.df$name), "character")
expect_match(typeof(libs.df$version), "character")
expect_equal(nrow(libs.df), 43)
expect_equal(ncol(libs.df), 4)

## Scripts
context("Scripts access function")
scripts.df <- get.scripts(prov)
expect_match(class(scripts.df), "data.frame")
expect_match(typeof(scripts.df$script), "character")
expect_match(typeof(scripts.df$timestamp), "character")
expect_equal (nrow (scripts.df), 1)
expect_equal(ncol(scripts.df), 3)

## Input files
context ("Input files")
input.files.and.urls <- get.input.files (prov)
expect_equal (nrow (input.files.and.urls), 2)
expect_setequal (input.files.and.urls$name, c("x.csv", "http://harvardforest.fas.harvard.edu/data/p00/hf000/hf000-01-daily-m.csv"))

input.files <- get.input.files (prov, only.files=TRUE)
expect_equal (nrow (input.files), 1)
expect_equal (input.files$name, "x.csv")

## URLs
context ("URLs")
urls <- get.urls (prov)
expect_equal (nrow (urls), 1)
expect_equal (urls$name, "http://harvardforest.fas.harvard.edu/data/p00/hf000/hf000-01-daily-m.csv")

## Output files
context ("Output files")
output.files <- get.output.files (prov)
expect_equal (nrow (output.files), 2)
expect_setequal (output.files$name, c ("shortdata.csv", "airt-vs-prec.pdf"))

## Variables set
context ("Variables set")
variables.set <- get.variables.set (prov)
expect_equal (nrow (variables.set), 21)

## Variables used
context ("Variables used")
variables.used <- get.variables.used (prov)
expect_equal (nrow (variables.used), 16)

## Variables named
context("Variable named")
variables.named.z <- get.variable.named (prov, "z")
expect_equal (nrow (variables.named.z), 3)
variables.named.foo <- get.variable.named (prov, "foo")
expect_equal (nrow (variables.named.foo), 0)

## Standard Output
context("Standard output access")
stdout.df <- get.stdout.nodes(prov)
expect_equal(nrow(stdout.df), 2)

## If input is a string

context("String prov")
prov <- prov.parse(readLines(test.data), isFile = FALSE)

context("String prov - Environment access function")
envi.df <- get.environment(prov)
expect_match(class(envi.df), "data.frame")
expect_match(typeof(envi.df$value), "character")
expect_equal(nrow(envi.df), 15)
expect_equal(ncol(envi.df), 2)

context("String prov - Procedure nodes access function")
proc.df <- get.proc.nodes(prov)
expect_match(class(proc.df), "data.frame")
expect_match(typeof(proc.df$name), "character")
expect_match(typeof(proc.df$type), "character")
expect_match(typeof(proc.df$elapsedTime), "double")
expect_match(typeof(proc.df$scriptNum), "integer")
expect_match(typeof(proc.df$startLine), "integer")
expect_match(typeof(proc.df$startCol), "integer")
expect_match(typeof(proc.df$endLine), "integer")
expect_match(typeof(proc.df$endCol), "integer")
expect_equal(nrow(proc.df), 32)
expect_equal(ncol(proc.df), 9)


