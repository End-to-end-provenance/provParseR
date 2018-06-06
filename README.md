[![Travis-CI Build Status](https://travis-ci.org/ProvTools/provParseR.svg?branch=master)](https://travis-ci.org/ProvTools/provParseR)

# provParseR
Given prov JSON files, returns details of the user's computing environment, including versions of all libraries used. For R scripts.

This R package can be used with RDataTracker.


# Installation
Install from GitHub:
```{r}
# install.packages("devtools")
devtools::install_github("provtools/provParseR")
```
Once installed, load the package:
```{r}
library("provParseR")
```


# Usage
Once you've generated a Data Derivation Graph (DDG) with RDataTracker and set the working directory, enter the following command, where ddg.json is a PROV-JSON file in the working directory:
```{r}
prov.parse("ddg.json")
```
provParse will parse the JSON file, generating data frames for each type of node and edge in the DDG, and for the computing environment, libraries used, and source scripts used. To access each data frame, enter the respective command:

```{r}
# Computing environment
get.environment()

# Libraries used
get.libs()

# Source scripts used
get.scripts()

# Procedure nodes
get.proc.nodes()

# Data nodes
get.data.nodes()

# Function nodes
get.func.nodes()

# Procedure-to-procedure edges
get.proc.proc()

# Data-to-procedure edges
get.data.proc()

# Procedure-to-data edges
get.proc.data()

# Function-to-procedure edges
get.func.proc()

# Function-library edges
get.func.lib()
```
