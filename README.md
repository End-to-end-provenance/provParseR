[![Travis-CI Build Status](https://travis-ci.org/ProvTools/provParseR.svg?branch=master)](https://travis-ci.org/ProvTools/provParseR)

# provParseR

Parses the provenance collected by rdtLite or rdt and returns selected provenance as an R data frame.


# Installation
Install from GitHub:
```{r}
# install.packages("devtools")
devtools::install_github("ProvTools/provParseR")
```
Once installed, load the package:
```{r}
library("provParseR")
```


# Usage
The prov.parse function parses the prov.json file or string created by rdtLite or rdt and returns a ProvInfo object. This object can then be queried to return a data frame containing the desired values. For example:

```{r}
prov <- prov.parse("c:/prov/prov.json")
data.nodes <- get.data.nodes(prov)
```
creates a ProvInfo object "prov" (where the path to the provenance file is "c:/prov/prov.json") and a data frame "data.nodes" that contains all data nodes in the provenance graph.

The following access functions return data frames:

```{r}
ENVIRONMENT

# Computing environment
get.environment()

# Libraries used
get.libs()

# Provenance collection tool
get.tool.info()

SCRIPTS

# Scripts executed
get.scripts()

# Location of saved scripts
get.saved.scripts()

NODES

# Procedure nodes
get.proc.nodes()

# Data nodes
get.data.nodes()

# Function nodes
get.func.nodes()

EDGES

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

INPUT/OUTPUT

# Files read
get.input.files()

# Files written
get.output.files()

# URLs read
get.urls()

# Standard output
get.stdout.nodes()

VARIABLES

# Variable data type
get.val.type()

# Variables with specified name
get.variable.named()

# Variables assigned
get.variables.set()

# Variables used
get.variables.used()

```
