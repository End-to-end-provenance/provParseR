parseEnvi <- function(prov.data) {
  env <- prov.data$entity$environment
  env <- env[ - which(names(env) == "sourcedScripts")]
  env <- env[ - which(names(env) == "sourcedScriptTimeStamps")]
  
  environment <- t(as.data.frame(env))
  colnames(environment) <- c("Value")
  return(data.frame(environment))
}

parseLibs <- function(prov.data) {
  # libraries
  libraries <- prov.data$entity[grep("^l", names(prov.data$entity))]
  
  parseRows <- function(x) {
    return(c(x[[1]], x[[2]]))
  }
  
  lib.df <- t(as.data.frame(sapply(libraries, parseRows)))
  colnames(lib.df) <- c("name", "version")
  return(data.frame(lib.df))
}

parseDataNodes <- function(prov.data) {
  # data nodes
  data.nodes <- prov.data$entity[grep("^d", names(prov.data$entity))]
  
  remove.valType <- function(dn) {
    dn <- dn[ - which(names(dn) == "valType")]
  }
  
  data.nodes <- t(sapply(data.nodes, remove.valType))
  data.nodes <- data.frame(data.nodes)
  return(data.nodes)
}

parseProcNodes <- function(prov.data) {
  proc.nodes <- prov.data$activity[grep("^p", names(prov.data$activity))]
  parseRows <- function(x) {
    return(x)
  }
  
  rows <- as.data.frame(t(sapply(proc.nodes, parseRows)))
  return(rows)
}

prov.parse <- function(filename) {
  library("jsonlite")
  
  prov <- readLines(filename)
  prov <- gsub("rdt:", "", prov)
  
  prov.data <- fromJSON(prov)
  
  envi.df <- parseEnvi(prov.data)
  lib.df <- parseLibs(prov.data)
  dnodes.df <- parseDataNodes(prov.data)
  pnodes.df <- parseProcNodes(prov.data)
}

