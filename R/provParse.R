parseEnvi <- function(prov.data) {
  env <- prov.data$entity$environment
  env <- env[ - which(names(env) == "sourcedScripts")]
  env <- env[ - which(names(env) == "sourcedScriptTimeStamps")]
  
  environment <- t(as.data.frame(env))
  colnames(environment) <- c("Value")
  return(data.frame(environment))
}

parseLibs <- function(prov.data) {
  #libraries
  libraries <- prov.data$entity[grep("^l", names(prov.data$entity))]
  libraries <- data.frame(do.call(rbind, libraries))
  libraries <- libraries[ - which(names(libraries) == "prov.type")]

  return(libraries)
}

parseDataNodes <- function(prov.data) {
  data.nodes <- prov.data$entity[grep("^d", names(prov.data$entity))]
  return(do.call(rbind.data.frame, data.nodes))
}

parseProcNodes <- function(prov.data) {
  proc.nodes <- prov.data$activity[grep("^p", names(prov.data$activity))]
  return(do.call(rbind, proc.nodes))
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

