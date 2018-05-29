# environment
parse.envi <- function(prov.data) {
  env <- prov.data$entity$environment
  env <- env[ - which(names(env) == "sourcedScripts")]
  env <- env[ - which(names(env) == "sourcedScriptTimeStamps")]
  
  environment <- t(as.data.frame(env))
  colnames(environment) <- c("Value")
  
  return(data.frame(environment))
}

# libraries
parse.libs <- function(prov.data) {
  libraries <- prov.data$entity[grep("^l", names(prov.data$entity))]
  libraries <- data.frame(do.call(rbind, libraries))
  libraries <- libraries[ - which(names(libraries) == "prov.type")]
  
  return(libraries)
}

# data nodes
parse.data.nodes <- function(prov.data) {
  data.nodes <- prov.data$entity[grep("^d", names(prov.data$entity))]
  
  return(do.call(rbind.data.frame, data.nodes))
}

# procedure nodes
parse.proc.nodes <- function(prov.data) {
  proc.nodes <- prov.data$activity[grep("^p", names(prov.data$activity))]
  
  return(do.call(rbind, proc.nodes))
}

# function-library edges
parse.func.lib.edges <- function(prov.data) {
  func.lib.edges <- prov.data$hadMember[grep("^m", names(prov.data$hadMember))]
  
  return(do.call(rbind, func.lib.edges))
}

# functions
parse.functions <- function(prov.data) {
  func.nodes <- prov.data$entity[grep("^f", names(prov.data$entity))]
  
  return(do.call(rbind.data.frame,func.nodes))
}

prov.parse <- function(filename) {
  library("jsonlite")
  
  prov <- readLines(filename)
  prov <- gsub("rdt:", "", prov)
  
  prov.data <- fromJSON(prov)
  
  func.lib.df <- parse.func.lib.edges(prov.data)
  func.df <- parse.functions(prov.data)
  envi.df <- parse.envi(prov.data)
  lib.df <- parse.libs(prov.data)
  dnodes.df <- parse.data.nodes(prov.data)
  pnodes.df <- parse.proc.nodes(prov.data)
}

