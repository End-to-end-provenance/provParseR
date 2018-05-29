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
  colnames(lib.df) <- c("Name", "Version")
  return(data.frame(lib.df))
}

parseDataNodes <- function(prov.data) {
  
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
