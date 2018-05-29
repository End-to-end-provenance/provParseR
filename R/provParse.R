parseEnvi <- function(prov.data) {
  env <- prov.data$entity$environment
  env <- env[ - which(names(env) == "rdt:sourcedScripts")]
  env <- env[ - which(names(env) == "rdt:sourcedScriptTimeStamps")]
  
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
  
}

prov.parse <- function(filename) {
  library("jsonlite")
  
  prov.data <- fromJSON(file)
  
  envi.df <- parseEnvi(prov.data)
  lib.df <- parseLibs(prov.data)
  dnodes.df <- 
}

