parseEnvi <- function(prov.data) {
  env <- prov.data$entity$environment
  env <- env[ - which(names(env) == "rdt:sourcedScripts")]
  env <- env[ - which(names(env) == "rdt:sourcedScriptTimeStamps")]
  
  environment <- t(as.data.frame(env))
  colnames(environment) <- c("Value")
  return(data.frame(environment))
}

prov.parse <- function(filename) {
  library("jsonlite")
  
  prov.data <- fromJSON(file)
  
  envi.df <- parseEnvi(prov.data)
  libr.df <- parseLibs(prov.data)
}

