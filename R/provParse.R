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

#Source Scripts
parse.scripts <- function(prov.data) {
  env <- prov.data$entity$environment
  scripts <-env$`rdt:sourcedScripts`
  scripts <- as.data.frame(cbind(scripts, env$`rdt:sourcedScriptTimeStamps`))
  if(length(scripts) > 0) names(scripts) <- c("Scripts", "Timestamps")
  return(scripts)
}

#generalized parser
parse.general <- function(m.list, requested) {
  grep.arg <- paste("^",requested,"[[:digit:]]", sep = "")
  nodes <- m.list[grep(grep.arg, names(m.list))]
  return(do.call(rbind.data.frame, nodes))
}

prov.parse <- function(filename) {
  library("jsonlite")
  
  prov <- readLines(filename)
  prov <- gsub("rdt:", "", prov)
  
  prov.data <- fromJSON(prov)
  
  master.list <- unlist(prov.data, recursive = F)
  names(master.list) <- gsub("^.*\\.","", names(master.list))
 
  envi.df <- parse.envi(prov.data)
  lib.df <- parse.libs(prov.data)
  scr.df <- parse.scripts(prov.data)
  
  obj.chars <- c("p", "d", "f", "pp", "pd", "dp", "fp", "m")
  obj.df <- lapply(obj.chars, parse.general, m.list = master.list)
  names(obj.df) <- c("procNodes", "dataNodes", "funcNodes", "procProcEdges","ProcDataEdges",
                     "dataProcEdges", "FuncProcEdges", "FuncLibEdges")
  obj.df[["envi"]] <- envi.df
  obj.df[["libs"]] <- lib.df
  obj.df[["scripts"]] <- scr.df

  return(obj.df)
}

