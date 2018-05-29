eParse <- function(file) {
  library("jsonlite")
  
  prov.data <- fromJSON(file)

  env <- prov.data$entity$environment
  
  # Source Scripts
  scripts <-env$`rdt:sourcedScripts`
  scripts <- as.data.frame(cbind(scripts, env$`rdt:sourcedScriptTimeStamps`))
  names(scripts) <- c("Scripts", "Timestamps")
  write.csv(scripts,"scripts.csv")

  # Environment
  env <- env[ - which(names(env) == "rdt:sourcedScripts")]
  env <- env[ - which(names(env) == "rdt:sourcedScriptTimeStamps")]
  
  environment <- t(as.data.frame(env))
  colnames(environment) <- c("Value")
  write.csv(environment, "environment.csv")
  

  # libraries
  libraries <- prov.data$entity[grep("^l", names(prov.data$entity))]
  
  parseRows <- function(x) {
    return(c(x[[1]], x[[2]]))
  }
  
  lib.df <- t(as.data.frame(sapply(libraries, parseRows)))
  colnames(lib.df) <- c("name", "version")
  write.csv(lib.df, "libraries.csv")
}
