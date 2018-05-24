eParse <- function(file) {
  library("jsonlite")
  
  prov.data <- fromJSON(file)
  
  # environment
  environment <- t(as.data.frame(prov.data$entity$environment))
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
