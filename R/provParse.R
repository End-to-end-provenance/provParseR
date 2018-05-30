# provParse.R 
# Orenna Brand & Joe Wonsil

# Generalized parser
parse.general <- function(requested) {
  
  # Constructs pattern to match to using the grep function.
  grep.arg <- paste("^", requested, "[[:digit:]]", sep = "")
  
  # Using the pattern to pull out only the requested nodes/edges
  # from the master list.
  nodes <- master.list[grep(grep.arg, names(master.list))]
  
  # Combine into a single data frame.
  return(do.call(rbind.data.frame, nodes))
}

# Environment parser
parse.envi <- function() {
  
  env <- master.list$'environment'
  
  # Remove nested lists which are parsed in a different function
  env <- env[ - which(names(env) == "sourcedScripts")]
  env <- env[ - which(names(env) == "sourcedScriptTimeStamps")]
  
  # Swap rows and columns for clarity and apply name the column
  environment <- t(as.data.frame(env))
  colnames(environment) <- c("Value")
  
  return(data.frame(environment))
}

# Libraries parser
parse.libs <- function() {
  
  # Locate all the library nodes, all start with "l"
  libraries <- master.list[grep("^l", names(master.list))]
  
  # Combine the libraries into a data frame to return to the user
  libraries <- data.frame(do.call(rbind, libraries))
  
  # Remove unnecessary data (is it? TODO: determine if we keep this step)
  libraries <- libraries[ - which(names(libraries) == "prov.type")]
  
  return(libraries)
}

# Source scripts parser
parse.scripts <- function() {
  
  # The source scripts are nested in the environment object
  env <- master.list$'environment'
  
  # Grab the script names 
  scripts <- env$`rdt:sourcedScripts`
  
  # Append the script time stamps to the end 
  scripts <- as.data.frame(cbind(scripts, env$`rdt:sourcedScriptTimeStamps`))
  
  # If there are scripts, append names to the data frame
  if(length(scripts) > 0) names(scripts) <- c("Scripts", "Timestamps")
  
  return(scripts)
}

##==Simple wrapper functions for calling data frames==##

get.environment <- function() {
  return(parse.envi())
}

get.proc.nodes <- function() {
  return(parse.general("p"))
}
  
get.data.nodes <- function() {
  return(parse.general("d"))
}

get.func.nodes <- function() {
  return(parse.general("f"))
}

get.proc.proc <- function() {
  return(parse.general("pp"))

get.data.proc <- function() {
  return(parse.general("dp"))
}

get.proc.data <- function() {
  return(parse.general("pd"))
}

get.func.proc <- function() {
  return(parse.general("fp"))
}

get.func.lib <- function() {
  return(parse.general("m"))
}

get.libs <- function() {
  return(parse.libs())
}

get.scripts <- function() {
  return(parse.scripts())
}

##====##

prov.parse <- function(filename, retList = T) {
  library("jsonlite")
  
  # Removing "rdt:" prefix for legibility of data.
  prov <- readLines(filename)
  prov <- gsub("rdt:", "", prov)
  
  # Converting to an R-useable data structure.
  prov.data <- fromJSON(prov)
  
  # Creating the master list by unlisting the nodes/edges.
  master.list <- unlist(prov.data, recursive = F)
  
  # This removes the appended prefixes created by unlisting.
  # This leaves the nodes with their original names.
  names(master.list) <- gsub("^.*\\.","", names(master.list))
  
  assign("master.list", master.list, envir = .GlobalEnv, )
  
  # These nodes cannot be parsed with the generalized function.
  # Therefore they are done separately and appended later.
  envi.df <- parse.envi()
  lib.df <- parse.libs()
  scr.df <- parse.scripts()
  
  # This list represents the characters codes for the different possible objects. 
  obj.chars <- c("p", "d", "f", "pp", "pd", "dp", "fp", "m")
  
  # Utilizes char codes to produce the list of data frames.
  obj.df <- lapply(obj.chars, parse.general)
  
  names(obj.df) <- c("procNodes", "dataNodes", "funcNodes", "procProcEdges", 
                     "procDataEdges", "dataProcEdges", "funcProcEdges", "funcLibEdges")
  
  # Appending hard-coded data to list of data frames.
  obj.df[["envi"]] <- envi.df
  obj.df[["libs"]] <- lib.df
  obj.df[["scripts"]] <- scr.df

  if(retList) return(obj.df[])
}
