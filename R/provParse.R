# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2014, 2015, 2016, 2017, 2018.

# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public
#   License along with this program.  If not, see
#   <http://www.gnu.org/licenses/>.

# Created by Orenna Brand & Joe Wonsil, summer 2018
# Updated by Barbara Lerner, fall 2018

methods::setClass("ProvInfo",
    slots = list(
        proc.nodes = "data.frame", 
        data.nodes = "data.frame", 
        func.nodes = "data.frame", 
        proc.proc.edges = "data.frame", 
        proc.data.edges = "data.frame", 
        data.proc.edges = "data.frame", 
        func.proc.edges = "data.frame", 
        func.lib.edges = "data.frame", 
        agents = "data.frame", 
        envi = "data.frame", 
        libs = "data.frame", 
        scripts = "data.frame")
)

# This is called when a new ProvInfo object is created.  It initializes all of the slots.
methods::setMethod ("initialize",
    "ProvInfo",
    function(.Object, prov){
      
      # Removing 'rdt:' prefix for legibility of data.
      if (length(prov) == 0) warning ("Provenance is empty")
      prov <- gsub("rdt:", "", prov)
      prov <- gsub("prov:", "", prov)
      
      # Converting to an R-useable data structure.
      prov.data <- jsonlite::fromJSON(prov)
      
      # Creating the master list by unlisting the nodes/edges.
      master.list <- unlist(prov.data, recursive = F)
      
      # This removes the appended prefixes created by unlisting.
      # This leaves the nodes with their original names.
      names(master.list) <- gsub("^.*\\.", "", names(master.list))
      
      
      # These nodes cannot be parsed with the generalized function.
      # Therefore they are done separately and appended later.
      .Object@envi <- parse.envi(master.list)
      .Object@libs <- parse.libs(master.list)
      .Object@scripts <- parse.scripts(master.list)
      
      # This list represents the characters codes for the different
      # possible objects.
      obj.chars <- c("p", "d", "f", "pp", "pd", "dp", "fp", "m", "a")
      
      # Utilizes char codes to produce the list of data frames.
      prov.df <- lapply(obj.chars, parse.general, m.list = master.list)
      
      .Object@proc.nodes <- prov.df[[1]]
      .Object@data.nodes <- prov.df[[2]]
      .Object@func.nodes <- prov.df[[3]]
      .Object@proc.proc.edges <- prov.df[[4]]
      .Object@proc.data.edges <- prov.df[[5]]
      .Object@data.proc.edges <- prov.df[[6]]
      .Object@func.proc.edges <- prov.df[[7]]
      .Object@func.lib.edges <- prov.df[[8]]
      .Object@agents <- prov.df[[9]]
      
      return (.Object)
    }
)
      

# Generalized parser
parse.general <- function(requested, m.list) {
  
  # Constructs pattern to match to using the grep function.
  grep.arg <- paste("^", requested, "[[:digit:]]", sep = "")
  
  # Using the pattern to pull out only the requested
  # nodes/edges from the master list.
  # This list had data stored in rows not columns
  nodes <- m.list[grep(grep.arg, names(m.list))]
  
  nodes.df <- data.frame(NULL)
  
  if(length(nodes) > 0) {
    # The num of columns are stored in each row in the list
    # Pull out how many columns so we can index through each
    # row and receive the columns
    col.length <- 1:length(nodes[[1]])
    
    # Use index nums to pull out each column so they are
    # no longer stored as rows but columns
    col.list <- lapply(col.length, function(x) {
      return(mapply(`[`, nodes, x))
    })
    
    # To each column, replace the string "NA" with
    # an actual R value of NA, the extract the column 
    # as a vector to coerce into one type
    node.vec <- lapply(col.length, function(x) {
      col <- mapply(`[`, nodes, x)
      col[col=="NA"] <- NA
      return(mapply(`[`, col, 1))
    })
    
    # Convert the data frame, we do not have factors
    # in data so keep them as strings
    nodes.df <- data.frame(node.vec, stringsAsFactors = F)
    colnames(nodes.df) <- names(nodes[[1]])
    nodes.df <- cbind.data.frame(names(nodes), nodes.df, stringsAsFactors = F)
    names(nodes.df)[names(nodes.df) == "names(nodes)"] <- "id"
    rownames(nodes.df) <- NULL
  }
  
  # Combine into a single data frame.
  return(nodes.df)
}

# Environment parser
parse.envi <- function(m.list) {
  
  env <- m.list$environment
  
  # Remove nested lists which are parsed in a different
  # function
  env <- env[-which(names(env) == "sourcedScripts")]
  env <- env[-which(names(env) == "sourcedScriptTimeStamps")]
  
  # Swap rows and columns for clarity and apply name the column
  environment <- t(as.data.frame(env))
  environment <- data.frame(environment, stringsAsFactors = F)
  colnames(environment) <- c("value")
  environment <- cbind.data.frame(rownames(environment), environment, stringsAsFactors = F)
  names(environment)[names(environment) == "rownames(environment)"] <- "label"
  rownames(environment) <- NULL
  
  return(environment)
}

# Libraries parser
parse.libs <- function(m.list) {
  # Use the general function, however it will 
  # add unneeded columns
  libraries <- parse.general("l", m.list)
  
  # Pull out columns of info wanted
  libraries <- libraries[,c("id", "name", "version")]
  return(libraries)
}

# Source scripts parser
parse.scripts <- function(m.list) {
  
  # The source scripts are nested in the environment object
  env <- m.list$environment
  
  # Put the main script in the table
  scripts.df <- data.frame (script = env$script[1], timestamp = env$scriptTimeStamp[1],
      stringsAsFactors = F)
  
  # Grab the sourced script names
  scripts <- env$`sourcedScripts`
  if (length(scripts) > 1) {
    # Append the sourced scripts
    scripts.df <- rbind (scripts.df, 
        cbind(script = scripts, timestamp = env$`sourcedScriptTimeStamps`))
    
  }
  return(scripts.df)
}

#' Provenance parser
#' 
#' The prov.parse function parses the provenance collected by rdt or rdtLite.  This 
#' provenance can be stored in a prov.json file or passed to prov.parse as a string.  
#' The provParseR package also defines
#' a number of functions that extract and return information from the parsed
#' provenance.
#'
#' @param prov.input A path to a json file that has been created by rdt
#'   or rdtLite or a string that is in prov.json format.
#' @param isFile A logical value that indicates whether the provenance information 
#'   is stored in a file (isFile=T) or in a string (isFile=F).
#' @return A ProvInfo object that can be passed to the access functions provided
#'   by the provParseR package.
#' @export
#' @examples
#' prov <- prov.parse(system.file ("testdata", "prov.json", package="provParseR", mustWork=TRUE))
#' @seealso The access functions, including \code{\link{get.environment}}
prov.parse <- function(prov.input, isFile = T) {

  # If the input is a string does not need to be read from the file system
  if (isFile) {
    prov <- readLines(prov.input)
  } else {
    prov <- prov.input
  }
  
  return (methods::new (Class = "ProvInfo", prov))
}

## ==Simple wrapper functions for calling data frames==##


    
#' Provenance access functions
#' 
#' These functions extract information from a ProvInfo object created by the prov.parse function 
#' and return this information as a data frame.
#' 
#' @param prov a ProvInfo object created by calling \code{\link{prov.parse}}.
#' 
#' @examples
#' prov <- prov.parse(system.file ("testdata", "prov.json", package="provParseR", mustWork=TRUE))
#' get.proc.nodes(prov)
#' get.input.files(prov)
#' get.output.files(prov)
#' get.variables.set(prov)
#' get.variables.used(prov)
#' get.data.nodes(prov)
#' get.func.nodes(prov)
#' get.proc.proc(prov)
#' get.data.proc(prov)
#' get.proc.data(prov)
#' get.func.proc(prov)
#' get.func.lib(prov)
#' get.libs(prov)
#' get.scripts(prov)
#' get.environment(prov)
#' 
#' @return All access functions return NULL if there is no parsed provenance.  If parsed provenance
#'   exists, but there is no provenance for the type of information requested, such as no input 
#'   files, an empty data frame is returned.
#' 
#' @seealso \code{\link{prov.parse}}

#' @return get.environment returns a data frame containing information about how the provenance was collected.
#'    The data frame has 2 columns:  label and value.  The labels are: 
#'    \itemize{
#'    \item {name} {- whose value will always be "environment"}
#'    \item {architecture}
#'    \item {operatingSystem}
#'    \item {language}
#'    \item {langVersion}
#'    \item {script} {- the absolute path to the script executed}
#'    \item {scriptTimeStamp} {- when the script was last modified}
#'    \item {workingDirectory}
#'    \item {provDirectory} {- where the provenance is stored}
#'    \item {provTimeStamp} {- when the provenance was collected}
#'    \item {hashAlgorithm}
#'    }
#' @rdname access
#' @export
get.environment <- function(prov) {
  return(if (!is.null(prov)) {
    prov@envi
  } else {
    NULL
  })
}

#' @return get.libs returns a data frame describing the libraries used by the 
#'   script.  It contains 3 columns:  id, name, and version.
#' @rdname access
#' @export
get.libs <- function(prov) {
  return(if (!is.null(prov)) {
    prov@libs
  } else {
    NULL
  })
}

#' @return get.tool.info returns a data frame describing the tool that 
#'   collected the provenance.  It contains 3 columns:  tool.name, tool.version
#'   and json.version.  
#' @rdname access
#' @export
get.tool.info <- function(prov) {
  return(if (!is.null(prov)) {
            prov@agents[c("tool.name", "tool.version", "json.version")]
          } else {
            NULL
          })
}

#' @return get.scripts returns a data frame identifying all the scripts executed.  The main script
#'    will be first, followed by all sourced scripts.  The data frame contains 
#'    2 columns:  name and timestamp (when the script was last modified).  
#' @rdname access
#' @export
get.scripts <- function(prov) {
  return(if (!is.null(prov)) {
    prov@scripts
  } else {
    NULL
  })
}

#' @return get.scripts returns a data frame identifying the location of saved copies
#'    of all the scripts executed.  The main script
#'    will be first, followed by all sourced scripts.  The data frame contains 
#'    2 columns:  name and timestamp (when the script was last modified).  
#' @rdname access
#' @export
get.saved.scripts <- function (prov) {
  scripts <- get.scripts(prov)
  env <- get.environment(prov)
  prov.dir <- env[env$label == "ddgDirectory", ]$value
  names <- paste0 (prov.dir, "/scripts/", basename (scripts$script))
  return (data.frame (script = names, timestamp = scripts$timestamp, stringsAsFactors=FALSE))
}

#' @return get.proc.nodes returns a data frame identifying all the procedural nodes executed.  
#'    These are represented in PROV-JSON as activities and include nodes
#'    corresponding to lines of code, start or finish nodes that surround
#'    blocks of code, and nodes to represent the binding of function arguments
#'    to parameters.  The data frame contains 
#'    8 columns:  
#'    \itemize{
#'      \item{id} {- a unique id}
#'      \item{name} {- a description of what the node represents.  Often this is a line of code from
#'        the script, perhaps shortened}
#'      \item{type} {- one of Operation, Binding, Start, Finish, or Incomplete}
#'      \item{elapsedTime} {- when this executed relative to the start of the script}
#'      \item {scriptNum} {- a number identifing the script it comes from, with script 1 being the main
#'        script}
#'      \item {startLine} {- the line in the script this corresponds to, which may be NA, and the following
#'        other position infofmation}
#'      \item {startCol}
#'      \item {endLine}
#'      \item {endCol}
#'    }
#' @rdname access
#' @export
get.proc.nodes <- function(prov) {
  return(if (!is.null(prov)) {
    prov@proc.nodes
  } else {
    NULL
  })
}

#' @return get.data.nodes returns a data frame with an entry for each data node
#'   in the provenance.  The data frame contains the following columns:
#'   \itemize{
#'      \item {id} {- a unique id}
#' 			\item {name} {- the descriptive name for the node, which is generally a variable name, file name, or URL}
#' 			\item {value} {- either a text value (possible shortened) or the name of a file where the value is stored}
#' 			\item {valType} {- a description of the value's type, including its container (such as list, vector, etc.), 
#'         dimensions and member types (such as character, numeric, etc.)}
#' 			\item {type} {- the type of the node, one of Data, Snapshot, File, URL, Exception, or Device}
#' 			\item {scope} {- a hex number identifying the scope.  This is only used for node's with type Data or Snapshot}
#' 			\item {fromEnv} {- a logical value.  If true, it means the variable had a value before the script began execution}
#' 			\item {hash} {- the hash value for File nodes}
#' 			\item {timestamp} {- the time at which the node was created}
#' 			\item {location} {- for file nodes, the absolute path to the file}

#'   }
#' @rdname access
#' @export
get.data.nodes <- function(prov) {
  return(if (!is.null(prov)) {
    prov@data.nodes
  } else {
    NULL
  })
}

#' @return get.func.nodes returns a data frame containing information about the functions
#'   used from other libraries within the script.  The data frame has 2 columns:  id 
#'   (a unique id) and name (the name of the function called).  
#' @rdname access
#' @export
get.func.nodes <- function(prov) {
  return(if (!is.null(prov)) {
    prov@func.nodes
  } else {
    NULL
  })
}

#' @return get.proc.proc returns a data frame containing information about the edges
#'   that go between two procedural nodes.  These edges indicate a control-flow relationship
#'   between the two activities.  The data frame has 3 columns:  id 
#'   (a unique id), informant (the tail of the edge), and informed (the head of the edge).  
#' @rdname access
#' @export
get.proc.proc <- function(prov) {
  return(if (!is.null(prov)) {
    prov@proc.proc.edges
  } else {
    NULL
  })
}

#' @return get.data.proc returns a data frame containing information about the edges
#'   that go from data nodes to procedural nodes.  These edges indicate an input relationship
#'   where the data is used by the activity.  The data frame has 3 columns:  id 
#'   (a unique id), entity (the input data), and activity (the procedural node that uses the
#'   data).  
#' @rdname access
#' @export
get.data.proc <- function(prov) {
  return(if (!is.null(prov)) {
    prov@data.proc.edges
  } else {
    NULL
  })
}

#' @return get.proc.data returns a data frame containing information about the edges
#'   that go from procedural nodes to data nodes.  These edges indicate an output relationship
#'   where the data is produed by the activity.  The data frame has 3 columns:  id 
#'   (a unique id), entity (the output data), and activity (the procedural node that produces the
#'   data).  
#' @rdname access
#' @export
get.proc.data <- function(prov) {
  return(if (!is.null(prov)) {
    prov@proc.data.edges
  } else {
    NULL
  })
}

#' @return get.proc.func returns a data frame containing information about where externally-defined
#'   functions are used in the script.  The data frame has 3 columns:  func_id (the id of the
#'   function node), activity (the procedural node 
#'   that calls the function) and function (the function's name).  
#' @rdname access
#' @export
get.func.proc <- function(prov) {
  if (is.null (prov)) return (NULL)
  
  else {
    func.proc.edges <- prov@func.proc.edges
    if (nrow (func.proc.edges) == 0) return (data.frame("func_id"=vector(), "function"=vector(), "activity"=vector()))
    func.nodes <- get.func.nodes (prov)
    func.proc.df <- merge (func.proc.edges, func.nodes, by.x = "entity", by.y = "id")
    colnames(func.proc.df)[1] <- "func_id"
    colnames(func.proc.df)[4] <- "function"
    return (func.proc.df[c("func_id", "function", "activity")])
  } 
}

#' @return get.func.lib returns a data frame containing information about what
#'   libraries externally-defined
#'   functions come from.  The data frame has 3 columns:  func_id (the id of the
#'   function node), library (a library node)
#'   and function (the name of a function).  
#' @rdname access
#' @export
get.func.lib <- function(prov) {
  if (is.null (prov)) return (NULL)
  
  else {
    func.lib.edges <- prov@func.lib.edges
    if (nrow (func.lib.edges) == 0) return (data.frame("func_id"=vector(), "library"=vector(), "function"=vector()))
    func.nodes <- get.func.nodes (prov)
    func.lib.df <- merge (func.lib.edges, func.nodes, by.x = "entity", by.y = "id")
    colnames(func.lib.df)[1] <- "func_id"
    colnames(func.lib.df)[3] <- "library"
    colnames(func.lib.df)[4] <- "function"
    return (func.lib.df[c("func_id", "function", "library")])
  } 
}

#' @rdname access
#' @return get.input.files returns a data frame containing a subset of the data nodes that correspond to files or URLs that are 
#'   read by the script.  
#' @export
get.input.files <- function (prov) {
  data.nodes <- get.data.nodes(prov)
  if (is.null (data.nodes)) return (NULL)
  
  file.nodes <- data.nodes[data.nodes$type %in% c ("File", "URL"), ]
  if (nrow (file.nodes) == 0) {
    return (file.nodes)
  }
  
  input.data <- get.data.proc(prov)$entity
  input.files <- file.nodes[file.nodes$id %in% input.data, ]
  return (input.files)
}

#' @rdname access
#' @return get.output.files returns a data frame containing a subset of the data nodes that correspond to files that are 
#'   written by the script.  
#' @export
get.output.files <- function (prov) {
  data.nodes <- get.data.nodes(prov)
  if (is.null (data.nodes)) return (NULL)
  
  file.nodes <- data.nodes[data.nodes$type == "File", ]
  if (nrow (file.nodes) == 0) {
    return (file.nodes)
  }
  
  output.data <- get.proc.data(prov)$entity
  output.files <- file.nodes[file.nodes$id %in% output.data, ]
  return (output.files)
}

#' @rdname access
#' @return get.variables.set returns a data frame containing a subset of the data nodes that correspond to variables
#'   assigned to in the script.  
#' @export
get.variables.set <- function (prov) {
  data.nodes <- get.data.nodes(prov)
  if (is.null (data.nodes)) return (NULL)
  
  data.nodes <- data.nodes[data.nodes$type %in% c ("Data", "Snapshot"), ]
  if (nrow (data.nodes) == 0) {
    return (data.nodes)
  }
  
  output.data <- get.proc.data(prov)$entity
  variables.set <- data.nodes[data.nodes$id %in% output.data, ]
  return (variables.set)
}

#' @rdname access
#' @return get.variables.used returns a data frame containing a subset of the data nodes that correspond to variables
#'   whose values are used in the script.  
#' @export
get.variables.used <- function (prov) {
  data.nodes <- get.data.nodes(prov)
  if (is.null (data.nodes)) return (NULL)
  
  data.nodes <- data.nodes[data.nodes$type %in% c ("Data", "Snapshot"), ]
  if (nrow (data.nodes) == 0) {
    return (data.nodes)
  }
  
  input.data <- get.data.proc(prov)$entity
  variables.used <- data.nodes[data.nodes$id %in% input.data, ]
  return (variables.used)
}

## ====##
