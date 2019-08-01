# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2018, 2019.

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

#' Collection of information gathered from parsing a PROV file
#' 
#' This is the class that stores provenance information.  It is created by 
#' prov.parse.  Rather than access the slots directly, it is better to use
#' the access functions the package provides.
#' 
#' @slot proc.nodes the procedure nodes
#' @slot data.nodes the data nodes
#' @slot func.nodes the function nodes
#' @slot proc.proc.edges control flow edges
#' @slot proc.data.edges output data edges
#' @slot data.proc.edges input data edges
#' @slot func.proc.edges function use edges
#' @slot func.lib.edges function library edges
#' @slot agents tool that created the provenance
#' @slot envi environmental information
#' @slot libs libraries
#' @slot scripts scripts executed 
#' 
#' @seealso The parse function, which creates the ProvInfo object, \code{\link{prov.parse}}
#' @seealso The access functions, including \code{\link{get.environment}}
#' @import methods
#' @exportClass ProvInfo
ProvInfo <- methods::setClass("ProvInfo",
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
      
      # Check the type of the elapsedTime column.
      # Convert to a column of doubles if it is a column of strings.
      if(length(.Object@proc.nodes) > 0) {
        elapsedTime <- .Object@proc.nodes["elapsedTime"][ , 1]
        
        if(typeof(elapsedTime) == "character") {
          elapsedTime <- parse.elapsed.time(elapsedTime)
          .Object@proc.nodes["elapsedTime"] <- elapsedTime
        }
      }
      
      # Complete
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
  
  if (length(nodes) == 0) return(data.frame())
  
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
  
  if (length(scripts) > 0 && scripts[1] != "") {
    # Append the sourced scripts
    scripts.df <- rbind (scripts.df, 
        cbind(script = scripts, timestamp = env$`sourcedScriptTimeStamps`))
  }
  
  return(scripts.df)
}

# Parse a vector of elapsedTime values from strings to a vector of doubles.
# elapsedTime strings can have ',' and/or '.' for digit grouping and/or as a decimal separator.
# elapsedTime values will always have at least a decimal separator.
parse.elapsed.time <- function(vector) {
	
	vector <- sapply(
		vector,
		function(str) {
			# Try to parse the string normally as a double.
			# If and when it fails, a warning will be thrown and NA will be returned.
			val <- suppressWarnings(as.double(str))
			
			# If parsing fails, manipulate the string into a format where it will parse.
			# e.g. When ',' and/or '.' are used to group digits or as a decimal separator
			if(is.na(val[1])) {
				
				# split string into array where ',' or '.' occurs
				# there will always be at least a decimal separator
				regex <- '(,|\\.)'
				parts <- strsplit(str, regex)[[1]]
				
				# as the last part is always the part after the decimal separator,
				# add a '.' before it before combining the parts back into a single string
				parts[length(parts)] <- paste('.', parts[length(parts)], sep='')
				str <- paste(parts, collapse='')
				
				val <- as.double(str)
			}
			
			return(val)
		})
	
	return(unname(vector))
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
#' @param only.files If true, the output of get.input.files contains just files.  If false,
#'    it contains both files and URLs.
#' @param var.name a string containing the name of a variable used in the script the
#'  provenance is for
#' 
#' @examples
#' prov <- prov.parse(system.file ("testdata", "prov.json", package="provParseR", mustWork=TRUE))
#' get.proc.nodes(prov)
#' get.input.files(prov)
#' get.urls(prov)
#' get.output.files(prov)
#' get.variables.set(prov)
#' get.variables.used(prov)
#' get.variable.named(prov, "z")
#' get.data.nodes(prov)
#' get.error.nodes(prov)
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

#' @return get.saved.scripts returns a data frame identifying the location of saved copies
#'    of all the scripts executed.  The main script
#'    will be first, followed by all sourced scripts.  The data frame contains 
#'    2 columns:  name and timestamp (when the script was last modified).  
#' @rdname access
#' @export
get.saved.scripts <- function (prov) {
  scripts <- get.scripts(prov)
  env <- get.environment(prov)
  prov.dir <- env[env$label == "provDirectory", ]$value
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
  if (!is.null(prov)) {
    proc.nodes <- prov@proc.nodes
    if (ncol(proc.nodes) == 0) {
      #   id      name      type elapsedTime scriptNum startLine startCol endLine endCol
      # 1 p1 Issue10.R     Start       0.441        NA        NA       NA      NA     NA
      proc.nodes <- data.frame (id=character(), name=character(), type=character(), 
          elapsedTime=character(), scriptNum=integer(), startLine=integer(), 
          startCol=integer(), endLine=integer(), endCol=integer(), stringsAsFactors=FALSE)
    }
    return (proc.nodes)
  } else {
    return (NULL)
  }
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
  if (!is.null(prov)) {
    data.nodes <- prov@data.nodes
    if (ncol (data.nodes) == 0) {
      #   id  name   value    valType                                                 type   scope
      # 1 d1   y       5  {"container":"vector", "dimension":[1], "type":["numeric"]} Data R_GlobalEnv
            
      # fromEnv      hash    timestamp   location
      #  FALSE
      
      data.nodes <- data.frame (id=character(), name=character(), value=character(), valType=character(),
          type=character(), scope=character(), fromEnv=logical(), hash=character(), timestamp=character(), 
          location=character(), stringsAsFactors=FALSE)
    }
    return (data.nodes)
    
  } else {
    return (NULL)
  }
}

#' @return get.error.nodes returns a data frame with an entry for each error node
#'   in the provenance.  The data frame contains the following columns:
#'   \itemize{
#'      \item {id} {- a unique id}
#' 			\item {value} {- either a text value (possible shortened) or the name of a file where the value is stored}
#' 			\item {timestamp} {- the time at which the node was created}
#'   }
#' @rdname access
#' @export
get.error.nodes <- function(prov) {
  data.nodes <- get.data.nodes(prov)
  error.nodes <- data.nodes[data.nodes$type=="Exception",]
  error.table <- subset (error.nodes, select=c("id", "value", "timestamp"))
  return (error.table)
}
  
#' @return get.func.nodes returns a data frame containing information about the functions
#'   used from other libraries within the script.  The data frame has 2 columns:  id 
#'   (a unique id) and name (the name of the function called).  
#' @rdname access
#' @export
get.func.nodes <- function(prov) {
  if (!is.null(prov)) {
    func.nodes <- prov@func.nodes
    if (ncol(func.nodes) == 0) {
      #   id    name
      # 1 f1 str_to_upper
      func.nodes <- data.frame(id=character(), name=character(), stringsAsFactors=FALSE)
    }
    return(func.nodes)
  } else {
    return (NULL)
  }
}

#' @return get.proc.proc returns a data frame containing information about the edges
#'   that go between two procedural nodes.  These edges indicate a control-flow relationship
#'   between the two activities.  The data frame has 3 columns:  id 
#'   (a unique id), informant (the tail of the edge), and informed (the head of the edge).  
#' @rdname access
#' @export
get.proc.proc <- function(prov) {
  if (!is.null(prov)) {
    proc.proc.edges <- prov@proc.proc.edges
    if (ncol(proc.proc.edges) == 0) {
        #     id  informant informed
        # 1   pp1    p1       p2
      proc.proc.edges <- data.frame (id=character(), informant=character(), 
          informed=character(), stringsAsFactors=FALSE)
    }
    return (proc.proc.edges)
    
  } else {
    return (NULL)
  }
}

#' @return get.data.proc returns a data frame containing information about the edges
#'   that go from data nodes to procedural nodes.  These edges indicate an input relationship
#'   where the data is used by the activity.  The data frame has 3 columns:  id 
#'   (a unique id), entity (the input data), and activity (the procedural node that uses the
#'   data).  
#' @rdname access
#' @export
get.data.proc <- function(prov) {
  if (!is.null(prov)) {
    data.proc.edges <- prov@data.proc.edges
    if (ncol (data.proc.edges) == 0) {
      #     id    entity  activity
      # 1   dp1     d1       p7
      data.proc.edges <- data.frame (id=character(), entity=character(), 
          activity=character(), stringsAsFactors=FALSE)
    }
    return (data.proc.edges)
  } else {
    return (NULL)
  }
}

#' @return get.proc.data returns a data frame containing information about the edges
#'   that go from procedural nodes to data nodes.  These edges indicate an output relationship
#'   where the data is produed by the activity.  The data frame has 3 columns:  id 
#'   (a unique id), entity (the output data), and activity (the procedural node that produces the
#'   data).  
#' @rdname access
#' @export
get.proc.data <- function(prov) {
  if (!is.null(prov)) {
    proc.data.edges <- prov@proc.data.edges
    if (ncol(proc.data.edges) == 0) {
      #     id  activity entity
      # 1   pd1   p6       d1
      proc.data.edges <- data.frame (id=character(), activity=character(), 
          entity=character(), stringsAsFactors=FALSE)
    }
    return (proc.data.edges)
  } else {
    return (NULL)
  }
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
#' @return get.input.files returns a data frame containing a subset of the data nodes that correspond to files that are 
#'   read by the script.  If only.files is False, the data frame contains information about both input files and URLs.
#' @export
get.input.files <- function (prov, only.files=FALSE) {
  data.nodes <- get.data.nodes(prov)
  if (is.null (data.nodes)) return (NULL)
  
  if (only.files) {
    file.nodes <- data.nodes[data.nodes$type == "File", ]
  }
  else {
    file.nodes <- data.nodes[data.nodes$type %in% c("File","URL"), ]
  }
  if (nrow (file.nodes) == 0) {
    return (file.nodes)
  }
  
  input.data <- get.data.proc(prov)$entity
  input.files <- file.nodes[file.nodes$id %in% input.data, ]
  return (input.files)
}

#' @rdname access
#' @return get.urls returns a data frame containing a subset of the data nodes that correspond to urls used 
#'   in the script.  
#' @export
get.urls <- function (prov) {
  data.nodes <- get.data.nodes(prov)
  if (is.null (data.nodes)) return (NULL)
  
  url.nodes <- data.nodes[data.nodes$type == "URL", ]
  return (url.nodes)
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

#' @rdname access
#' @return get.variable.named returns a data frame containing a subset of the data nodes that correspond to variables
#'   with the specified name.  
#' @export
get.variable.named <- function (prov, var.name) {
  data.nodes <- get.data.nodes(prov)
  if (is.null (data.nodes)) return (NULL)
  
  variable.nodes <- data.nodes[data.nodes$type %in% c ("Data", "Snapshot"), ]
  variable.nodes <- variable.nodes[variable.nodes$name == var.name, ]
  return (variable.nodes)
}

#' get.val.type parses the valTypes of each data node in the given provenance,
#'	or the valType of the specified node, and returns it in a data frame.
#'
#' @rdname access
#' @return A data frame containing the valType of the specified data node, 
#'	or the valTypes of all data nodes if no data node is specified. Return NULL
#'	if there are no data nodes or if the specified data node is not found.
#' @export
get.val.type <- function(prov, node.id = NULL) {
	
	data.nodes <- get.data.nodes(prov)[ , c("id", "valType")]
	
	# extract row for specified node, if applicable
	if(! is.null(node.id))
		data.nodes <- data.nodes[data.nodes$id %in% node.id, ]
	
	# node not found, return null.
	if(nrow(data.nodes) == 0)
		return(NULL)
	
	# use sapply to parse val.type into a matrix with 3 columns
	# since it's a matrix, can query each column or just put into df!!
	parsed.val.type <- sapply(data.nodes[ , "valType"], function(val.type) {
		# a string vector to store the parsed valType
		# keep all terms as strings in order for sapply to be able to convert
		# the resulting list of character vectors into a matrix
		# container, dim, type
		arr = vector(mode = "character", length = 3L)
		
		# there are 2 types of valType:
		# a json object as a string, or
		# a simple string
		if(grepl("^\\{(.+)\\}$", val.type)) {
			
			# Type is string parsed from entity valType
			val.type <- jsonlite::fromJSON(val.type)
			
			arr[1] <- val.type$container
			
			# format dimension and type into a list
			# so that we can put it in a single element of a data frame
			arr[2] <- paste(val.type$dimension, collapse = ",")
			
			# type could be null (e.g. list)
			if(is.null(val.type$type))
				arr[3] <- NA
			else
				arr[3] <- paste(val.type$type, collapse= ", ")
			
		} else {
			arr[1] <- NA
			arr[2] <- NA
			arr[3] <- val.type
		}
		
		return(arr)
	}, USE.NAMES = FALSE)
	
	# form result data frame and return
	result <- data.frame("id" = data.nodes[ , "id"],
						 "container" = parsed.val.type[1, ],
						 "dimension" = parsed.val.type[2, ],
						 "type" = parsed.val.type[3, ],
						 stringsAsFactors = FALSE)
	return(result)
}

## ==== ##
