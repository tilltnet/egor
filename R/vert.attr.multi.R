#' Get multiple variables from a data.frame and set them as vertex attributes
#' in a graph. Uses \code{vert.attr()} in a loop.
#' 
#' This function repeatedly applies vert.attr() to a set of attributes. The 
#' problem that this function solves is that authors (nodes) in the graph and 
#' authors (records) in the data.frame data are not necessarily in the same 
#' sort order.
#' 
#' @param data The data.frame containing the vertex attribute variables to be 
#' imported in the graph (it must also include a authorID variable).
#' @param graph The graph where to import the attribute.
#' @param dataID Character. The ID variable for nodes in \code{data}. Must be the
#' name of a variable in \code{data}. Will be used as a merge key with 
#' \code{graphID}.
#' @param graphID Character. The vertex attribute that contains node IDs in 
#' \code{graph}. Must be the name of a vertex attribute in \code{graph}. Will be 
#' used as a merge key with \code{dataID}.
#' @param attributes Character. Variable names in \code{data}.
#' @param FUN Character. Function names given as character vector (the FUN 
#' argument to \code{vert.attr()}). If the vector is shorter than "attributes", 
#' it's recycled. Defaults to NA for all attributes.
#' @param attr.names Character. Names to use for the attributes in the graph. 
#' Defaults to \code{attributes}.
#' 
#' @return An igraph object.
#' 
#' @section Uses: 
#' \code{\link{vert.attr}}
#' 
#' 
vert.attr.multi <- function(data, graph, dataID, graphID= "name", attributes, FUN=NA, attr.names= attributes) {
  
  # Argument validity.
  stopifnot(is.data.frame(data), is.character(attributes), igraph::is.igraph(graph))
  
  # If FUN is missing, that means FUN is NA for each attribute
  if (missing(FUN)) {
    FUN <- rep(NA, length.out= length(attributes))
  } else {
    # Otherwise, if FUN is specified but is shorter than attributes, recycle it.
    ## But first, if FUN is a function, need to convert it to function name as character
    FUN <- rep(FUN, length.out= length(attributes))
  }
  
  # If the vector of attribute names has been specified with different length than "attributes", use "attributes" instead, with a warning.
  if (length(attr.names) != length(attributes)) {
    attr.names <- attributes
    warning("'attr.names' has incorrect length, so 'attributes' was used for names.")
  }
  
  # For each element of attributes
  for (i in 1:length(attributes)) {
    # Take the corresponding element of attributes, and apply vert.attr() on that element of attributes and that 
    # element of FUN. Set the result as vertex.attribute in 'graph', whose name is the same as 
    # the element of attributes.
    graph <- igraph::set.vertex.attribute(graph= graph,
                                          name= attr.names[i],
                                          value= vert.attr(graph= graph, data= data, dataID= dataID, graphID= graphID, attribute= attributes[i], FUN= FUN[i]))
  }
  
  # Return the graph
  return(graph)
  
}