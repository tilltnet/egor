#' Vertex attribute to data frame into graph.
#' 
#' Extracts a vertex variable from a data.frame and returns it as a vector ready
#' to be set as vertex attribute in a graph. May aggregate the variable by node 
#' before returning. This function extracts variables at the author level from 
#' 'data' and set them as vertex.attributes in 'graph', a network of authors. 
#' The function aggregates the variable by author using the aggregating function
#' FUN. The function needs a 'graph' igraph object of authors in which the UFIDs
#' are recorded as a  attribute, and a 'data' data.frame in which the same UFIDs
#' are associated to a variable (attribute). The function takes \code{data} and 
#' aggregates FUN(attribute) by dataID in 'data'. It returns the result of the 
#' aggregation in the order of dataID given by V(graph)$name. NOTE that if 
#' attribute is categorical (e.g. College), the argument FUN is ignored and 
#' vert.attr() is just going to take the category of the variable for authorID 
#' (if there are more than 1 category, e.g. more than 1 Academic Units, for the 
#' same authorID, vert.attr just takes the 1st category associated to authorID 
#' in 'data').
#' 
#' @param data Data frame. The data.frame containing the author attribute 
#'   variable to be imported in the graph (it must also include a authorID 
#'   variable).
#' @param graph Researchers graph where to import the attribute.
#' @param dataID Character. The variable in data giving the ID of nodes in the 
#'   graph (authors)
#' @param graphID Character. The vertex attribute of graph that gives node IDs 
#'   (i.e. IDs of authors) which are the same as dataID.
#' @param attribute A variable name in 'data' (as character)
#' @param FUN A function name given as character. The argument is ignored if 
#'   "data" has one record for each dataID value, i.e. does not need aggregation
#'   by dataID. If NA or missing, attribute is aggregated by just taking its 1st
#'   non-NA value for each value of dataID.
#' @param unfactor Logical. If TRUE, and attribute is character, keeps it from 
#'   being turned into factor. Notice that this argument has no effect if the
#'   attribute is aggregated first. \bold{TODO}: regulate this behavior when 
#'   attribute is factor (not character) in the first place; regulate this 
#'   behavior when attribute is aggregated first.
#'   
#' @return A vector that contains the attribute 'attribute' from data, in the 
#'   order given by the order of authorID in the graph 'graph'.
#'   
#' @seealso \code{\link{vert.attr.multi}}
#'   
#' 
vert.attr <- function(data, attribute, graph, dataID, graphID= "name", FUN= NA, unfactor= TRUE) {

  # Argument validity
  stopifnot(is.data.frame(data), is.character(attribute), igraph::is.igraph(graph), dataID %in% names(data), attribute %in% names(data), graphID %in% igraph::list.vertex.attributes(graph))
  
  # Crop only relevant variables: dataID and attribute
  data <- data[,c(dataID, attribute)]
  
  # Clean data from duplicated on c(dataID, attribute), because the function needs to evaluate
  # whether attribute needs aggregation or not.
  data <- data[!duplicated(data),]
  
  # Get the relevant objects:
  ## authorID from 'data'
  dataID <- data[[dataID]] 
  ## 'attribute' from 'data'
  attr <- data[[attribute]]
  
  ## authorID in the order of V(graph)$name, from 'graph'
  vertex.name <- igraph::get.vertex.attribute(graph, name= graphID)
  
  # If data need to be aggregated (there are any duplicates in dataID), pick up the aggregating
  # function FUN.
  if (any(duplicated(dataID))) {
    # If argument FUN is NOT specified.
    if (missing(FUN)) {
      # ...simply get the function: pick the first non-NA value of attr for each dataID.
      f <- function(x) x[!is.na(x)][1]
    } else {
      # Otherwise, if FUN is specified
      # simply get FUN if it's already function.
      if (is.function(FUN)) {
        f <- FUN
      } else {
        # (I put other conditions in if{} because if FUN is a function testing these conditions on it gives a warning)
        # If FUN is characgter, match FUN name.
        if (is.character(FUN)) {
          f <- match.fun(FUN)
        } else {
          if (is.na(FUN)) {
            # If FUN is not a function and it's NA, simply get the function: pick the first non-NA
            # value.
            f <- function(x) x[!is.na(x)][1]
          } else {
            # If we are at this point, FUN is specified, but not a function, not a character and not NA: ERROR because don't know how to pick FUN
            stop("FUN is specified but not character, function or NA")
          }
        }
      }
    }
    
    # Aggregate attribute 'attr' by dataID using the function picked above.
    aggr <- aggregate(x= attr, by= list(dataID), FUN= f)
    
    # Rename variables resulting from aggregate
    names(aggr) <- c("dataID", "attr")
    
  } else {
    # Otherwise, if there is one record for each dataID, attr does not need to
    # be aggregated and we don't need any aggregating function: just crop the
    # relevant variables out of "data".
    
    # Depending on unfactor argument, set value of stringsAsFactors
    saf <- ifelse(unfactor, FALSE, TRUE)
    
    # Creat the data frame
    aggr <- data.frame(dataID= dataID, attr= attr, stringsAsFactors= saf)
    
  }
  
  # Put vertex.name in a data.frame with an additional variable giving the record order
  vertex.name <- data.frame(vertex.name= vertex.name, order= 1:length(vertex.name))
  
  # To put the result of aggregation in the order of authorID given by vertex.name, merge with
  # vertex.name by ufid.
  # all.y=TRUE means that vertexes are kept in the resulting data.frame ONLY if they are in the
  # graph (vertex.name).
  aggr <- merge(aggr, vertex.name,
                by.x= 'dataID', by.y= 'vertex.name',
                all.y=TRUE)
  
  # aggr contains ONLY and ALL the vertexes in the graph. Order them by the variable "order" (which 
  # gives the original order of ufids in vertex.name)
  aggr <- aggr[order(aggr$order),]
  
  # Get only the attribute vector from the data.frame
  attr <- aggr[, 'attr']

  # Return attr
  return(attr)
}
