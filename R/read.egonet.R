# Read ego-centric-network data from single file format or two-file format.

#' Trim/ listify ego-centric network data
#'
#' This function deletes empty alteri rows and it can be used to transform the dataframe into a list, with entries for each network.
#' @param long A 'long' dataframe with alteri/dyads in rows.
#' @param broad A 'broad' dataframe with networks in rows.
#' @param netsize Name of a variable in \code{broad} consisting of numerics for the network size of each network.
#' @param netID Name of the network ID variable in \code{broad}.
#' @param back.to.df If \code{TRUE} a dataframe is returned, if \code{FALSE} a list. Defaults to \code{FALSE}.
#' @keywords ego-centric netowrk analysis
#' @export
long.df.to.list <- function(long, broad, netsize, netID, back.to.df = F) {
  # Create list where every entry contains all alteri of one ego.
  tie_list <- list()
  p <- 1
  for (i in broad[[netID]]) {
    tie_list[[p]] <- subset(long, netID == i)
    p <- p + 1
  }
  
  # Create a new list with entries containing as many alteris as the
  # netsize variable predicts. This assumes the NA line to be at the
  # bottom of the entries - to prevent failure the entries should be
  # sorted with NA lines at the bottom!
  tie_list2 <- list()
  for (i in 1:length(tie_list)) {
    net_sz <- netsize[i]
    if (!is.na(net_sz) & net_sz > 0) 
      tie_list2[[i]] <- tie_list[[i]][1:net_sz, ]
  }
  if (back.to.df == T) 
    return(do.call("rbind", tie_list2))
  tie_list2
} 

# 



#' Transform 'broad' alter-level data to the 'long'-format
#'
#' A function to transform a broad-format dataframe of ego-centric network data into a long-format data-frame, where every row represents one alter/dyad. In the created dataframe numerous networks can be distinguished by a network ID (netID).
#' @param items.df A broad-format dataframe of ego-centric network data.
#' @param netID Variable containing netork IDs. (#!# Harmonise netID/ netID!!)
#' @param max.alteri A numeric for the maximum number of alteri.
#' @param start.col Number of first colum containg alter-alter relation data. #!# Should: Defaults to first column of \code{items.df}.
#' @param last.col Number of first colum containg alter-alter relation data. #!# Should: Defaults to last column of \code{items.df}.
#' @export 
broad.to.long <- function(items.df, netID, max.alteri, start.col, end.col) {
  ### Generating a matrix containing all variable names of one particular alteri item (sex, age, etc.).
  alteri.item.count <- (end.col-start.col+1)/max.alteri
  name_mt <- matrix(names(items.df[start.col:end.col]), alteri.item.count)
  
  ### Transfrom Matrix to a list where every entry is a vector of the variables for one item (sex, age, etc.).
  vary <- list()
  for(i in 1:alteri.item.count) {
    vary[[i]] <-   name_mt[i,]
  }
  
  # Generate a vector giving numbers to the alteri (alterID).
  times <- seq_along(vary[[1]])

  ### Create a long format data.frame of the alteri items.
  long <- reshape(items.df[start.col:end.col], vary,  ids = items.df[[netID]], times = times,  direction = 'long')
  
  ### Change names of alterID and netID variables.
  names(long)[1] <- "alterID"
  colnames(long)[alteri.item.count+2] <- "netID"
  
  ### Return:
  long
}





#' Transform broad alter-alter data to an edge list.
#
#' When alter-alter for numerous networks is stored in one file/ object it is common use the 'broad' dataformat. This function transforms such data to an edglist.
#' @param broad A dataframe containing the alter-alter relation data in the 'broad' format.
#' @param fist.var Number of colum containing the relation between the first and the second network contact.
#' @param max.alteri Maximum number of alteri for which alter-alter relations were collected.
#' @export
broad.dyads.to.edgelist <- function(broad, first.var, max.alteri) {
  
  ### Calculate max. possible count of dyads per network.
  dyad.poss <- function(max.alteri) { (max.alteri^2-max.alteri)/2 }
  dp <- dyad.poss(max.alteri)
  
  ### Create a helper matrix vor naming alteri.
  name.matrix <- 1:max.alteri
  for(i in 1:(max.alteri-1)) {
    start.val <- i+1
    end.val <- max.alteri
    name.matrix <- cbind(name.matrix, start.val:end.val)
    
  }
  
  ### Extract relevant variables from dataset.
  last.var <- first.var + dp - 1  
  alter.alter <- broad[first.var:last.var]
    
  # Create a list of dataframes, each containg the edgelists per network. #!# This could probably be done with reshape!
  alter.alter.list <- list()
  count.var <- 1
  
  for(case in 1:nrow(broad)) {
    alter.alter.df <- data.frame()
    count.var <- 1
    for(i in 1:(max.alteri - 1)) {
      for(j in 1:(max.alteri - i)) {
        this.alter.alter <- data.frame(from = i, to = name.matrix[i+1, j], weight = alter.alter[case, count.var])
        alter.alter.df <- rbind(alter.alter.df, this.alter.alter)
        count.var <- count.var + 1
        alter.alter.df <- na.omit(alter.alter.df)
        rownames(alter.alter.df) <- c()
      }
      alter.alter.list[[case]] <- alter.alter.df
    }
  }
  
  ### Delete all zero edges.
  alter.alter.list2 <- lapply(alter.alter.list, function(x)
    subset(x, weight != 0))
  
  ### Return:
  alter.alter.list2 
}

long.to.attributes <- function(long, alterID) {

}


#' edges.attributes.to.network
#'
#' This function 
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
edges.attributes.to.network <- function(elist, attributes) {
  graph.data.frame(d= elist, vertices= attributes, directed= FALSE)
}




#' to.network
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export

to.network <- function(elist, attributes) {  
  graph.list <- mapply(FUN= edges.attributes.to.network, elist, attributes, SIMPLIFY=FALSE)
  graph.list
}

read.egonet.one.file <- function(egos, netsize,  netID = "netID", attr.start.col, attr.end.col, dy.max.alteri, dy.first.var) {
  long <- broad.to.long(items.df = egos, netID, max.alteri = dy.max.alteri, start.col = attr.start.col, end.col = attr.end.col)    
  attributes <- long.df.to.list(long, broad = egos, netsize, netID, back.to.df = F)
  elist <- broad.dyads.to.edgelist(broad = egos, first.var = dy.first.var, max.alteri)
  graphs <- to.network(elist, attributes)
  list(egos.df = egos, long.df = long, long.list = attributes, edges = elist, graphs = graphs, results = data.frame(egos[[netID]], netsize))
}

read.egonet.two.files <- function(egos, long, netsize = NULL,  netID = "netID", alterID = NULL, dy.max.alteri, dy.first.var) {
  if(!is.null(alterID)) {
    alterID.col <- match(alterID , names(long))
    alterID.col
    # Return:
    long <- data.frame(alterID = long[[alterID]], long[1:(alterID.col - 1)],long[(alterID.col + 1) : ncol(long)])
    }    
    
  long.list <- long.df.to.list(long, egos, netsize, "netID")
  long.list <- lapply(long.list, FUN = function(x) data.frame(alterID = as.character(c(1:NROW(x))), x))
  long.list[1]
  long <- do.call("rbind", long.list)


  if(is.null(netsize)) netsize <- aggregate(long, by = list(long$netID), NROW)[2]
  attributes <- long.df.to.list(long, broad = egos, netsize, netID, back.to.df = F)
  elist <- broad.dyads.to.edgelist(broad = egos, first.var = dy.first.var, max.alteri = dy.max.alteri)
  graphs <- to.network(elist, attributes)
  list(egos.df = egos, long.df = long, long.list = attributes, edges = elist, graphs = graphs, results = data.frame(egos[[netID]], netsize))
}



#' Import raw ego-centric network data.
#'
#' This function allows you to import raw ego-centric network data. See X for supported formats.
#read.egonet <- function(ties = NULL, folders_alter = NULL, folder_edges = NULL) {
#  if(method == "onefile")
#  
# ---- read.egonet is organised with format specific funtions for now.  
#}

