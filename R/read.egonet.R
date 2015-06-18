# Read ego-centric-network data from single file format or two-file format.

#' Trim/ listify ego-centric network data
#'
#' This function deletes empty alteri rows and it can be used to transform the 
#' dataframe into a list, with entries for each network.
#' @param long A 'long' dataframe with alteri/dyads in rows.
#' @param wide A 'wide' dataframe with networks in rows.
#' @template netsize
#' @template egoID
#' @param back.to.df If \code{TRUE} a dataframe is returned, if \code{FALSE} a 
#' list. Defaults to \code{FALSE}.
#' @return Returns a \code{list} of \code{dataframes} where every 
#' \code{dataframe} represents one ego/ network and the rows in the
#' \code{dataframe} represents one alter. If the \code{back.to.df} parameter is
#' called the \code{list} entries are combined to one \code{dataframe}, in the
#' 'long' format.
#' line represents 
long.df.to.list <- function(long, wide, netsize, egoID, back.to.df = F) {
  # Create list where every entry contains all alteri of one ego.
  tie_list <- list()
  p <- 1
  for (i in wide[[egoID]]) {
    #!# check if 'egoID' var exits if not recycle egoID name variable
    if("egoID" %in% names(long)) {
      tie_list[[p]] <- subset(long, long[["egoID"]] == i)
    } else {
      tie_list[[p]] <- subset(long, long[[egoID]] == i)
    }
    p <- p + 1
  }
  
  # Create a new list with entries containing as many alteri as the
  # netsize variable predicts. This assumes the NA line to be at the
  # bottom of the entries - to prevent failure the entries should be
  # sorted with NA lines at the bottom!
  tie_list2 <- list()
  for (i in 1:length(tie_list)) {
    net_sz <- netsize[i]
    #print(net_sz)
    if(is.null(net_sz)) net_sz <- NA
    if(!is.na(net_sz) & net_sz > 0) tie_list2[[i]] <- tie_list[[i]][1:net_sz, ]
  }
  if (back.to.df == T) 
    return(do.call("rbind", tie_list2))
  tie_list2
} 

# 



#' Transform 'wide' alter-level data to the 'long'-format
#'
#' A function to transform a wide-format dataframe of ego-centric network data 
#' into a long-format data-frame, where every row represents one alter/dyad. In 
#' the created dataframe numerous networks can be distinguished by a network ID 
#' (egoID).
#' @param items.df A wide-format dataframe of ego-centric network data.
#' @template egoID
#' @param max.alteri A numeric for the maximum number of alteri.
#' @param start.col Number of first colum containg alter-alter relation data. 
#' #!# Should: Defaults to first column of \code{items.df}.
#' @param last.col Number of first colum containg alter-alter relation data. 
#' #!# Should: Defaults to last column of \code{items.df}.

wide.to.long <- function(items.df, egoID, max.alteri, start.col, end.col, 
                          ego.vars = NULL) {
  ### Generating a matrix containing all variable names of one particular alteri
  ### item (sex, age, etc.).
  alteri.item.count <- (end.col-start.col+1)/max.alteri
  name_mt <- matrix(names(items.df[start.col:end.col]), alteri.item.count)
  
  ### Transfrom Matrix to a list where every entry is a vector of the variables 
  ### for one item (sex, age, etc.).
  vary <- list()
  for(i in 1:alteri.item.count) {
    vary[[i]] <-   name_mt[i,]
  }
  
  # Generate a vector giving numbers to the alteri (alterID).
  times <- seq_along(vary[[1]])
  
  ### Create a long format data.frame of the alteri items.
  coll_df <- cbind(items.df[start.col:end.col], items.df[ego.vars])
  
  long <- reshape(coll_df, vary, ids = 
                  items.df[egoID],
                  times = times,  direction = 'long')
  
  ### Change names of alterID and egoID variables.
  
  colnames(long)[which(names(long) == "time")] <- "alterID"
  colnames(long)[which(names(long) == "id")] <- "egoID"
  
  #long$alterID <- as.integer(long$alterID)
  
  ### Return:
  long
}





#' Transform wide alter-alter data to an edge list.
#
#' When alter-alter for numerous networks is stored in one file/ object it is 
#' common use the 'wide' dataformat. This function transforms such data to an 
#' edge lists.
#' @param wide A dataframe containing the alter-alter relation data in the 
#' 'wide' format.
#' @param fist.var Number of colum containing the relation between the first and
#' the second network contact.
#' @param max.alteri Maximum number of alteri for which alter-alter relations 
#' were collected.

wide.dyads.to.edgelist <- function(wide, first.var, max.alteri,
                                    long.list = NULL, selection = NULL) {
  
  ### Calculate max. possible count of dyads per network.
  dyad.poss <- function(max.alteri) { (max.alteri^2-max.alteri)/2 }
  dp <- dyad.poss(max.alteri)
  
  ### Create a helper matrix vor naming alteri.
  if(is.null(selection)) {
    name.matrix <- 1:max.alteri
    for(i in 1:(max.alteri-1)) {
      start.val <- i+1
      # c(x:y,rep()) is used to avoid cbind throwing warning because of unequal 
      # vector lengths.
      name.matrix <- cbind(name.matrix, c(start.val:max.alteri, rep(9,i)))
      
    }
  } 
  ### Extract relevant variables from dataset.
  last.var <- first.var + dp - 1  
  alter.alter <- wide[first.var:last.var]
    
  # Create a list of dataframes, each containg the edgelists per network. 
  #!# This could probably be done with reshape!?
  alter.alter.list <- list()
  count.var <- 1
  
  for(case in 1:NROW(wide)) {
    alter.alter.df <- data.frame()
    count.var <- 1
    if(!is.null(selection)) {
      names_ <- as.character(subset(long.list[[case]], long.list[[case]][selection] == 1)$alterID)
      #if(length(names) < max.alteri) {
      #  diff_ <- max.alteri - length(names_)
      #  names_ <- c(names_, rep("99", diff_))
      #}
      name.matrix <- names_
      for(i in 1:(max.alteri-1)) {
        start.val <- i+1
        # c(x:y,rep()) is used to avoid cbind throwing warning because of unequal 
        # vector lengths.
          name.matrix <- suppressWarnings(cbind(name.matrix, c(names_[start.val:max.alteri], rep(99,i))))
        
      }
    }
    i <- 1
    for(i in 1:(max.alteri - 1)) {
      for(j in 1:(max.alteri - i)) {
        this.alter.alter <- data.frame(from = name.matrix[i, 1], to = name.matrix[i+1, j], 
                                       weight = alter.alter[case, count.var])
        alter.alter.df <- rbind(alter.alter.df, this.alter.alter)
        count.var <- count.var + 1
        alter.alter.df <- na.omit(alter.alter.df)
        rownames(alter.alter.df) <- c()
      }
      alter.alter.list[[as.character(case)]] <- alter.alter.df
    }
  }
  
  ### Delete all zero edges.
  alter.alter.list2 <- lapply(alter.alter.list, function(x)
    subset(x, weight != 0))
  
  ### Return:
  alter.alter.list2 
}


#' edges.attributes.to.network
#'
#' This function generates one igraph object from an edgelist and a dataframe 
#' alteri attributes.
#' @param elist [dataframe]
#' @param attributes [dataframe]
#' @keywords ego-centric network analysis
edges.attributes.to.network <- function(elist, attributes) {
  #print(attributes$alterID)
  igraph::graph.data.frame(d= elist, vertices= attributes, directed= FALSE)
}


#' to.network
#'
#' This function generates a list of igraph objects from an edgelist and a 
#' dataframe alteri attributes.
#' @param elist [list of dataframes]
#' @param attributes [list of dataframes]
#' @keywords ego-centric network analysis
to.network <- function(elist, attributes) {  
  graph.list <- mapply(FUN= edges.attributes.to.network, elist, attributes, 
                       SIMPLIFY=FALSE)
  graph.list
}

#' add_ego_vars_to_long_df
#'
#' This function adds ego attributes to a 'long.list' object of ego-centered
#' networks. This is helpful if (multi-level) regressions are to be executed.
#' @param long.list 
#' @template egos 
#' @param ego.vars 
#' @template netsize
#' @keywords ego-centric network analysis
add_ego_vars_to_long_df <- function(alteri.list, egos.df, ego.vars, netsize) {
  new_alteri.list <- alteri.list
  for (var in ego.vars) {
    for(i in 1:length(alteri.list)) {
      new_alteri.list[[i]] <- cbind(new_alteri.list[[i]], rep(egos.df[i,][[var]], netsize[i]))
      new_var_pos <- length(colnames(new_alteri.list[[i]]))
      colnames(new_alteri.list[[i]])[new_var_pos] <- paste("ego", var, sep = "_")
    }
  }
  # Return as long.df
  do.call("rbind", new_alteri.list)
}

#' Import ego-centric network data from 'one file format'
#'
#' This function imports ego-centric network data, from one file, providing
#' ego, alter and edge data.
#' @template egos
#' @template netsize
#' @template egoID
#' @param attr.start.col
#' @param dy.max.alteri
#' @param dy.first.var
#' @template return_egoR
#' @export
read.egonet.one.file <- function(egos, netsize,  egoID = "egoID", 
                                 attr.start.col, attr.end.col, dy.max.alteri,
                                 dy.first.var) {
  print("Transforming alteri data to long format: $long")
  long <- wide.to.long(items.df = egos, egoID, max.alteri = dy.max.alteri,
                        start.col = attr.start.col, end.col = attr.end.col)
  
  print("Splitting long alteri data into list entries for each network: $long.list")
  attributes <- long.df.to.list(long = long, wide = egos, netsize = netsize, 
                                egoID = egoID, back.to.df = F)
  
  print("Transforming wide dyad data to edgelist: $edges")
  elist <- wide.dyads.to.edgelist(wide = egos, first.var = dy.first.var, 
                                   dy.max.alteri)
  
  print("Creating igraph objects: $graphs")
  graphs <- to.network(elist, attributes)
  
  print("Adding results data.frame: $results")
  list(egos.df = egos, long.df = long, long.list = attributes, edges = elist, 
       graphs = graphs, results = data.frame(egos[[egoID]], netsize))
}

#' Import ego-centric network data from two file format
#'
#' This function imports ego-centric network data, stored in two files, where 
#' one file contains the ego attributes and the edge information and the other file 
#' contains the alteri data.
#' @template netsize
#' @template egoID
#' @param e.max.alteri Maximum number of alteri that are included in edge data.
#' @param e.first.var Index of first column in \code{egos} containing edge data.
#' @template egos
#' @template alteri
#' @template return_egoR
#' @export
read.egonet.two.files <- function(egos, alteri, netsize = NULL,  egoID = "egoID",
                                  alterID = NULL, e.max.alteri, e.first.var,
                                  ego.vars = NULL, selection = NULL) {
  if(!is.null(alterID)) {
    print("alterID specified; moving to first column.")
    alterID.col <- match(alterID , names(alteri))
    alterID.col
    # Return:
    alteri <- data.frame(alterID = alteri[[alterID]], alteri[1:(alterID.col - 1)], 
                       alteri[(alterID.col + 1) : ncol(alteri)])
    }    
  
  if(is.null(netsize)) {
    print("No netsize variable specified, calculating/ guessing netsize by egoID in alteri data.")
    netsize <- aggregate(alteri, by = list(alteri[[egoID]]), NROW)[, 2]
  }

  print("Preparing alteri data.")
  alteri.list <- long.df.to.list(alteri, egos, netsize, "egoID")
  alteri.list <- lapply(alteri.list, FUN = function(x) 
    data.frame(alterID = as.character(c(1:NROW(x))), x))
  
  if(!is.null(ego.vars)) {
    print("ego.vars defined, adding them to $alteri.df")
    alteri <- add_ego_vars_to_long_df(alteri.list = alteri.list, egos.df = egos, 
                            ego.vars = ego.vars, netsize = netsize)
  } else {
    print("Restructuring alteri data: $alteri.df")
    alteri <- do.call("rbind", alteri.list)
  }

  print("Splitting alteri data into list entries for each network: $alteri.list")
  attributes_ <- long.df.to.list(alteri, wide = egos, netsize, "egoID",
                                back.to.df = F)
  
  print("Transforming wide edge data to edgelist: $edges")
  elist <- wide.dyads.to.edgelist(wide = egos, first.var = e.first.var,
                                   max.alteri = e.max.alteri, 
                                   long.list = alteri.list, selection = selection)
  
  print("Creating igraph objects: $graphs")
  graphs <- to.network(elist, attributes_)
  
  list(egos.df = egos, alteri.df = alteri, alteri.list = attributes_, edges = elist,
       graphs = graphs, results = data.frame(egos[[egoID]], netsize))
}


#' Import raw ego-centric network data.
#'
#' This function allows you to import raw ego-centric network data. See X for
#'  supported formats.
#read.egonet <- function(ties = NULL, folders_alter = NULL, 
#folder_edges = NULL) {
#  if(method == "onefile")
#  
# ---- read.egonet is organised with format specific funtions for now.  
#}

