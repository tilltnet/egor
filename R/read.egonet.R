# Read ego-centric-network data from single file format or two-file format.

#' Trim/ listify ego-centric network data
#'
#' This function generates the \code{alteri.list} object. \code{alteri.list} is a list where 
#' each entry entails a \code{dataframe} of the alteri of one ego. By using
#' the \code{netsize} variable it is ensured, that the list entries are of the
#' correct length and possibly present rows of NA values are deleted.
#' @param long A 'long' dataframe with alteri/dyads in rows.
#' @template wide
#' @template netsize
#' @template egoID
#' @param back.to.df If \code{TRUE} a dataframe is returned, if \code{FALSE} a 
#' list. Defaults to \code{FALSE}.
#' @return Returns a \code{list} of \code{dataframes} where every 
#' \code{dataframe} represents one ego/ network and the rows in the
#' \code{dataframe} represents one alter. If the \code{back.to.df} parameter is
#' called the \code{list} entries are combined to one \code{dataframe}, in the
#' 'long' format.
#' @export
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
  # bottom of the entries - #!# to prevent failure the entries should be
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


#' Transform 'wide' alter-level data to the 'long'-format
#'
#' A function to transform a wide-format dataframe of ego-centric network data 
#' into a long-format data-frame, where every row represents one alter/dyad. In 
#' the created dataframe numerous networks can be distinguished by a network ID 
#' (egoID).
#' @template wide
#' @template egoID
#' @template max_alteri
#' @param start.col Number of first colum containg alter-alter relation data. 
#' #!# Should: Defaults to first column of \code{wide}.
#' @param last.col Number of first colum containg alter-alter relation data.
#' #!# Should: Defaults to last column of \code{wide}.
#' @template ego_vars
#' @param var.wise a logical value indicating wheter the alter attributes are
#' stored variable-wise, if FALSE alter-wise storage is assumed.
#' @export
wide.to.long <- function(wide, egoID = "egoID", max.alteri, start.col, end.col, 
                          ego.vars = NULL, var.wise = F) {
  ### Generating a matrix containing all variable names of one particular alteri
  ### item (sex, age, etc.).
  mt_dimmer <- ifelse(var.wise == T, max.alteri, NROW(wide[start.col:end.col, ]) / max.alteri)
  #print(mt_dimmer)
  name_mt <- matrix(names(wide[start.col:end.col]), mt_dimmer)
  #print(name_mt)
  if(var.wise) name_mt <- t(name_mt)
  #if(!var.wise) print("var.wise not T")
  
  ### Transfrom Matrix to a list where every entry is a vector of the variables 
  ### for one item (sex, age, etc.).
  vary <- list()
  
  # Wenn var.wise max.alteri, statt alteri.item.count nehmen!!! #!#
  for(i in 1:dim(name_mt)[1]) {
    vary[[i]] <-   name_mt[i,]
  }
  
  # Generate a vector giving numbers to the alteri (alterID).
  times <- seq_along(vary[[1]])
  
  ### Create a long format data.frame of the alteri items.
  coll_df <- cbind(wide[start.col:end.col], wide[ego.vars])
  
  long <- reshape(coll_df, vary, ids = wide[egoID],
                  times = times,  direction = 'long')
  
  ### Change names of alterID and egoID variables.
  colnames(long)[which(names(long) == "time")] <- "alterID"
  colnames(long)[which(names(long) == "id")] <- "egoID"
  #print(which(names(long) == "id"))
  egoID_idx <- grep("egoID", names(long))
  alterID_idx <- grep("alterID_idx", names(long))
  long <- data.frame(alterID = long["alterID"], egoID = long["egoID"], long[, -c(egoID_idx, alterID_idx)])
  long <- long[with(long, order(egoID)), ]
  
  ### Return:
  long
}


#' Transform wide alter-alter data to an edge list.
#
#' When alter-alter for numerous networks is stored in one file/ object it is 
#' common use the 'wide' dataformat. This function transforms such data to an 
#' edge lists.
#' @param e.wide A dataframe containing the alter-alter relation data in the 
#' 'wide' format.
#' @param fist.var Number of colum containing the relation between the first and
#' the second network contact.
#' @param max.alteri Maximum number of alteri for which alter-alter relations 
#' were collected.
#' @export
wide.dyads.to.edgelist <- function(e.wide, first.var, max.alteri,
                                    alteri.list = NULL, selection = NULL) {
  
  ### Calculate max. possible count of dyads per network.
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
  alter.alter <- e.wide[first.var:last.var]
    
  # Create a list of dataframes, each containg the edgelists per network. 
  #!# This could probably be done with reshape!?
  alter.alter.list <- list()
  count.var <- 1
  
  for(case in 1:NROW(e.wide)) {
    alter.alter.df <- data.frame()
    count.var <- 1
    if(!is.null(selection)) {
      names_ <- as.character(subset(alteri.list[[case]], alteri.list[[case]][selection] == 1)$alterID) #!# ['alterID'] ??
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
#' @param e.list \code{data.frame} containg edge data/ one edgelist.
#' @param alteri \code{data.frame} containg alteri attributes.
#' @keywords ego-centric network analysis
#' @export
edges.attributes.to.network <- function(e.list, alteri) {
  #print(attributes$alterID)
  igraph::graph.data.frame(d= e.list, vertices= alteri, directed= FALSE)
}


#' to.network
#'
#' This function generates a list of igraph objects from an edgelist and a 
#' dataframe alteri attributes.
#' @param e.lists \code{List} of \code{data.frame}s containg edge data/ one edgelist.
#' @template alteri_list
#' @keywords ego-centric network analysis
#' @export
to.network <- function(e.lists, alteri.list) {  
  graph.list <- tryCatch({
    message("Creating igraph objects: $graphs")
    mapply(FUN= edges.attributes.to.network, e.lists, alteri.list, 
                       SIMPLIFY=FALSE)},
    warning=function (cond) {
      message("WARNING: There was an warning trying to combine alter and edge data to igraph objects. Carefully check objects for correctness!")
      message(paste("igraph warning: ", cond))
      return(mapply(FUN= edges.attributes.to.network, e.lists, alteri.list, 
                    SIMPLIFY=FALSE))},
    error=function (cond) {
      message("WARNING: There was an error trying to combine alter and edge data to igraph objects. $graphs will be empty!")
      message(paste("igraph error: ", cond))
      return(list())}
    )
  graph.list
}

#' add_ego_vars_to_long_df
#'
#' This function adds ego attributes to a 'alteri.list' object of ego-centered
#' networks. This is helpful if (multi-level) regressions are to be executed.
#' @template alteri_list
#' @template egos 
#' @template ego_vars 
#' @template netsize
#' @keywords ego-centric network analysis
#' @export
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
#' @param attr.start.col First colum containing alter attributes.
#' @param attr.end.col Last colum containing alter attributes.
#' @param dy.max.alteri Maximum number of alteri.
#' @param dy.first.var First column containing alter-alter relations/ edges.
#' @template return_egoR
#' @export
read.egonet.one.file <- function(egos, netsize,  egoID = "egoID", 
                                 attr.start.col, attr.end.col, dy.max.alteri,
                                 dy.first.var, ego.vars = NULL, var.wise = F) {
  
  #Sort egos by egoID.
  message("Sorting data by egoID.")
  egos <- egos[order(as.numeric(egos[[egoID]])), ]
  
  message("Transforming alteri data to long format: $long")
  alteri.df <- wide.to.long(wide = egos, egoID, max.alteri = dy.max.alteri,
                        start.col = attr.start.col, end.col = attr.end.col, 
                        ego.vars = ego.vars, var.wise = var.wise)
  
  #
  message("Deleting NA rows in long alteri data.")
  alteri.df <- long.df.to.list(long = alteri.df, wide = egos, netsize = netsize, 
                  egoID = egoID, back.to.df = T)
  

  #alteri.df <- alteri.df[order(alteri.df[["egoID"]], alteri.df[["alterID"]]), ]
  
  message("Splitting long alteri data into list entries for each network: $alteri.list")
  alteri.list <- long.df.to.list(long = alteri.df, wide = egos, netsize = netsize, 
                                egoID = egoID, back.to.df = F)
  
  message("Transforming wide dyad data to edgelist: $edges")
  e.lists <- wide.dyads.to.edgelist(e.wide = egos, first.var = dy.first.var, 
                                   dy.max.alteri)
  
  # Check if all egoIDs have alteri associated to them, if not: exclude 0/NA Networks
  egos_have_alteri <- egos[[egoID]] %in% unique(alteri.df[["egoID"]])
  excluded <- egos[!egos_have_alteri, ]
  egos <- egos[egos_have_alteri, ]
  netsize <- netsize[egos_have_alteri]
  
  #print("Creating igraph objects: $graphs")
  graphs <- to.network(e.lists, alteri.list)
  
  message("Adding results data.frame: $results")
  egoR <- list(egos.df = egos, alteri.df = alteri.df, alteri.list = alteri.list, edges = e.lists, 
       graphs = graphs, results = data.frame(egos[[egoID]], netsize))
  if(NROW(excluded) > 0) {
    message("Egos having no alteri associated to them are excluded: $excluded")
    egoR$excluded <- excluded
  }
  #Return:
  egoR
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
    message("alterID specified; moving to first column of $alteri.df.")
    alterID.col <- match(alterID , names(alteri))
    alterID.col
    # Return:
    alteri <- data.frame(alterID = alteri[[alterID]], alteri[1:(alterID.col - 1)], 
                       alteri[(alterID.col + 1) : ncol(alteri)])
  } 
  
  if(is.null(alterID)) alterID <- "alterID"
  #Sort egos by egoID and alteri by egoID and alterID.
  message("Sorting data by egoID and alterID.")
  egos <- egos[order(as.numeric(egos[[egoID]])), ]
  alteri <- alteri[order(as.numeric(alteri[[egoID]]), as.numeric(alteri[[alterID]])), ]
  
  if(is.null(netsize)) {
    message("No netsize variable specified, calculating/ guessing netsize by egoID in alteri data.")
    netsize <- aggregate(alteri[[egoID]], by = list(alteri[[egoID]]), NROW)    
    #results <- merge(egos[egoID], y = netsize, by.x = egoID, by.y = "Group.1", all = T)
    netsize <- netsize[[2]]    
  }

  # Check if all egoIDs have alteri associated to them, if not: exclude 0/NA Networks
  egos_have_alteri <- egos[[egoID]] %in% unique(alteri[[egoID]])
  excluded <- egos[!egos_have_alteri, ]
  egos <- egos[egos_have_alteri, ]
  
  
  print("Preparing alteri data.")
  alteri.list <- long.df.to.list(alteri, egos, netsize, "egoID")
  alteri.list <- lapply(alteri.list, FUN = function(x) 
    data.frame(alterID = as.character(c(1:NROW(x))), x))
  
  if(!is.null(ego.vars)) {
    message("ego.vars defined, adding them to $alteri.df")
    alteri <- add_ego_vars_to_long_df(alteri.list = alteri.list, egos.df = egos, 
                            ego.vars = ego.vars, netsize = netsize)
  } else {
    message("Restructuring alteri data: $alteri.df")
    alteri <- do.call("rbind", alteri.list)
  }

  message("Splitting alteri data into list entries for each network: $alteri.list")
  attributes_ <- long.df.to.list(alteri, wide = egos, netsize, egoID,
                                back.to.df = F)
  
  message("Transforming wide edge data to edgelist: $edges")
  elist <- wide.dyads.to.edgelist(e.wide = egos, first.var = e.first.var,
                                   max.alteri = e.max.alteri, 
                                   alteri.list = alteri.list, selection = selection)
  
  #print("Creating igraph objects: $graphs")
  graphs <- to.network(elist, attributes_)
  
  egoR <- list(egos.df = egos, alteri.df = alteri, alteri.list = attributes_, edges = elist,
       graphs = graphs, results = netsize)
  if(NROW(excluded) > 0) {
    message("Egos having no alteri associated to them are excluded: $excluded")
    egoR$excluded <- excluded
  }
  #Return:
  egoR
}
