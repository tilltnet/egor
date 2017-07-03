# Read ego-centric-network data from single file format or two-file format.

#' Trim/ listify ego-centric network data
#'
#' This function generates the \code{alters.list} object. \code{alters.list} is a list where 
#' each entry entails a \code{dataframe} of the alters of one ego. By using
#' the \code{netsize} variable it is ensured, that the list entries are of the
#' correct length and possibly present rows of NA values are deleted.
#' @param long A 'long' dataframe with alters/dyads in rows.
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
#' @keywords internal
long.df.to.list <- function(long, netsize, egoID, back.to.df = F) {
  # Create list where every entry contains all alters of one ego.
  
  tie_list <- split(x = long, f = long[[egoID]])
  
  # Create a new list with entries containing as many alters as the
  # netsize variable predicts. This assumes the NA lines to be at the
  # bottom of the entries - #!# to prevent failure the entries should be
  # sorted with NA lines at the bottom!
  netsize_nona <- netsize
  netsize_nona[is.na(netsize_nona)] <- 0
  netsize_nona[is.nan(netsize_nona)] <- 0
  tie_list_nona <- mapply(FUN = function(x, netsize) x[0:netsize, ], 
                             x = tie_list, netsize = netsize_nona, 
                             SIMPLIFY = F)
  
  
  if (back.to.df == T) 
    return(do.call("rbind", tie_list_nona))
  tie_list_nona
} 


#' Transform 'wide' alter-level data to the 'long'-format
#'
#' A function to transform a wide-format dataframe of ego-centric network data 
#' into a long-format data-frame, where every row represents one alter/dyad. In 
#' the created dataframe numerous networks can be distinguished by a network ID 
#' (egoID).
#' @template wide
#' @template egoID
#' @template max_alters
#' @param start.col Number of first colum containg alter-alter relation data. 
#' #!# Should: Defaults to first column of \code{wide}.
#' @param last.col Number of first colum containg alter-alter relation data.
#' #!# Should: Defaults to last column of \code{wide}.
#' @template ego_vars
#' @param var.wise a logical value indicating wheter the alter attributes are
#' stored variable-wise, if FALSE alter-wise storage is assumed.
#' @keywords internal
wide.to.long <- function(wide, egoID = "egoID", max.alters, start.col, end.col, 
                          ego.vars = NULL, var.wise = F) {
  ### Generating a matrix containing all variable names of one particular alters
  ### item (sex, age, etc.).
  mt_dimmer <- ifelse(var.wise == T, max.alters, NROW(wide[start.col:end.col, ]) / max.alters)
  #print(mt_dimmer)
  name_mt <- matrix(names(wide[start.col:end.col]), mt_dimmer)
  #print(name_mt)
  if(var.wise) name_mt <- t(name_mt)
  #if(!var.wise) print("var.wise not T")
  
  ### Transfrom Matrix to a list where every entry is a vector of the variables 
  ### for one item (sex, age, etc.).
  vary <- list()
  
  # Wenn var.wise max.alters, statt alters.item.count nehmen!!! #!#
  for(i in 1:dim(name_mt)[1]) {
    vary[[i]] <-   name_mt[i,]
  }
  
  # Generate a vector giving numbers to the alters (alterID).
  times <- seq_along(vary[[1]])
  
  ### Create a long format data.frame of the alters items.
  coll_df <- cbind(wide[start.col:end.col], wide[ego.vars])

#' @importFrom stats reshape
  long <- reshape(coll_df, varying = vary, ids = wide[egoID],
                  times = times,  direction = 'long', 
                  new.row.names = 1:(NROW(wide)*length(times)))
  
  ### Change names of alterID and egoID variables.
  colnames(long)[which(names(long) == "time")] <- "alterID"
  colnames(long)[which(names(long) == "id")] <- "egoID"
  #print(which(names(long) == "id"))
  egoID_idx <- grep("egoID", names(long))
  alterID_idx <- grep("alterID", names(long))
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
#' @param max.alters Maximum number of alters for which alter-alter relations 
#' were collected.
#' @keywords internal
wide.dyads.to.edgelist <- function(e.wide, first.var, max.alters,
                                    alters.list = NULL, selection = NULL) {
  
  ### Calculate max. possible count of dyads per network.
  dp <- dyad.poss(max.alters)
  
  ### Create a helper matrix vor naming alters.
  if(is.null(selection)) {
    name.matrix <- 1:max.alters
    for(i in 1:(max.alters-1)) {
      start.val <- i+1
      # c(x:y,rep()) is used to avoid cbind throwing warning because of unequal 
      # vector lengths.
      name.matrix <- cbind(name.matrix, c(start.val:max.alters, rep(9,i)))
      
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
      names_ <- as.character(subset(alters.list[[case]], alters.list[[case]][selection] == 1)$alterID) #!# ['alterID'] ??
      #if(length(names) < max.alters) {
      #  diff_ <- max.alters - length(names_)
      #  names_ <- c(names_, rep("99", diff_))
      #}
      name.matrix <- names_
      for(i in 1:(max.alters-1)) {
        start.val <- i+1
        # c(x:y,rep()) is used to avoid cbind throwing warning because of unequal 
        # vector lengths.
          name.matrix <- suppressWarnings(cbind(name.matrix, c(names_[start.val:max.alters], rep(99,i))))
        
      }
    }
    i <- 1
    for(i in 1:(max.alters - 1)) {
      for(j in 1:(max.alters - i)) {
        this.alter.alter <- data.frame(from = name.matrix[i, 1], to = name.matrix[i+1, j], 
                                       weight = alter.alter[case, count.var])
        alter.alter.df <- rbind(alter.alter.df, this.alter.alter)
        count.var <- count.var + 1
#' @importFrom stats na.omit
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
#' containing alter attributes.
#' @param e.list \code{data.frame} containg edge data/ one edgelist.
#' @param alters \code{data.frame} containg alter attributes.
#' @keywords internal
edges.attributes.to.network <- function(e.list, alters) {
  #print(attributes$alterID)
  igraph::graph.data.frame(d= e.list, vertices= alters, directed= FALSE)
}


#' Generate list of igraph objects from alters and edge data
#'
#' This function generates a list of igraph objects from a edgelists organized in list and a list of
#' dataframes containing alter attributes.
#' @param e.lists \code{List} of \code{data.frame}s containg edge data/ one edgelist.
#' @template alters_list
#' @keywords igraph
#' @export
to.network <- function(e.lists, alters.list) {  
  graph.list <- tryCatch({
    message("Creating igraph objects: $graphs")
    mapply(FUN= edges.attributes.to.network, e.lists, alters.list, 
                       SIMPLIFY=FALSE)},
    warning=function (cond) {
      message("WARNING: There was an warning trying to combine alter and edge data to igraph objects. Carefully check objects for correctness!")
      message(paste("igraph warning: ", cond))
      return(mapply(FUN= edges.attributes.to.network, e.lists, alters.list, 
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
#' This function adds ego attributes to a 'alters.list' object of ego-centered
#' networks. This is helpful if (multi-level) regressions are to be executed.
#' @template alters_list
#' @template egos 
#' @template ego_vars 
#' @template netsize
#' @keywords internal
add_ego_vars_to_long_df <- function(alters.list, egos.df, ego.vars, netsize) {
  new_alters.list <- alters.list
  for (var in ego.vars) {
    for(i in 1:length(alters.list)) {
      new_alters.list[[i]] <- cbind(new_alters.list[[i]], rep(egos.df[i,][[var]], netsize[i]))
      new_var_pos <- length(colnames(new_alters.list[[i]]))
      colnames(new_alters.list[[i]])[new_var_pos] <- paste("ego", var, sep = "_")
    }
  }
  # Return as long.df
  do.call("rbind", new_alters.list)
}

#' Import ego-centric network data from 'one file format'
#'
#' This function imports ego-centric network data, stored in a single file, providing
#' ego, alter and edge data. This data format is for exampled used by the Allbus 2010 (GESIS)
#' and similar social surveys.
#' @template egos
#' @template netsize
#' @template egoID
#' @param attr.start.col First colum containing alter attributes.
#' @param attr.end.col Last colum containing alter attributes.
#' @param dy.max.alters Maximum number of alters.
#' @param dy.first.var First column containing alter-alter relations/ edges.
#' @template ego_vars
#' @param var.wise Logical value indicatin if the alter attributes are sorted variable wise (defaults to FALSE).
#' @param ... additional arguments to [egor()].
#' @template return_egoR
#' @references Muller, C., Wellman, B., & Marin, A. (1999). How to Use SPSS to 
#' Study Ego-Centered Networks. Bulletin de Methodologie Sociologique, 
#' 64(1), 83-100.
#' @keywords import
#' @export
read.egonet.one.file <- function(egos, netsize,  egoID = "egoID", 
                                 attr.start.col, attr.end.col, dy.max.alters,
                                 dy.first.var, ego.vars = NULL, var.wise = F, ...) {
  
  #Sort egos by egoID.
  message("Sorting data by egoID.")
  egos <- egos[order(as.numeric(egos[[egoID]])), ]
  
  message("Transforming alters data to long format.")
  alters.df <- wide.to.long(wide = egos, egoID, max.alters = dy.max.alters,
                        start.col = attr.start.col, end.col = attr.end.col, 
                        ego.vars = ego.vars, var.wise = var.wise)
  
  message("Deleting NA rows in long alters data.")
  message("Splitting long alters data into list entries for each network: $alters.list")
  alters.list <- long.df.to.list(long = alters.df, netsize = netsize, 
                  egoID = "egoID", back.to.df = F)
  
  message("Combining trimmed alters.list to data.frame: $alters.df")
  
  alters.df <- do.call(rbind, alters.list)
  
  message("Transforming wide dyad data to edgelist: $edges")
  e.lists <- wide.dyads.to.edgelist(e.wide = egos, first.var = dy.first.var, 
                                   dy.max.alters)
  
  # Create Global edge list
  aaties <- mapply(FUN = function(x, y) data.frame(egoID = y, x), e.lists, egos[[egoID]], SIMPLIFY = F)
  
  aaties.df <- do.call(rbind, aaties)
  
  # Return:
  egor(alters.df, egos, aaties.df, ...)
}

#' Import ego-centric network data from two file format
#'
#' This function imports ego-centric network data, stored in two files, where 
#' one file contains the ego attributes and the edge information and the other file 
#' contains the alters data. This form of data storage for ego-centered network data 
#' is proposed by Muller, Wellman and Marin (1999).
#' @template egos
#' @template alters
#' @template netsize
#' @template ID.vars
#' @param e.max.alters Maximum number of alters that are included in edge data.
#' @param e.first.var Index of first column in \code{egos} containing edge data.
#' @param ego.vars \code{Character vector} naming variables in the egos data,
#' in order to copy them in to the long alters \code{dataframe}.
#' @param selection \code{Character} naming \code{numeric} variable indicating 
#' alters selection with zeros and ones. 
#' @param ... additional arguments to [egor()].
#' @template return_egoR
#' @keywords import
#' @export
read.egonet.two.files <- function(egos, alters, netsize = NULL,
                                  ID.vars=list(ego="egoID", alter="alterID", source="Source", target="Target"),
                                  e.max.alters, e.first.var,
                                  ego.vars = NULL, selection = NULL, ...) {
  IDv <- modifyList(eval(formals()$ID.vars), ID.vars)
  if(!is.null(IDv$alter)) {
    message("alterID specified; moving to first column of $alters.df.")
    alterID.col <- match(IDv$alter , names(alters))
    #alterID.col
    # Return:
    #!# What happens if alterID is already in column 1?
    alters <- data.frame(alterID = alters[[IDv$alter]], alters[1:(alterID.col - 1)], 
                       alters[(alterID.col + 1) : ncol(alters)])
  } 
  
  # Sort egos by egoID and alters by egoID and alterID.
  message("Sorting data by egoID and alterID.")
  egos <- egos[order(as.numeric(egos[[IDv$ego]])), ]
  alters <- alters[order(as.numeric(alters[[IDv$ego]]), as.numeric(alters[[IDv$alter]])), ]
  
  if(is.null(netsize)) {
    message("No netsize variable specified, calculating/ guessing netsize by egoID in alters data.")
#' @importFrom stats aggregate
    netsize <- aggregate(alters[[IDv$ego]], by = list(alters[[IDv$ego]]), NROW)    
    netsize <- netsize[[2]]    
  }


  message("Preparing alters data.")
  alters.list <- long.df.to.list(long = alters, netsize = netsize, egoID = IDv$ego)
  alters.list <- lapply(alters.list, FUN = function(x) 
    data.frame(alterID = as.character(c(1:NROW(x))), x)) #!# This generates two alterIDs in the transnat import, not good!
  
  if(!is.null(ego.vars)) {
    message("ego.vars defined, adding them to $alters.df")
    alters <- add_ego_vars_to_long_df(alters.list = alters.list, egos.df = egos, 
                            ego.vars = ego.vars, netsize = netsize)
  } else {
    message("Restructuring alters data: $alters.df")
    alters <- do.call("rbind", alters.list)
  }

  message("Splitting alters data into list entries for each network: $alters.list")
  attributes_ <- long.df.to.list(long = alters, netsize = netsize, egoID = IDv$ego,
                                back.to.df = F)
  
  message("Transforming wide edge data to edgelist: $edges")
  elist <- wide.dyads.to.edgelist(e.wide = egos, first.var = e.first.var,
                                   max.alters = e.max.alters, 
                                   alters.list = alters.list, selection = selection)
  

  # Create Global edge list
  aaties <- mapply(FUN = function(x, y) data.frame(egoID = y, x), elist, egos[[IDv$ego]], SIMPLIFY = F)
  
  aaties.df <- do.call(rbind, aaties)
  
  # Return:
  egor(alters, egos, aaties.df, ID.vars = IDv, ...)
}
