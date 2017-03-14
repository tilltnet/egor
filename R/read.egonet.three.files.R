#' Order edge list colums with source and target at the beginning.
#' @keywords internal
order.edge.list.columns <- function(edges, source_, target) {
  cbind(edges[c(source_, target)], edges[!names(edges) %in% c(source_, target)])
}

#' Check for ID integrity
#' @keywords internal
check.ID.integrity <- function(egos, alteri.df, edges, egoID = "egoID", 
                               alterID = "alterID") {
  egos_have_alteri <- egos[[egoID]] %in% unique(alteri.df[[egoID]])
  alteri_have_ego <- alteri.df[[egoID]] %in% egos[[egoID]]
  edges_have_ego <- edges[[egoID]] %in% egos[[egoID]]
  list(egos = egos_have_alteri, alteri = alteri_have_ego, edges = edges_have_ego)
}
#' Ensure ID integrity
#' @keywords internal
ensure.ID.integrity <- function(egos, alteri.df, edges, egoID = "egoID", 
                                alterID = "alterID") {
  valid_IDs <- check.ID.integrity(egos, alteri.df, edges, egoID = egoID, alterID = alterID)
  objs <- list(egos, alteri.df, edges)
  valid_objs <- mapply(FUN = function(x, y)x[y, ], objs, valid_IDs, SIMPLIFY = F)
  invalid_objs <- mapply(FUN = function(x, y)x[!y, ], objs, valid_IDs, SIMPLIFY = F)
  names(valid_objs) <- c("egos", "alteri.df", "edges")
  names(invalid_objs) <- c("egos", "alteri.df", "edges")
  
  list(valid_objs, invalid_objs)
}
#' Check for ID uniqueness
#' @keywords internal
check.ID.unique <- function(x, ID) {
  dup <- duplicated(x[[ID]])
  if(!all(!dup)) {
    message(paste(ID, "values are not unique. Returning duplicated IDs:", sep = " "))
    data.frame(col_1 = x[[1]][dup], ID = x[[ID]][dup])
  } else {
    "all fine!"
  }
}


#' Read ego-centered network data from the three files format, EgoWeb2.0 or
#' openeddi.
#' 
#' @description These functions read ego-centered network data from the three files
#' format, EgoWeb2.0 or openeddi and transform it to an egoR object. The three 
#' files format consists of an ego file, on alteri file and one file containing
#' the edge data. EgoWeb2.0 and openeddi use variations of this format.
#' @template egos
#' @template alteri_df
#' @param ID.vars A character vector specifying ID variable names used (egoID, 
#' alterID, source, target). 
#' @template ego_vars
#' @param edges \code{Dataframe}. A global edge list, first column separates 
#' egos.
#' @param alteri.file \code{Character} name of the alteri data file.
#' @template return_egoR
#' @export
read.egonet.three.files <- function(egos, alteri.df, edges, 
                                    ID.vars = c("egoID", "alterID", "Source", "Target"),
                                    ego.vars = NULL) {
  # 0. Extract ID var Names
  egoID = ID.vars[[1]]
  alterID = ID.vars[[2]]
  source_ = ID.vars[[3]]
  target = ID.vars[[4]]
  
  # 1. Check and ensure data integretiy
  # - Check if all alteri have an ego associated, if not: exclude
  # - Check if all egos have at least one alter listed, if not: exclude
  message("Checking and ensuring data integrity (Rules: egos have alteri, alteri have egos, edges have egos).")
  valid_invalid <- ensure.ID.integrity(egos = egos, alteri.df = alteri.df, edges = edges, egoID = egoID, alterID = alterID)
  valid <- valid_invalid[[1]]
  egos <- valid$egos
  alteri.df <- valid$alteri.df
  edges <- valid$edges
  excluded <- valid_invalid[[2]]
  if(NROW(excluded$egos) + NROW(excluded$alteri.df)+ NROW(excluded$edges) > 0) {
    message("----------------------------")
    if(NROW(excluded$egos) > 0) message("Egos are being excluded.")
    if(NROW(excluded$alteri.df) > 0) message("Alteri are being excluded.")
    if(NROW(excluded$edges) > 0) message("Edges are being excluded.")
    message("Check $excluded to find out which egos, alteri and/or edge data is excluded.")
    message("----------------------------")
  }
  # 2. Reorder edgelist columns and split to list
  edges_cor <- order.edge.list.columns(edges = edges, source_ = source_, target = target)
  edges <- split(edges_cor, factor(edges_cor[[egoID]]))
  
  # 3. Sort by egoID
  # Sort egos by egoID and alteri by egoID and alterID.
  message("Sorting data by egoID and alterID.")
  egos <- egos[order(as.numeric(egos[[egoID]])), ]
  alteri.df <- alteri.df[order(as.numeric(alteri.df[[egoID]]), as.numeric(alteri.df[[alterID]])), ]

  # 4. (Add ego.vars, ) Reorder alteri columns (alterID first!) and split to alteri.df
  # - add ego.vars
  if(!is.null(ego.vars)) {
    message("Adding ego variables to alteri data.")
    alteri.df <- merge(alteri.df, egos[c(egoID, ego.vars)],  by = egoID)
    alteri.df <- alteri.df[order(as.numeric(alteri.df[[egoID]]), as.numeric(alteri.df[[alterID]])), ]
    
  }

  # - reorder
  message("Reordering columns in alteri data (alterID changed to first column).")
  alteri.df <- cbind(alteri.df[alterID], alteri.df[, names(alteri.df) != alterID])
  
  # - calc netsize
  message("Calculating/ guessing netsize by egoID in alteri data.")
  netsize.df <- aggregate(alteri.df[[egoID]], by=alteri.df[egoID], FUN = NROW)
  names(netsize.df)[2] <- "netsize"

  
  # - split to alteri.list
  message("Splitting alteri.df to alteri.list.")
  alteri.list <- split(x = alteri.df, f = factor(alteri.df[[egoID]]))
  

  # 6. to network
  graphs <- egonetR::to.network(edges, alteri.list)
  
  # 7. return egoR
  egoR <- list(egos = egos, 
     alteri.df = alteri.df, 
     alteri.list = alteri.list, 
     edges = edges, 
     graphs = graphs, 
     results = netsize.df, 
     excluded = excluded)
  message(paste(c("egoR Object:", names(egoR)), collapse=" $", sep = ""))
  egoR
}

#' @describeIn read.egonet.three.files This function reads in data from
#' an EgoWeb 2.0 survey and transforms it to an egoR object. If no file name for
#' the egos file is provided ego data is assumed to be merged with alteri data
#' and it will be extracted by \code{read.egoweb}. By default the 
#' standard ID variable names of EgoWeb are used, if you need to specify the ID
#' variable names use the ID.vars parameter. Further Information: 
#' github.com/qualintitative/egoweb
#' @param egos.file A character specifiying the filename of the ego data.
#' @param alter.file A character specifiying the filename of the alteri data.
#' @param edges.file A character specifiying the filename of the edge data.
#' @export
read.egoweb <- function(alter.file, edges.file, egos.file = NULL, 
                        ID.vars = c("EgoID", "Alter.Number", "Alter.1.Number", 
                                    "Alter.2.Number"), ego.vars = NULL) {
  # Preliminaries - Get ID var Names

  # Import CSVs
  alteri.df <- read.csv(file = alter.file)
  edges <- read.csv(file = edges.file)
  if (!is.null(egos.file)) { egos <- read.csv(file = egos.file) 
   } else {
    # Extract ego data
    alterID.index <- which(names(alteri.df) == ID.vars[2])
    egos <- unique(alteri.df[1:(alterID.index - 1)])
    
    # Extract alter data
    egoID.index <- which(names(alteri.df) == ID.vars[1])
    alteri.df <- cbind(
                    alteri.df[c(alterID.index, egoID.index)], 
                    alteri.df[-c(alterID.index, egoID.index)])
  }
  read.egonet.three.files(egos = egos, alteri.df = alteri.df, edges = edges, ID.vars = ID.vars, ego.vars = ego.vars)
}


#' @describeIn read.egonet.three.files This function reads in data created
#' by the openeddi survey software and transforms it to an egoR object. If no 
#' parameters are provided \code{read.openeddi} will try to find the adequate files
#' in the working directory. By default the standard ID variable names of openeddi
#' are used, if you need to specify the ID variable names use the ID.vars parameter.
#' Further Information: www.openeddi.com 
#' @export
read.openeddi <- function(egos.file =  NULL, 
                          alteri.file = NULL, 
                          edges.file = NULL, 
                          ID.vars = c("puid","nameid","nameid","targetid"), 
                          ego.vars = NULL){
  if(is.null(egos.file)) {
    message("No filenames specified, looking for ego, alteri and edge files in working directory.")
    files <- list.files()
    egos_index <-  grep("responses" , files)
    alteri_index <-  grep("namedetails" , files)
    edges_index <-  grep("nameties" , files)
    egos.file <- files[egos_index]
    alteri.file <- files[alteri_index]
    edges.file <- files[edges_index]
  }
  egos <- read.csv(egos.file)
  alteri.df <- read.csv(alteri.file)
  edges <- read.csv(edges.file)
  
  read.egonet.three.files(egos = egos, alteri.df = alteri.df, edges = edges, ID.vars = ID.vars, ego.vars = ego.vars)
}


