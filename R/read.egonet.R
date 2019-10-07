# Read ego-centered-network data from single file format or two-file format.

#' Obtain the index of a column in a data frame (or a list), producing
#' an error if there is a problem.
#'
#' @param name a character vector giving the names of the columns to look up.
#' @param df a [`data.frame`] or a [`list`] object.
#'
#' @return An integer giving the column index of the named column.
#'
#' @note Numeric inputs for `name` are passed through, so this
#'   function is safe to use if the input is already a column index.
#' @keywords internal
col_idx <- function(name, df){
  if(is.numeric(name)) name
  else{
    col <- which(names(df) %in% name)
    if(length(col)!=length(name)) stop("Column ",sQuote(name)," is not found in ", deparse(substitute(df))," or is ambiguous.")
    col
  }
}


#' Longest common prefix of a set of strings.
#'
#' @param x a character vector.
#'
#' @return A character vector that is the longest common substring at
#'   the start of each of the input vectors.
#' @keywords internal
common_prefix <- function(x){
  j <- 0
  # There is probably a faster way to do this.
  while(length(unique(sapply(x, substr, 1, j+1)))==1) j <- j+1
  substr(x[1], 1, j)
}

#' Trim/listify ego-centered network data
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
long.df.to.list <- function(long, netsize, egoID, back.to.df = FALSE) {
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
                             SIMPLIFY = FALSE)
  
  
  if (back.to.df == TRUE) 
    return(do.call("rbind", tie_list_nona))
  tie_list_nona
} 


#' Transform 'wide' alter-level data to the 'long'-format
#'
#' A function to transform a wide-format dataframe of ego-centered network data 
#' into a long-format data-frame, where every row represents one alter/dyad. In 
#' the created dataframe numerous networks can be distinguished by a network ID 
#' (egoID).
#' @template wide
#' @template egoID
#' @template max_alters
#' @param start.col Index or name of the first colum containg alter-alter relation data. 
#' #!# Should: Defaults to first column of \code{wide}.
#' @param last.col Index or name of the first colum containg alter-alter relation data.
#' #!# Should: Defaults to last column of \code{wide}.
#' @template ego_vars
#' @param var.wise a logical value indicating wheter the alter attributes are
#' stored variable-wise, if FALSE alter-wise storage is assumed.
#' @keywords internal
wide.to.long <- function(wide, egoID = "egoID", max.alters, start.col, end.col, 
                         ego.vars = NULL, var.wise = FALSE) {
  start.col <- col_idx(start.col, wide)
  end.col <- col_idx(end.col, wide)
  ### Generating a matrix containing all variable names of one particular alters
  ### item (sex, age, etc.).
  mt_dimmer <- ifelse(var.wise == TRUE, max.alters, ncol(wide[start.col:end.col]) / max.alters)
  #print(mt_dimmer)
  name_mt <- matrix(names(wide[start.col:end.col]), mt_dimmer)
  #print(name_mt)
  if(var.wise) name_mt <- t(name_mt)
  #if(!var.wise) print("var.wise not TRUE")
  
  ### Transfrom Matrix to a list where every entry is a vector of the variables 
  ### for one item (sex, age, etc.).
  vary <- list()
  vn <- c()
  
  # Wenn var.wise max.alters, statt alters.item.count nehmen!!! #!#
  for(i in 1:dim(name_mt)[1]) {
    vary[[i]] <-   name_mt[i,]
    vn  <- c(vn, common_prefix(vary[[i]]))
  }
  
  # Generate a vector giving numbers to the alters (alterID).
  times <- seq_along(vary[[1]])
  
  ### Create a long format data.frame of the alters items.
  coll_df <- cbind(wide[start.col:end.col], wide[ego.vars])

#' @importFrom stats reshape
  long <- reshape(coll_df, varying = vary, ids = wide[egoID], v.names = vn,
                  times = times,  direction = 'long', idvar=egoID,
                  new.row.names = 1:(NROW(wide)*length(times)))
  
  ### Change names of alterID and egoID variables.
  colnames(long)[which(names(long) == "time")] <- "alterID"
  #print(which(names(long) == "id"))
  egoID_idx <- col_idx(egoID, long)
  alterID_idx <- col_idx("alterID", long)
  long <- cbind(egoID = long[egoID], alterID = long["alterID"], long[, -c(egoID_idx, alterID_idx)])
  long <- long[order(long[[egoID]],long$alterID), ]
  
  ### Return:
  long
}


#' Transform wide alter-alter data to an edge list.
#
#' When alter-alter for numerous networks is stored in one file/object it is 
#' common use the 'wide' data format. This function transforms such data to an 
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
  first.var <- col_idx(first.var, e.wide)

  
  ### Calculate max. possible count of dyads per network.
  dp <- dyad.poss(max.alters)
  
  ### Create a helper matrix vor naming alters.
  if (is.null(selection)) {
    name.matrix <- 1:max.alters
    for (i in 1:(max.alters - 1)) {
      start.val <- i + 1
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
  
  for (case in 1:NROW(e.wide)) {
    alter.alter.df <- data.frame()
    count.var <- 1
    if (!is.null(selection)) {
      names_ <- as.character(subset(alters.list[[case]], alters.list[[case]][selection] == 1)$alterID) #!# ['alterID'] ??
      #if(length(names) < max.alters) {
      #  diff_ <- max.alters - length(names_)
      #  names_ <- c(names_, rep("99", diff_))
      #}
      name.matrix <- names_
      for (i in 1:(max.alters - 1)) {
        start.val <- i + 1
        # c(x:y,rep()) is used to avoid cbind throwing warning because of unequal 
        # vector lengths.
          name.matrix <- suppressWarnings(cbind(name.matrix, c(names_[start.val:max.alters], rep(99,i))))
        
      }
    }
    i <- 1
    for (i in 1:(max.alters - 1)) {
      for (j in 1:(max.alters - i)) {
        this.alter.alter <- data.frame(.tmp.srcID = name.matrix[i, 1], .tmp.tgtID = name.matrix[i + 1, j], 
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
    subset(x, x$weight != 0))
  
  ### Return:
  alter.alter.list2 
}

if(getRversion() >= "2.15.1") utils::globalVariables(c("nm"))
#' Transform wide alter-alter data to an edge list.
#
#' A regex based implementation to convert a wide list to an edgelist.
#' 
#' @param e.wide A dataframe containing the alter-alter relation data
#'   in the 'wide' format.
#' @template aa.regex
#' @template netsize
#' @keywords internal
wide.dyads.to.edgelist.regex <- function(e.wide, aa.regex, netsize) {
  en <- names(e.wide)
  ms <- gregexpr(aa.regex, en, perl=TRUE)

  um <- which(unlist(ms)==-1)
  if(length(um)){
    warning("Columns ", paste(sQuote(en[um]), collapse=","), " did not match the regular expression and were omitted.")
    e.wide <- e.wide[-nm]
    if(ncol(e.wide)==0) stop("None of the columns matched the regular expression.")
    en <- names(e.wide)
    ms <- gregexpr(aa.regex, en, perl=TRUE)
  }

  cps <- mapply(function(cn, m){
    ss <- attr(m,"capture.start")
    ls <- attr(m,"capture.length")
    ns <- attr(m,"capture.names")
    mapply(function(s, l, n) structure(substr(cn, s, s+l-1), names=n), ss, ls, ns)
  }, en, ms, SIMPLIFY=FALSE)

  col.list <- mapply(function(col, cp){
    col <- data.frame(col, .egoRow=seq_along(col), .tmp.srcID=as.integer(cp["src"]),.tmp.tgtID=as.integer(cp["tgt"]))
    names(col)[1] <- cp["attr"]
    col
  }, e.wide, cps, SIMPLIFY=FALSE)

  col.df <- Reduce(function(x,y){
    if(names(x)[1]==names(y)[1]) rbind(x,y)
    else merge(x, y, by=c(".egoRow",".tmp.srcID",".tmp.tgtID"), all=TRUE)
  }, col.list)

  col.df.list <- split(col.df, col.df$.egoRow)

  col.df.list <- mapply(function(e, n){
    e <- e[-col_idx('.egoRow', e)]
    e <- e[e$.tmp.srcID<=n & e$.tmp.tgtID<=n,]
  }, col.df.list, netsize, SIMPLIFY=FALSE)
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

#' Import ego-centered network data from 'one file format'
#'
#' This function imports ego-centered network data, stored in a single file, providing
#' ego, alter and edge data. This data format is used by the Allbus 2010 (GESIS)
#' and similar social surveys.
#' @template egos
#' @template netsize
#' @template ID.vars
#' @param attr.start.col Index or name of the first colum containing alter attributes.
#' @param attr.end.col Index or name of the last colum containing alter attributes.
#' @param max.alters Maximum number of alters.
#' @param aa.first.var First column containing alter-alter relations/ edges.
#' @template aa.regex
#' @template ego_vars
#' @param var.wise Logical value indicatin if the alter attributes are sorted variable wise (defaults to FALSE).
#' @param ... additional arguments to [egor()].
#' @template return_egoR
#' @references Muller, C., Wellman, B., & Marin, A. (1999). How to Use SPSS to 
#' Study Ego-Centered Networks. Bulletin de Methodologie Sociologique, 
#' 64(1), 83-100.
#' @keywords import
#' @export
onefile_to_egor <- function(egos, netsize,  ID.vars = list(ego = "egoID"), 
                                 attr.start.col, attr.end.col, max.alters,
                            aa.first.var, aa.regex=NULL, ego.vars = NULL, var.wise = FALSE, ...) {
  IDv <- modifyList(eval(formals()$ID.vars), ID.vars)
  attr.start.col <- col_idx(attr.start.col, egos)
  attr.end.col <- col_idx(attr.end.col, egos)
  aa.first.var <- col_idx(aa.first.var, egos)
  #Sort egos by egoID.
  message("Sorting data by egoID.")
  egos <- egos[order(as.numeric(egos[[IDv$ego]])), ]
  
  message("Transforming alters data to long format.")
  alters.df <- wide.to.long(wide = egos, IDv$ego, max.alters = max.alters,
                        start.col = attr.start.col, end.col = attr.end.col, 
                        ego.vars = ego.vars, var.wise = var.wise)
  
  message("Transforming wide dyad data to edgelist: $edges")
  e.lists <- if (is.null(aa.regex))
    wide.dyads.to.edgelist(e.wide = egos, first.var = aa.first.var,
                           max.alters)
  else
    wide.dyads.to.edgelist.regex(e.wide = egos[aa.first.var:ncol(egos)],
                                 aa.regex = aa.regex,
                                 netsize = netsize)
  
  elist <- purrr::map2_dfr(egos$egoID, e.lists, function(ego_id, edges) 
    {edges[IDv$ego] <- ego_id
    edges})
  
  # Return:
  egor(alters.df,
       egos[-c(attr.start.col:attr.end.col,aa.first.var:ncol(egos))], 
       elist,
       ID.vars = list(ego = IDv$ego,
                      source = ".tmp.srcID",
                      target = ".tmp.tgtID"),
       alter_design = list(max = max.alters),...)
}

#' Import ego-centered network data from two file format
#'
#' This function imports ego-centered network data, stored in two files, where 
#' one file contains the ego attributes and the edge information and the other file 
#' contains the alters data. This form of data storage for ego-centered network data 
#' is proposed by Muller, Wellman and Marin (1999).
#' @template egos
#' @template alters
#' @template netsize
#' @template ID.vars
#' @param e.max.alters Maximum number of alters that are included in edge data.
#' @param e.first.var Index or name of the first column in \code{egos} containing edge data.
#' @param ego.vars \code{Character vector} naming variables in the egos data,
#' in order to copy them in to the long alters \code{dataframe}.
#' @param selection \code{Character} naming \code{numeric} variable indicating 
#' alters selection with zeros and ones. 
#' @param ... additional arguments to [egor()].
#' @template return_egoR
#' @keywords import
#' @export
twofiles_to_egor <- function(egos, alters, netsize = NULL,
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
    message("No netsize variable specified, calculating netsize by egoID in alters data.")
#' @importFrom stats aggregate
    netsize <- aggregate(alters[[IDv$ego]], by = list(alters[[IDv$ego]]), NROW)    
    netsize <- netsize[[2]]    
  }


  message("Preparing alters data.")
  alters.list <- long.df.to.list(long = alters, netsize = netsize, egoID = IDv$ego)
  alters.list <- lapply(alters.list, FUN = function(x) 
    data.frame(alterID = as.character(c(1:NROW(x))), x)) #!# This generates two alterIDs in the transnat impor
  
  if (!is.null(ego.vars)) {
    message("ego.vars defined, adding them to $alters.df")
    alters <- add_ego_vars_to_long_df(alters.list = alters.list, egos.df = egos, 
                            ego.vars = ego.vars, netsize = netsize)
  } else {
    message("Restructuring alters data: $alters.df")
    alters <- do.call("rbind", alters.list)
  }

  message("Transforming wide edge data to edgelist: $edges")
  elist <- wide.dyads.to.edgelist(e.wide = egos, first.var = e.first.var,
                                   max.alters = e.max.alters, 
                                   alters.list = alters.list, selection = selection)
  
  elist <- purrr::map2_dfr(egos$egoID, elist, function(ego_id, edges) 
  {edges[IDv$ego] <- ego_id
  edges})
  
  # Return:
  egor(alters,
       egos,
       elist,
       ID.vars = list(ego = IDv$ego,
                      alter = IDv$alter,
                      source = ".tmp.srcID",
                      target = ".tmp.tgtID"), ...)
}

