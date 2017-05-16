#' Calculate the EI-Index
#'
#' The EI-Index compares the intra-group edge density to the outer-group edge 
#' density. It can be calculated for the whole network and for subgroups. The
#' whole network EI is a metric indicating the tendency of a network to be 
#' grouped by the categories of a given factor variable. The EI value of a 
#' groups describes the tendency of a group to be connected or not connected 
#' to other groups. Additionally, the EI index can be employ as a measurment
#' for egos tendendy to homo-/heterphily - use the \code{composition} command
#' for this version, as it is a compositional measure.
#' @param alters \code{List} of alters attribute \code{data.frame}s 
#' or \code{data.frame} of alters attributes.
#' @param edges_ \code{List} of edgelist-\code{dataframes} or one 
#' \code{dataframes} #' containing all edges_
#' @param var_name \code{Character} naming grouping variable.
#' @param egoID \code{Character} naming ego ID variable.
#' @param alterID \code{Character} naming alter ID variable.
#' @references Krackhardt, D., Stern, R.N., 1988. Informal networks and 
#' organizational crises: an experimental simulation. Social Psychology 
#' Quarterly 51 (2), 123-140.
#' @references Everett, M. G., & Borgatti, S. P. (2012). Categorical attribute 
#' based centrality: E-I and G-F centrality. Social Networks, 34(4), 562-569. 
#' @keywords ego-centered network
#' @keywords sna
#' @examples
#' data("alters32")
#' data("edges32")
#' EI(alters32, edges32, var_name = "alter.sex")
#' @export
EI <- function(alters, edges_, var_name, egoID = "egoID", alterID = "alterID") {
  
  # Check if edges_ are dataframe or list, if dataframe split to list by egoID.
  if (!is.data.frame(edges_)) { 
    edges_.list <- edges_
  } else {
    edges_.list <- split(edges_, as.numeric(edges_[[egoID]]))
  }
  
  # Check if alters are dataframe or list, if dataframe: split to list by egoID.
  if (!is.data.frame(alters)) { 
    alters.list <- alters
  } else {
    alters.list <- split(alters, as.numeric(alters[[egoID]]))
  }
  
  # Function: calculating possible dyads between a given number of alters/ nodes.
  dyad.poss <- function(max.alters) { (max.alters ^ 2 - max.alters) / 2 }
  
  # Function for calculating the possible internal and external edges_.
  possible_edges_ <- function(alters, var_name) {
    
    # Select only those alters for which the variable in question was collected.
    alters <- alters[!is.na(alters[[var_name]]), ]
    
    alters_groups <- split(alters, alters[[var_name]])
    tble_var <- sapply(alters_groups, FUN = NROW)
    poss_internal <- sapply(tble_var, FUN=dyad.poss, simplify = T)
    
    poss_external <- sapply(tble_var, FUN=function(x) {
      poss_ext_edges_ <- (NROW(alters) - x) * x
    })
    
    list(poss_internal = poss_internal, poss_external = poss_external)
  }
  
  # Classify a single edge as heterogen or homogen.
  rows_to_hm_hts <- function(edge, alters) {
    if ('Source' %in% names(edge)) {
      source_ <- edge$Source
      target_ <- edge$Target
    } else {
      source_ <- as.numeric(as.character(edge[1,1]))
      target_ <- as.numeric(as.character(edge[1,2]))
    }
    hm_ht <- ifelse(alters[as.numeric(as.character(alters[[alterID]])) == source_, ][[var_name]] == alters[as.numeric(as.character(alters[[alterID]])) == target_, ][[var_name]], 'HM', 'HT')
    hm_ht
  }
  
  # Calculate group and network EIs.
  lists_to_EIs <- function(edges_, alters, alterID = 'alterID') {
    #print(alters$a0fall)
    if(NROW(edges_)<1 | !NROW(alters)>1 ) return(na.df)
    if(length(table(alters[[var_name]])) < 2) return(na.df)
    if(sum(!is.na(alters[[var_name]])) < 2) return(na.df)
    # Make sure alters are sorted and there is a useful alterID
    alters <- alters[order(alters[[alterID]]), ]
    #alters[[alterID]] <- 1:NROW(alters)
    
    # Function for calulation EI.
    calc.EI <- function(E, I) {(E-I)/(E+I)}
    # Classify all edges_ as homogen, or heterogen.
    hm_hts <- plyr::adply(edges_, .margins = 1, .fun = rows_to_hm_hts, alters)
    hm_hts_ <- factor(rev(hm_hts)[[1]], levels = c('HM', 'HT'))
    tble_hm_hts <- table(hm_hts_)
    #if(is.na(tble_hm_hts[1]) | length(tble_hm_hts)<2) return(na.df)
    
    # Calculate regular EI for whole network.
    EIs <- as.numeric(calc.EI(tble_hm_hts['HT'], tble_hm_hts['HM']))
    # Get possible edges_ for all groups (internal and external).
    poss_int_ext <- possible_edges_(alters, var_name)
    # Count internal and external edges_ for all groups.
    int_ext <- list()
    var_levels <- levels(factor(alters[[var_name]]))
    for(i in 1:length(var_levels)) {
      
      alters_ids <- alters[alterID][ alters[[var_name]] == var_levels[[i]] , ]
      
      if ('Source' %in% names(hm_hts)) {
        grp_edges_ <- hm_hts[hm_hts$Source %in%  alters_ids | hm_hts$Target %in%  alters_ids, ]
      } else {
        grp_edges_ <- hm_hts[hm_hts[1,1] %in%  alters_ids | hm_hts[1,2] %in%  alters_ids, ]
      }
      grp_edges_ <- rev(grp_edges_)[[1]]
      grp_edges_ <- factor(grp_edges_, c("HM", "HT"))
      tble_grp_edges_ <- table(grp_edges_)
      int_ext[['HM']][[var_levels[[i]]]] <- tble_grp_edges_['HM']
      int_ext[['HT']][[var_levels[[i]]]] <- tble_grp_edges_['HT']
    } 
    # Dichte für alle Gruppen berechnen. 
    densities <- mapply(function(x,y) x/y, int_ext, poss_int_ext)
    
    # Calculate group EIs, controlled by group-size.
    group_EIs <- calc.EI(densities[, 2], densities[, 1])
    
    # Average of group EIs.
    #avg_net_EIs <-  sum(group_EIs)/length(var_levels)
    # Calculate possible external edges_ for whole network.
    poss_all <- dyad.poss(NROW(alters))
    poss_ext_all <- poss_all - sum(poss_int_ext$poss_internal)
    
    # Calculate size controlled EI for whole network.
    sc_i <- (sum(densities[, 1]) / length(var_levels))
    sc_e <- (as.numeric(tble_hm_hts['HT']) / poss_ext_all)
    net_EIs_sc <- calc.EI(sc_e, sc_i)
    
    # Return data.frame with all EIs.
    data.frame(EI = EIs, sc_EI = net_EIs_sc, t(group_EIs))
    #data.frame(EI = net_EIs_sc, t(group_EIs))
  }

    # Create NA data-frame row for networks with missing data or only a single group
  na.df <- data.frame(t(c(EI = NA, sc_EI = NA, rep(NA, nlevels(factor(alters[[var_name]]))))))
  names(na.df) <- c(names(na.df)[1:2], levels(factor(alters[[var_name]])))
  na.df <- data.frame(na.df)

  # Invoke mapply on edges_ and alters using list_to_EIs.
  EIs <- mapply(lists_to_EIs, edges_.list, alters.list, SIMPLIFY = F)
  #class(EIs)
  lapply(EIs, FUN = function(x) colnames(x) <- colnames(EIs[[1]]))
  res <- do.call(rbind, EIs)
  res[2:NCOL(res)]
}


#' Network fragments of ego-centerd networks
#'
#' Calculate the count of fragments ego-centered networks form, if their 
#' respective egos are removed from the network.
#' @param alters.list \code{List} of \code{data frames} containing the alters 
#' data.
#' @param edges.list \code{List} of \code{data frames} containing the edge 
#' lists (= alter-alter relations).
#' @keywords ego-centered network
#' @keywords sna
#' @export
fragmentations <- function(alters.list, edges.list) { #!# This function should be taken out soon!
  graphs <- to.network(edges.list, alters.list)
  frags <- lapply(graphs, FUN = 
                    function(x) igraph::clusters(x)$no)
  data.frame(fragmentations = unlist(frags))$fragmentations
}

