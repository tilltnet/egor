#' Calculate the EI-Index
#'
#' The EI-Index is the division of the intra-group edge density and the outer-group edge 
#' density. It can be calculated for the whole network and for subgroups. The
#' whole network EI is a metric indicating the tendency of a network to be 
#' clustered by the categories of a given factor variable. The EI value of a 
#' groups describes the tendency of a group to be connected or not connected 
#' to other groups. Additionally, the EI index can be employed as a measurment
#' for egos tendendy to homo-/heterphily - use the \code{composition} command
#' for individual EI-Index.
#' @template object
#' @template aaties
#' @param var_name \code{Character} naming grouping variable.
#' @param egoID \code{Character} naming ego ID variable.
#' @param altID \code{Character} naming alter ID variable.
#' @template meth_dots
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
#' EI(alters32, edges32, var_name = "alter.sex", altID = "alterID")
#' @export
EI <- function(object, ...)
  UseMethod("EI", object)

#' @rdname EI
#' @export
EI.list <- function(object, aaties, var_name, egoID = "egoID", altID = '.altID', ...) {
  
  aaties_list <- aaties
  alters_list <- object
  
  # Function: calculating possible dyads between a given number of alters/ nodes.
  aaties_poss <- function(max.alters) { (max.alters ^ 2 - max.alters) / 2 }
  
  # Function for calculating the possible internal and external aaties.
  grp_aaties_pos <- function(alters, var_name) {
    
    # Select only those alters for which the variable in question was collected.
    alters <- alters[!is.na(alters[[var_name]]), ]
    
    # 
    alters_groups <- split(alters, alters[[var_name]])
    tble_var <- sapply(alters_groups, FUN = NROW)
    poss_internal <- sapply(tble_var, FUN=aaties_poss, simplify = TRUE)
    
    poss_external <- sapply(tble_var, FUN=function(x) {
      poss_ext_aaties <- (NROW(alters) - x) * x
    })
    
    list(poss_internal = poss_internal, poss_external = poss_external)
  }
  
  # Classify a single edge as heterogen or homogen.
  classify_aatie <- function(edge, alters) {
    if ('Source' %in% names(edge)) {
      source_ <- edge$Source
      target_ <- edge$Target
    } else {
      source_ <- as.numeric(as.character(edge[1,1]))
      target_ <- as.numeric(as.character(edge[1,2]))
    }
    hm_ht <- ifelse(alters[as.numeric(as.character(alters[[altID]])) == source_, ][[var_name]] == 
                    alters[as.numeric(as.character(alters[[altID]])) == target_, ][[var_name]], 'HM', 'HT')
    hm_ht
  }
  
  # Calculate group and network EIs.
  lists_to_EIs <- function(aaties, alters, altID) {
    print(alters$alterID.1)
    # Return na.df for 'incomplete' networks
    if(NROW(aaties)<1 | !NROW(alters)>1 ) return(na.df)
    if(length(table(factor(alters[[var_name]]))) < 2) return(na.df)
    if(sum(!is.na(alters[[var_name]])) < 2) return(na.df)
    
    # Make sure alters are sorted and there is a useful altID
    alters <- alters[order(alters[[altID]]), ]

    # Function for calulation EI.
    calc.EI <- function(E, I) {(E-I)/(E+I)}
   
     # Classify all aaties as homogen, or heterogen.
    hm_hts <- plyr::adply(aaties, .margins = 1, .fun = classify_aatie, alters)
    hm_hts_ <- factor(rev(hm_hts)[[1]], levels = c('HM', 'HT'))
    tble_hm_hts <- table(hm_hts_)

    # Calculate regular EI for whole network.
    EIs <- as.numeric(calc.EI(tble_hm_hts['HT'], tble_hm_hts['HM']))
    
    # Get possible aaties for all groups (internal and external).
    poss_int_ext <- grp_aaties_pos(alters, var_name)
    
    # Count internal and external aaties for all groups.
    int_ext <- list()
    var_levels <- levels(alters[[var_name]])
    for(i in 1:length(var_levels)) {
      
      alters_ids <- alters[altID][ alters[[var_name]] == var_levels[[i]] , ]
      if (class(alters_ids)[[1]] == "tbl_df") alters_ids <- alters_ids[[1]]
      
      if ('Source' %in% names(hm_hts)) {
        grp_aaties <- hm_hts[hm_hts$Source %in%  alters_ids | hm_hts$Target %in%  alters_ids, ]
      } else {
        grp_aaties <- hm_hts[hm_hts[1,1] %in%  alters_ids | hm_hts[1,2] %in%  alters_ids, ]
      }
      grp_aaties <- rev(grp_aaties)[[1]]
      grp_aaties <- factor(grp_aaties, c("HM", "HT"))
      tble_grp_aaties <- table(grp_aaties)
      int_ext[['HM']][[var_levels[[i]]]] <- tble_grp_aaties['HM']
      int_ext[['HT']][[var_levels[[i]]]] <- tble_grp_aaties['HT']
    } 
    # Dichte für alle Gruppen berechnen. 
    densities <- mapply(function(x,y) x/y, int_ext, poss_int_ext)
    
    # Calculate group EIs, controlled by group-size.
    group_EIs <- calc.EI(densities[, 2], densities[, 1])
    
    # Average of group EIs.
    #avg_net_EIs <-  sum(group_EIs)/length(var_levels)
    
    # Calculate possible external aaties for whole network.
    poss_all <- aaties_poss(NROW(alters))
    poss_ext_all <- poss_all - sum(poss_int_ext$poss_internal)
    
    # Calculate size controlled EI for whole network.
    sc_i <- (sum(densities[, 1]) / length(var_levels))
    sc_e <- (as.numeric(tble_hm_hts['HT']) / poss_ext_all)
    net_EIs_sc <- calc.EI(sc_e, sc_i)
    
    if(NROW(aaties)<1 | !NROW(alters)>1 ) return(na.df)
    
    # Return data.frame with all EIs.
    data.frame(EI = EIs, sc_EI = net_EIs_sc, t(group_EIs))
    #data.frame(EI = net_EIs_sc, t(group_EIs))
  }
  
  
  
  # Create NA data-frame row for networks with missing data or only a single group
  alters <- do.call(rbind, alters_list)
  na.df <- data.frame(t(c(EI = NA, sc_EI = NA, rep(NA, nlevels(factor(alters[[var_name]]))))))
  glob_levels <- c(names(na.df)[1:2], levels(factor(alters[[var_name]])))
  names(na.df) <- glob_levels
  na.df <- data.frame(na.df)
  
  # Cast factor() on non factor group vars
  if(!is.factor(alters[[var_name]])) 
    alters_list <- lapply(alters_list, FUN = function(x) {
      x[[var_name]] <- factor(x[[var_name]], levels = levels(factor(alters[[var_name]])))
      x
    })
  
  # Invoke list_to_EIs
  EIs <- mapply(FUN = lists_to_EIs, 
                aaties_list, 
                alters_list, 
                MoreArgs = list(altID = altID), 
                SIMPLIFY = FALSE)

  lapply(EIs, FUN = function(x) 
        colnames(x) <- colnames(na.df))
  
  res <- do.call(rbind, EIs)
  res[2:NCOL(res)]
}

#' @rdname EI
#' @export
EI.egor <- function(object, var_name, egoID = "egoID", altID = '.altID', ...) {
  EI(object = object$.alts, aaties = object$.aaties,  var_name = var_name, egoID = egoID, altID = altID)
}

#' @rdname EI
#' @export
EI.data.frame <- function(object, aaties, var_name, egoID = "egoID", altID = '.altID', ...) {
  aaties_list <- split(aaties, as.numeric(aaties[[egoID]]))
  alters_list <- split(object, as.numeric(object[[egoID]]))
  EI(object = alters_list, aaties = aaties_list, var_name = var_name, egoID = egoID, altID = altID)
}




