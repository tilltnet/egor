#' Generate counting variables for each category of a variable. Only used by gen.egonet.agg().
#'
#' This function generates Variables counting the frequency of each category of a variable. To use on data aggregated from the long ego-centric network data format.
#' #' @param x A factor representing an alteri-attribute variable.
#' @keywords ego-centric network analysis
fun.count <- function(x) table(as.factor(x))

#' Generate proportional variables for each category of a variable. Only used by gen.egonet.agg().
#'
#' This function generates Variables of the proportional frequency of each category of a variable. To use on data aggregated from the long ego-centric network data format. #!# prop.table() produces errors here, when netsize and total of frequencies per variable do not align.
#' #' @param x A factor representing an alteri-attribute variable.
#' @keywords ego-centric network analysis
fun.prop <- function(x) prop.table(table(as.factor(x)))

#' Count category frequencies of an alter attribute for ego-centric-network data.
#'
#' This function counts the category frequencies of a variable representing alter attributes in ego-centric-network data.
#' @param long.df A 'long' dataframe with alteri/dyads in rows.
#' @param netsize Name of a variable in \code{broad} consisting of numerics for the network size of each network.
#' @param egoID Name of the network-/ ego-ID variable in \code{broad}.
#' @keywords ego-centric network analysis
### Function to aggregate data from the long format data, using the egoID as the break variable.
comp.cat.counts <- function(long.df, var, egoID = "egoID", fun = fun.count) {
  tmp_matrix <- aggregate(long.df[[var]], by = list(long.df[[egoID]]), FUN = fun)
  cat.counts <- data.frame(tmp_matrix[[2]])
  names(cat.counts) <- levels(long.df[[var]])
  cat.counts
}

comp.cat.counts.na <- function(cat.counts, netsize) {
  for(i in 1:ncol(cat.counts)) { 
    cat.counts[ , i] <- ifelse(is.na(netsize) | netsize == 0 | is.nan(cat.counts[ , i]), NA , cat.counts[ , i])
  }
  cat.counts
}

#' Calculate the EI-Index of ego-centric networks.
#'
#' This function calculates the EI-Index of ego-centric networks using a category-count-dataframe usually generated with gen.egonet.agg().
#' @param x A factor representing an alteri-attribute variable.
#' @keywords ego-centric network analysis
#' @export
comp.ei <- function(cat.counts, v_ego, netsize) {
  HM <- NA
  for(i in 1:ncol(cat.counts)) { 
    HM <- ifelse(names(cat.counts[i]) == as.character(v_ego), cat.counts[[i]], HM)
  }
  HM <- ifelse(is.na(netsize) , NA , HM)
  HM <- ifelse(netsize == 0 , NA , HM)
  HT <- ifelse(is.na(netsize) , NA , netsize - HM)
  EI <- (HT - HM) / (HT + HM)
  EI
}

#' Calculate the network diversity of ego-centric networks for a given alter-attribute.
#'
#' This function calculates the network diversity of ego-centric networks for a given alter-attribute. 
#' @param cat.counts A category-count-dataframe usually generated with gen.egonet.agg().
#' @param netsize A vector of network sizes.
#' @keywords ego-centric network analysis
#' @export
comp.diversity <- function(cat.counts, netsize) {
  diversity <- 0
  for(i in 1:ncol(cat.counts)) {
    diversity <- ifelse(cat.counts[[i]] > 0, diversity + 1, diversity)
  }
  # NAs are set if diversity is zero or netsize is zero or NA.
  diversity <- ifelse(diversity == 0, NA , diversity)
  diversity <- ifelse(is.na(netsize), NA , diversity)
  diversity <- ifelse(netsize == 0, NA , diversity)  
  div_prop <- diversity/ncol(cat.counts)
  tmp_df <- data.frame(diversity, div_prop, check.names = F)
  names(tmp_df) <- c("diversity", "div_prop")
  tmp_df
}

#' All-in-one ego-centric network composition function.
#'
#' This function outputs a dataframe containg serveral compositional measures for ego-centric-network data.
#' @param long.df A 'long' dataframe with alteri/dyads in rows.
#' @param v_alt A character naming the variabe containg the alter-attribute.
#' @param netsize A vector of network sizes.
#' @param v_ego A character naming the variable containg the ego-attribute. Only needed if EI-Index should be calculated. Caution: Variable of v_alt and v_ego have to correspond.
#' @param mode A character. "regular" for a basic output, "all" for a complete output.
#' @keywords ego-centric network analysis
#' @export
composition <- function (long.df, v_alt, netsize, egoID = "egoID", v_ego = NULL, mode = "regular") { # regular, all
  ## Generate category counts/ proportions.
  cat_counts <- comp.cat.counts(long.df, var = v_alt, fun = fun.count, egoID = egoID)
  cat_counts_prop <- comp.cat.counts(long.df, var = v_alt, fun = fun.prop , egoID = egoID)
  names(cat_counts_prop) <- paste("prop", colnames(cat_counts_prop), sep = "_")
  
  ## Insert NAs, when netsize is zero or NA.
  cat_counts <- comp.cat.counts.na(cat_counts, netsize)
  cat_counts_prop <- comp.cat.counts.na(cat_counts_prop, netsize)
  
  ## Switcher for regular and all
  if(mode == "all") tmp_df <- data.frame(cat_counts, cat_counts_prop, check.names = F)
  if(mode != "all") tmp_df <- data.frame(cat_counts_prop, check.names = F)
    
  ## If v_ego is not empty calculte EI and include it in output/ tmp_df
  if(!is.null(v_ego)) {
    EI <- comp.ei(cat_counts, v_ego, netsize)
    tmp_df <- data.frame(tmp_df, EI, check.names = F)
  }
  
  ## Calculate diversity count/ proportions
  diversity <- comp.diversity(cat_counts, netsize)
  
  data.frame(tmp_df, diversity, check.names = F)
}