#' Generate counting variables for each category of a variable. Used by comp.cat.counts().
#'
#' This function generates Variables counting the frequency of each category of a variable. To use on data aggregated from the long ego-centered network data format.
#' @param x A factor representing an alters-attribute variable.
#' @keywords internal
fun.count <- function(x) table(as.factor(x))

#' Generate proportional variables for each category of a variable. Used by comp.cat.counts().
#'
#' This function generates Variables of the proportional frequency of each category of a variable. To use on data aggregated from the long ego-centered network data format. #!# prop.table() produces errors here, when netsize and total of frequencies per variable do not align.
#' @param x A factor representing an alters-attribute variable.
#' @keywords internal
fun.prop <- function(x) prop.table(table(as.factor(x)))

#' Count category frequencies of an alter attribute for ego-centered-network data.
#'
#' This function counts the category frequencies (absolute or proportional) of a variable representing alter attributes in ego-centered-network data.
#' @param alters A 'long' dataframe with alters in rows.
#' @param var Alter attribute which categories are to be counted.
#' @param egoID \code{Character} giving the name of the variable identifying 
#' egos (default = "egoID") in \code{alters}.
#' @param fun Function to be used for counting. \code{fun.count} for absolute counts \code{fun.prop} propotional counts.
#' @return Returns a \code{dataframe} with counts of all categories as variables.
#' @keywords internal
### Function to aggregate data from the long format data, using the egoID as the break variable.
comp.cat.counts <- function(alters, var, egoID = "egoID", fun = fun.count) {
  # If var is not a factor it has to be coerced to one. Doing this to a variable
  # alrerady being a factor would strip away the levels.
  if(!is.factor(alters[[var]])) {
    tmp_matrix <- aggregate(as.factor(alters[[var]]), by = list(alters[[egoID]]), FUN = fun)    
  } else {
    tmp_matrix <- aggregate(alters[[var]], by = list(alters[[egoID]]), FUN = fun)
  }
  cat.counts <- data.frame(egoID = tmp_matrix$Group.1, tmp_matrix[[2]])
  if(!is.factor(alters[[var]])) {
    names(cat.counts) <- c(egoID, trimws(levels(factor(alters[[var]]))))
  } else {
    names(cat.counts) <- c(egoID, trimws(levels(alters[[var]])))
  }
  cat.counts
}

#' Set compositional results to NA if netsize is missing (NA), zero or not a number (NaN).
#'
#' This function sets compositional results to NA if netsize is missing (NA), zero or not a number (NaN).
#' @param cat.counts Results of the function comp.cat.counts
#' @param netsize Name of a variable in \code{broad} consisting of numerics for the network size of each network.
#' @keywords internal
### Function to aggregate data from the long format data, using the egoID as the break variable.
comp.cat.counts.na <- function(cat.counts, netsize) {
  for(i in 1:ncol(cat.counts)) { 
    cat.counts[ , i] <- ifelse(is.na(netsize) | netsize == 0 | is.nan(cat.counts[ , i]), NA , cat.counts[ , i])
  }
  cat.counts
}

#' Calculate the homophily of ego-centered networks.
#'
#' This function calculates the EI-Index of ego-centered networks using a "category-count-dataframe" usually generated with comp.cat.counts(). The EI-Index ranges from -1 to 1, where a positive value represents a heterogenous personal network in respect to a specified attribute of ego an its alters.
#' @param cat.counts Results of the function comp.cat.counts
#' @param v_ego variable of an ego attribute corresponding to the 
#' @param netsize Name of a variable in \code{broad} consisting of numerics for the network size of each network.
#' @return EI Index values per ego/ network in a \code{dataframe}.
#' @keywords internal
comp.homophily <- function(cat.counts, v_ego, netsize) {
  HM <- NA
  for(i in 1:ncol(cat.counts)) { 
    HM <- ifelse(names(cat.counts[i]) == as.character(v_ego), cat.counts[[i]], HM)
  }
  HM <- ifelse(is.na(netsize) , NA , HM)
  HM <- ifelse(netsize == 0 , NA , HM)
  HT <- ifelse(is.na(netsize) , NA , netsize - HM)
  homophily <- (HT - HM) / (HT + HM)
  homophily
}

#' Calculate the network diversity of ego-centered networks for a given alter-attribute.
#'
#' This function calculates the network diversity of ego-centered networks for a given alter-attribute. 
#' @param cat.counts A category-count-dataframe usually generated with comp.cat.counts().
#' @param netsize A vector of network sizes.
#' @return Returns the absolute and proportional count of unique categories per ego/ network in a \code{dataframe}.
#' @keywords internal
comp.diversity <- function(cat.counts, netsize) {
  diversity <- 0
  for(i in 2:ncol(cat.counts)) {
    diversity <- ifelse(cat.counts[[i]] > 0, diversity + 1, diversity)
  }
  # NAs are set if diversity is zero or netsize is zero or NA.
  diversity <- ifelse(diversity == 0, NA , diversity)
  diversity <- ifelse(is.na(netsize), NA , diversity)
  diversity <- ifelse(netsize == 0, NA , diversity)  
  div_prop <- diversity/ncol(cat.counts)
  tmp_df <- data.frame(diversity, div_prop, check.names = FALSE)
  names(tmp_df) <- c("diversity", "div_prop")
  tmp_df
}

#' Calculate compositional measurements (Proportion, Diversity, EI-Index)
#'
#' This function outputs a dataframe containg serveral compositional measures 
#' for ego-centered-network data. It reports the proportion of each group in the
#' network, the absolute count of groups present and, if provided the 
#' corresponding ego attribute, the EI-Index is employed as a measurment for ego's tendency
#' to homo-/heterophily.
#' @param object An egor object, a list or data.frame of alter attributes.
#' @param v_alt A character naming the variable containg the alter-attribute.
#' @template netsize
#' @template egoID
#' @param v_ego Character vector containing the ego attribute. Only needed for homophily index (EI). Caution: Levels of v_alt and v_ego need to correspond (see Details).
#' @param mode A character. "regular" for a basic output, "all" for a complete output.
#' @return Returns a dataframe with category counts, diversity and EI-Index values in an ego-centered network for a provided alter attribute.
#' @details v_ego is expected to consist of one entry per ego. The ego 
#' attributes are usually drawn from the ego dataframe. If the ego attribute is
#' stored alongside the alters data, make sure to drop repeated values per 
#' alters (see example two).
#' @keywords ego-centered network analysis
#' @examples
#' # Load example data
#' data("egos32")
#' data("alters32")
#' 
#' # Example one
#' composition(alters32, v_alt = "alter.sex", netsize = egos32$netsize, 
#'             v_ego = egos32$sex)
#' 
#' # Example two
#' # - using an ego attribute stored in the alter dataframe.
#' ego.sex <- alters32[!duplicated(alters32$egoID), ]$ego.sex
#' res <- composition(alters32, v_alt = "alter.sex", netsize = egos32$netsize, 
#'                    v_ego = ego.sex)
#' 
#' @export
composition <- function (object, v_alt, netsize, egoID = "egoID", v_ego = NULL, mode = "regular") {
  UseMethod("composition", object)
}

#' @rdname composition
#' @export
composition.list <- function(object, v_alt, netsize, egoID = "egoID", v_ego = NULL, mode = "regular"){
  netsize <- netsize[netsize != 0]
  netsize <- netsize[!is.na(netsize)]
  v_ego <- v_ego[netsize != 0]
  v_ego <- v_ego[!is.na(netsize)]
  object <- do.call(rbind, object)
  NextMethod()
}

#' @rdname composition
#' @export
composition.egor <- function(object, v_alt, netsize, egoID = "egoID", v_ego = NULL, mode = "regular") {
  netsize <- netsize[netsize != 0]
  netsize <- netsize[!is.na(netsize)]
  v_ego <- v_ego[netsize != 0]
  v_ego <- v_ego[!is.na(netsize)]
  object <- as_alts_df(object, egoID = egoID)
  NextMethod()
}

#' @rdname composition
#' @export
composition.data.frame <- function (object, v_alt, netsize, egoID = "egoID", v_ego = NULL, mode = "regular") { # regular, all
  ## Generate category counts/ proportions.
  cat_counts <- comp.cat.counts(alters = object, var = v_alt, fun = fun.count, egoID = egoID)
  cat_counts_prop <- comp.cat.counts(object, var = v_alt, fun = fun.prop , egoID = egoID)
  cat_counts_prop <- cat_counts_prop[names(cat_counts_prop) != egoID]
  names(cat_counts_prop) <- paste("prop", colnames(cat_counts_prop), sep = "_")
  
  ## Insert NAs, when netsize is zero or NA.
  cat_counts <- comp.cat.counts.na(cat_counts, netsize)
  cat_counts_prop <- comp.cat.counts.na(cat_counts_prop, netsize)
  
  ## Switch for regular and all
  if(mode == "all") tmp_df <- data.frame(cat_counts, cat_counts_prop, check.names = FALSE)
  if(mode != "all") tmp_df <- data.frame(cat_counts[egoID], cat_counts_prop, check.names = FALSE)
    
  ## If v_ego is not empty calculte EI and include it in output/ tmp_df
  if(!is.null(v_ego)) {
    #assign
    EI <- comp.homophily(cat_counts, v_ego, netsize)
    tmp_df <- data.frame(tmp_df, ego_EI = EI, check.names = FALSE)
    names(tmp_df)[ncol(tmp_df)] <- paste(v_alt, "EI", sep = "_")
  }
  
  ## Calculate diversity count/ proportions
  diversity <- comp.diversity(cat_counts, netsize)
  
  tmp_df <- data.frame(tmp_df, diversity, check.names = FALSE)
  names(tmp_df)[(ncol(tmp_df) - 1) : ncol(tmp_df)] <- c(paste(v_alt, "diversity", sep = "_"),
                                                        paste(v_alt, "div_prop", sep = "_")) 
  tmp_df
}
