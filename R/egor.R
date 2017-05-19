#' egor - a data class for ego-centered network data.
#'
#' The function \code{egor()} is used to create an egor object from
#' ego-centered network data.
#' @param alters.df either a \code{data.frame} containing the alters
#'   (whose nominator is identified by the column specified by `egoID`
#'   or a list of data frames with the same columns, one for each ego,
#'   with empty data frames or `NULL`s corresponding to egos with no
#'   nominees.
#' @param egos.df \code{data.frame} containing the egos.
#' @param alter_ties.df \code{data.frame} containing the alter-alter
#'   relations in the style of an edge list, or a list of data frames
#'   similar to `alters.df`.
#' @param egoID Name of ego ID variable; optional if `alters.df` and
#'   `alter_ties.df` are both lists of data frames.
#' @param max.netsize Optional parameter. Constant value used if the
#'   number of alters whose relations were collected is limited.
#' @return returns an \code{egor} object. An egor is a
#'   \code{data.frame}, which consists of an ego ID column, nested
#'   columns for alter and alte-alter tie data and regular columns for
#'   ego-level data.
#' @details If parameters `alters.df`, `egos.df`, and `alter_ties.df`
#'   are data frames, they need to share a common ego ID variable,
#'   with corresponding values. If they `alters.df` and
#'   `alter_ties.df` are lists of data frames, `egoID` is ignored and
#'   they are matched positionally with the rows of `egos.df`. Of the
#'   three parameters only `alters.df` is necessary to create an
#'   `egor` object, and `egos.df` and `alter_ties.df` are optional.
#' @keywords ego-centric network analysis
#' @examples
#'
#' @export
egor <- function(alters.df, egos.df = NULL, alter_ties.df = NULL, egoID="egoID") {
  # FUN: Inject empty data.frames with correct colums in to NULL cells in
  # $.alters and $.alter_ties
  inj_zero_dfs <- function(x, y) {
    null_entries <- sapply(x[[y]], is.null)
    zero_df <- x[[y]][!null_entries][[1]][0, ]
    zero_dfs <- lapply(x[[y]][null_entries], FUN = function(x) zero_df)
    x[null_entries, ][[y]] <- zero_dfs
    x
  }

  egor <-
    if(is.data.frame(alters.df)){
      alters_is_df <- TRUE
      # Create initial egor object from ALTERS
      tidyr::nest_(data = alters.df,
                   key_col = ".alters",
                   names(alters.df)[names(alters.df) != egoID]) # Select all but the egoID column for nesting
    }else{
      alters_is_df <- FALSE
      egoID <- ".egoIdx"
      tibble::tibble(.egoIdx=seq_along(alters.df),
                     .alters=lapply(alters.df, tibble::as_tibble))
    }
  
  # If specified add alter_ties data to egor
  if(!is.null(alter_ties.df)){
    alter_ties.tib <-
      if(is.data.frame(alter_ties.df)){
        tidyr::nest_(data = alter_ties.df,
                     key_col = ".alter_ties",
                     names(alter_ties.df)[names(alter_ties.df) != egoID])
      }else{
        tibble::tibble(.egoIdx=seq_along(alters.df),
                       .alter_ties=lapply(alter_ties.df, tibble::as_tibble))
      }
    
    egor <- dplyr::full_join(egor, alter_ties.tib, by = egoID)
    egor <- inj_zero_dfs(egor, ".alter_ties")
  }
  
  # If speciefied add ego data to egor
  if(!is.null(egos.df)){
    if(!alters_is_df) egos.df$.egoIdx <- seq_len(nrow(egor))

    egor <- dplyr::full_join(tibble::as_tibble(egos.df), egor, by = egoID)
    egor <- inj_zero_dfs(egor, ".alters")
  }
  
  # Check If egoIDs valid
  if (length(unique(egor[[egoID]])) < length(egor[[egoID]]))
    warning(paste(egoID, "values are note unique. Check your 'egos.df' data."))

  if(!alters_is_df) egor$.egoIdx <- NULL
  
  # Add meta attribute
  #  ----><----

  class(egor) <- c("egor", class(egor))
  egor
}

filter_egor <- function(egor, obj = c(".alters", ".alter_ties"), cond) {
  
}

#' @export
summary.egor <- function(object, ...) {
  # Network count
  nc <- nrow(object)
  
  # Average netsize
  nts <- sum(unlist(lapply(object$.alters, FUN = NROW))) / nc
  
  # Average density
  if(".alter_ties" %in% names(object)) dens <- sum(ego_density(object), na.rm = T) / nc else dens <- NULL
  
  cat(paste(nc, "Egos/ Ego Networks", "\nAverage Netsize", nts, "\n"))
  if(!is.null(dens)) cat(paste("Average Density", dens))

  # Meta Data

}

#' @export
#' @import tibble
print.egor <- function(object, ...) {
  class(object) <- class(object)[-seq_len(which(class(object)=="egor"))]
  print(as_tibble(object))
  print(attr(object,"ego.design"))
}

#' @export
as.egor <- function(x, ...) UseMethod("as.egor")
