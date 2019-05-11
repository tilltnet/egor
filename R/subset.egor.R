#' Convert a table to a list of rows
#'
#' A convenience function converting a [data.frame()] or a [tibble()].
#'
#' @param x a [data.frame()], a [tibble()], or some other table data
#'   structure backed by a [list()] of columns.
#'
#' @return A [list()] of length `nrow(x)`, with each element itself a
#'   named [list()] containing the elements in the corresponding
#'   row.
#'
#' @examples
#'
#' library(tibble)
#' (df <- tibble(x=2:1, y=list(list(1:3), list(3:4))))
#' rowlist(df)
#'
#' @export
rowlist <- function(x){
  apply(x, 1, identity)
}


#' Filter and Subset Ego-centered Datasets
#'
#' Functions to index and take subsets of [egor()] objects: manipulate
#' egos, alters, or alter-alter ties.
#'
#' @param x an [egor()] object.
#' @param unit a selector of the unit of analysis being affected: the
#'   egos, the alters or the (alter-alter) ties. Note that only one
#'   type of unit can be affected at a time. Defaults to the current
#'   active unit selected by [activate.egor()].
#'
#' @param subset either an expression evaluated on each of the rows of
#'   the selected unit (as in the eponymous argument of [subset()]) or
#'   a function whose first argument is a row, specifying which egos,
#'   alters, or alter-alter ties to keep. The expressions can access
#'   variables in the calling environment; columns of the active unit,
#'   columns of other units with which the active unit shares an ego
#'   via `egos$`, `alters$`, and `aaties$` as well as the following
#'   "virtual" columns to simplify indexing: \describe{
#' 
#' \item{Ego index `.egoRow`}{ contains the index (counting from 1) of the row being
#' evaluated. (This can be used to access vector variables in the
#' calling environment.)}
#' 
#' \item{Alter index `.altRow`}{ contains the index (counting from 1) of the row number in the alter table.}
#' 
#' \item{Alter--alter indices `.srcRow` and `.tgtRow`}{ contain the
#' index (counting from 1) of the row of the alter being refereced by
#' `.srcID` and `.tgtID`. (This can be used to quickly access the
#' attributes of the alters in question.)}
#' 
#' }
#' 
#' @param ... extra arguments to `subset` if `subset` is a function; otherwise unused.
#'
#' @details 
#'
#' Removing or duplicating an ego will also remove or duplicate their
#' alters and ties.
#'
#' @return An [egor()] object.
#'
#' @examples
#'
#' # Generate a small sample dataset
#' (e <- make_egor(5,4))
#'
#' # First three egos in the dataset
#' e[1:3,]
#'
#' # Similarly with subset()
#' subset(e, .egoRow <= 3)
#'
#' # Using an external vector
#' # (though normally, we would use e[.keep,] here)
#' .keep <- rep(c(TRUE, FALSE), length.out=nrow(e))
#' subset(e, .keep[.egoRow])
#' # a more robust version of the above: pass a function of row and
#' # keep (which is passed as an additional argument to the function):
#' subset(e, function(r, keep) keep[r$.egoRow], .keep)
#'
#' # Only keep egos with exactly three alters
#' subset(e, nrow(.alts)==3)
#'
#' # Only keep egos with exactly two female alters
#' subset(e, sum(.alts$sex=="w")==2)
#'
#' # Only keep female alters
#' subset(e, .alts$sex=="w", unit="alter")
#'
#' # Only keep alters of a different sex form ego
#' subset(e, sex != .alts$sex, unit="alter")
#'
#' # Only keep homophilous alter-alter ties
#' subset(e, .alts$sex[.aaties$.srcRow] ==
#'           .alts$sex[.aaties$.tgtRow],
#'        unit="aatie")
#'
#' @importFrom methods is
#' @importFrom dplyr nest_join
#' @export
subset.egor <- function(x, subset, ..., unit = attr(x, "active")){
  unit <- match.arg(unit, UNITS)
  f <- try(is.function(subset), silent=TRUE)
  if(is(f, "try-error") || !f){
    se <- substitute(subset)
    pf <- parent.frame()
    f <- function(r) eval(se, r, pf)
  }else f <- subset

  # The following works because we end up joining the same set of
  # tables to any of these, except that for unit=="aatie", we also
  # need to add row indices.

  xa <- switch(unit,
               ego = bind_cols(x$egos, .egoRow=seq_len(nrow(x$egos))),
               alter = 
                 # Within each ego, assign increasing alter
                 # indices. Note that the first argument of ave() is a
                 # dummy variable.
                 bind_colsx(x$alters, .altRow=ave(integer(nrow(x$alters)), x$alters$.egoID, seq_along)),
               aatie = x$aaties)

  xa <- nest_join(xa, bind_cols(x$egos, .egoRow=seq_len(nrow(x$egos))), ".egoID", keep=TRUE, name="egos")
  xa <- nest_join(xa, x$alters, ".egoID", keep=TRUE, name="alters")
  xa$alters <- lapply(x$alters, function(a) bind_rows(a, .altRow=seq_len(nrow(a))))
  xa <- nest_join(xa, x$aaties, ".egoID", keep=TRUE, name="aaties")
  xa$aaties <- mapply(function(a,aa)
    bind_cols(aa,
              .srcRow = match(aa$.srcID, a$.altID),
              .tgtRow = match(aa$.tgtID, a$.altID)),
    a=xa$alters, aa=xa$aaties, SIMPLIFY=FALSE)

  if(unit=="aatie"){
    xa$.srcRow <- mapply(function(a,aa) match(.srcID, a$.altID),
                         a=xa$alters, aa=xa$.srcID, SIMPLIFY=TRUE)
    xa$.tgtRow <- mapply(function(a,aa) match(.tgtID, a$.altID),
                         a=xa$alters, aa=xa$.tgtID, SIMPLIFY=TRUE)
  }
  
  # Call the function to perform indexing
  i <- lapply(rowlist(xa), f, ...)

  x[i,,unit=unit]
}

#' @rdname subset.egor
#'
#' @param i numeric or logical vector indexing the appropriate unit.
#'
#' @param j either an integer vector specifying which columns of the
#'   filtered structure (ego, alters, or ties) to select, or a logical
#'   vector specifying which columns to keep. Note that the special
#'   columns \Sexpr{sQuote(unlist(IDVARS))} are not indexed by `j`.
#'
#' @import tibble
#' @export
`[.egor` <- function(x, i, j, unit = attr(x, "active"), ...){
  unit <- match.arg(unit, UNITS)
  if(missing(i)) i <- TRUE
  if(missing(j)) j <- TRUE

  switch(unit,
         ego = {
           # This guarantees that the ego ID column is always preserved.
           x$egos <- bind_cols(x$egos[,seq_len(ncol(x$egos)-1),drop=FALSE][i,j,drop=FALSE, ...], x$egos[i,ncol(x$egos),drop=FALSE])
           x$alters <- filter(x$alters, .egoID %in% x$egos$.egoID)
           x$aaties <- filter(x$aaties, .egoID %in% x$egos$.egoID)
           attr(x, "ego_design") <- attr(x, "ego_design")[i,]
           x
         },
         alter = {
           x$alters <- bind_cols(x$alters[,seq_len(ncol(x$alters)-2),drop=FALSE][i,j,drop=FALSE, ...], x$alters[i,ncol(x$alters)-2L+1:2,drop=FALSE])
           # Explanation: keep a row in aaties iff its (egoID,altID) tuple can (still) be found in the alters as well.
           x$aaties <- filter(x$aaties, mapply(list, .egoID, .altID, SIMPLIFY=FALSE) %in% mapply(list, x$alters$.egoID, x$alters$.altID, SIMPLIFY=FALSE))
           x
         },
         aatie = {
           if(!hasName(x,"aaties"))
             stop("Attempted indexing of alter-alter ties on an object with no alter-alter ties observed.")
           x$aaties <- bind_cols(, x$aaties[,seq_len(ncol(x$aaties)-3),drop=FALSE][i,j,drop=FALSE, ...], x$aaties[i,ncol(x$aaties)-3L+1:3,drop=FALSE])
           x
         })
}
