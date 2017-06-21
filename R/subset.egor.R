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


#' Filter and Subset Egocentric Datasets
#'
#' Functions to index and take subsets of [egor()] objects: manipulate
#' egos, alters, or alter-alter ties.
#'
#' @param x an [egor()] object.
#' @param unit a selector of the unit of analysis being affected: the
#'   egos, the alters or the (alter-alter) ties. Note that only one
#'   type of unit can be affected at a time.
#'
#' @param subset either an expression evaluated on each of the rows of
#'   [egor()] (as in the eponymous argument of [subset()]) or a
#'   function whose first argument is a row, specifying which egos,
#'   alters, or alter-alter ties to keep; output format depends on
#'   `unit`: \describe{
#'
#' \item{`"ego"`}{a single logical value specifying whether the ego
#' should be kept.}
#'
#' \item{`"alter"`}{either an integer vector of indices specifying
#' which alters to select or a logical vector of length `nrow(x)`
#' specifying which alters should be kept.}
#'
#' \item{`"aatie"`}{either an integer vector of indices specifying
#' which alter-alter ties to select or a logical vector of length
#' `nrow(x)` specifying which alter-alter ties should be kept.}
#' 
#' }
#' the expressions can access variables in the calling environment;
#' columns of [egor()] as variables (which mask the variables in the
#' calling environment), as well as the following "virtual" columns to simplify indexing:
#' \describe{
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
#' (e <- generate.sample.ego.data(5,4))
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
#' subset(e, sum(.alts$alter.sex=="w")==2)
#'
#' # Only keep female alters
#' subset(e, .alts$alter.sex=="w", unit="alter")
#'
#' # Only keep alters of a different sex form ego
#' subset(e, sex != .alts$alter.sex, unit="alter")
#'
#' # Only keep homophilous alter-alter ties
#' subset(e, .alts$alter.sex[.aaties$.srcRow] ==
#'           .alts$alter.sex[.aaties$.tgtRow],
#'        unit="aatie")
#'
#' @importFrom methods is
#' @export
subset.egor <- function(x, subset, ..., unit = c("ego","alter","aatie")){
  unit <- match.arg(unit)
  f <- try(is.function(subset), silent=TRUE)
  if(is(f, "try-error") || !f){
    se <- substitute(subset)
    pf <- parent.frame()
    f <- function(r) eval(se, r, pf)
  }else f <- subset
  
  ## egor object augmented with extra columns
  # Copy and add an .egoRow column
  xa <- cbind(x,.egoRow=seq_len(nrow(x)))
  # Add an .altRow column to each alter
  xa$.alts <- lapply(xa$.alts, function(a) cbind(a, .altRow=seq_len(nrow(a))))
  # Add an .srcRow and .tgtRow column to each alter-alter table
  xa$.aaties <- mapply(function(a,aa)
    cbind(aa,
          .srcRow = match(aa$.srcID, a$.altID),
          .tgtRow = match(aa$.tgtID, a$.altID)),
    a=xa$.alts, aa=xa$.aaties, SIMPLIFY=FALSE)

  # Call the function to perform indexing
  i <- lapply(rowlist(xa), f, ...)

  x[i,,unit=unit]
}

#' @rdname subset.egor
#'
#' @param i depends on `unit`: \describe{
#'
#' \item{`"ego"`}{either an integer vector of indices specifying
#' which egos to select or a logical vector of length `nrow(x)`
#' specifying which rows should be kept; a logical list of length
#' `nrow(x)` is acceptable as well.}
#'
#' \item{`"alter"`}{a ragged array (a [list()]) of length `nrow(x)`,
#' either of integer vectors of indices specifying which alters to
#' select for the corresponding ego or of logical vectors of length
#' `nrow(x$.alts[k,,drop=FALSE])` specifying which alters should be kept.}
#'
#' \item{`"aatie"`}{a ragged array (a [list()]) of length `nrow(x)`,
#' either of integer vectors of indices specifying which alter-alter
#' ties to select for the corresponding ego or of logical vectors of
#' length `nrow(x$.aaties[k,,drop=FALSE])` specifying which ties
#' should be kept}
#'
#' }
#'
#' In general, constructing selection arrays for alters and ties is
#' complicated and error-prone, so the use of [subset()] is
#' recommended.
#' 
#' @param j either an integer vector specifying which columns of the
#'   filtered structure (ego, alters, or ties) to select, or a
#'   logical vector specifying which columns to keep.
#'
#' @import tibble
#' @importFrom utils getS3method
#' @export
`[.egor` <- function(x, i, j, unit = c("ego","alter","aatie"), ...){
  unit <- match.arg(unit)
  if(missing(i)) i <- TRUE
  if(missing(j)) j <- TRUE

  switch(unit,
         ego = {
           # Subset using the tibble's method, then copy over all
           # attributes except for the ones that could have changed.
           if(is.list(i)) i <- unlist(i)
           bracket <- getS3method("[", "tbl_df")
           xt <- bracket(x,i,j,drop=FALSE)
           if(".alts"%in%names(x) && ! ".alts"%in%names(xt)) xt <- cbind(xt, .alts=bracket(x,i,".alts"))
           if(".aaties"%in%names(x) && ! ".aaties"%in%names(xt)) xt <- cbind(xt, .aaties=bracket(x,i,".aaties"))
           for(a in setdiff(names(attributes(x)), c("ego.design", "names", "row.names")))
             attr(xt, a) <- attr(x, a)
           attr(xt, "ego.design") <- attr(x, "ego.design")[i,]
           xt
         },
         alter = {
           x$.alts <- mapply(function(a, ai){
             at <- a[ai,j,drop=FALSE]
             if(".altID"%in%names(a) && ! ".altID"%in%names(at)) at <- cbind(at, .altID=a$.altID[ai])
             at
           }, a=x$.alts, ai=i, SIMPLIFY=FALSE)
           x$.aaties <- mapply(function(a, aa){
             aa[aa$.srcID %in% a$.altID & aa$.tgtID %in% a$.altID,
                j,drop=FALSE]
           }, a=x$.alts, aa=x$.aaties, SIMPLIFY=FALSE)
           x
         },
         aatie = {
           x$.aaties <- mapply(function(aa, aai){
             aat <- aa[aai,j,drop=FALSE]
             if(! ".srcID"%in%names(aat)) aat <- cbind(aat, .srcID=aa$.srcID[aai])
             if(! ".tgtID"%in%names(aat)) aat <- cbind(aat, .tgtID=aa$.tgtID[aai])
             aat
           }, aa=x$.aaties, aai=i, SIMPLIFY=FALSE)
           x
         })
}
