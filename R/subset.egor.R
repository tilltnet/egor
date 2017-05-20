## FIXME: Filtering alters does not filter corresponding ties at this
## time.


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
#' @param aspect a selector of which aspect of an egocentric dataset
#'   to affect: the egos, the alters or the (alter-alter) ties. Note
#'   that only one can be affected at a time.
#'
#' @param subset an expression evaluated on each of the rows of [egor()]
#'   (as in the eponymous argument of [subset()]), specifying which
#'   egos, alters, or alter-alter ties to keep; output format depends
#'   on `aspect`: \describe{
#'
#' \item{`"egos"`}{a single logical value specifying whether the ego
#' should be kept.}
#'
#' \item{`"alters"`}{either an integer vector of indices specifying
#' which alters to select or a logical vector of length `nrow(x)`
#' specifying which alters should be kept.}
#'
#' \item{`"ties"`}{either an integer vector of indices specifying
#' which alter-alter ties to select or a logical vector of length
#' `nrow(x)` specifying which alter-alter ties should be kept.}
#' 
#' }
#' the expressions can access variables in the calling environment;
#' columns of [egor()] as variables (which mask the variables in the
#' environment), as well as a "virtual" column `.egoIdx` which
#' contains the index (counting from 1) of the row being
#' evaluated. (This can be used to access vector variables in the
#' calling environment.)
#'
#' @param ... extra arguments; currently unused.
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
#' subset(e, .egoIdx <= 3)
#'
#' # Using an external vector
#' # (though normally, we would use e[.keep,] here)
#' .keep <- rep(c(TRUE, FALSE), length.out=nrow(e))
#' subset(e, .keep[.egoIdx])
#'
#' # Only keep egos with exactly three alters
#' subset(e, nrow(.alters)==3)
#'
#' # Only keep egos with exactly two female alters
#' subset(e, sum(.alters$alter.sex=="w")==2)
#'
#' # Only keep female alters
#' subset(e, .alters$alter.sex=="w", aspect="alters")
#'
#' # Only keep alters of a different sex form ego
#' subset(e, sex != .alters$alter.sex, aspect="alters")
#'
#' # Only keep homophilous alter-alter ties
#' subset(e, .alters$alter.sex[.alter_ties$Source] ==
#'           .alters$alter.sex[.alter_ties$Target],
#'        aspect="ties")
#' 
#' @export
subset.egor <- function(x, subset, aspect = c("egos","alters","ties"), ...){
  aspect <- match.arg(aspect)
  se <- substitute(subset)
  pf <- parent.frame() 
  f <- function(r) eval(se, r, pf)

  i <- lapply(rowlist(cbind(x,.egoIdx=seq_len(nrow(x)))), f)

  x[i,,aspect=aspect]
}

#' @rdname subset.egor
#'
#' @param i depends on `aspect`: \describe{
#'
#' \item{`"egos"`}{either an integer vector of indices specifying
#' which egos to select or a logical vector of length `nrow(x)`
#' specifying which rows should be kept; a logical list of length
#' `nrow(x)` is acceptable as well.}
#'
#' \item{`"alters"`}{a ragged array (a [list()]) of length `nrow(x)`,
#' either of integer vectors of indices specifying which alters to
#' select for the corresponding ego or of logical vectors of length
#' `nrow(x$.alters[k,,drop=FALSE])` specifying which alters should be kept.}
#'
#' \item{`"ties"`}{a ragged array (a [list()]) of length `nrow(x)`,
#' either of integer vectors of indices specifying which alter-alter
#' ties to select for the corresponding ego or of logical vectors of
#' length `nrow(x$.alter_ties[k,,drop=FALSE])` specifying which ties
#' should be kept}
#'
#' }
#'
#' In general, constructing selection arrays for alters and ties is
#' complicated and error-prone, so the use of [subset()] is
#' recommended.
#' 
#' @param j either an integer vector specifying which columns of the
#'   filtered structure (egos, alters, or ties) to select, or a
#'   logical vector specifying which columns to keep.
#'
#' @import tibble
#' @importFrom utils getS3method
#' @export
`[.egor` <- function(x, i, j, aspect = c("egos","alters","ties"), ...){
  aspect <- match.arg(aspect)
  if(missing(i)) i <- TRUE
  if(missing(j)) j <- TRUE

  switch(aspect,
         egos = {
           # Subset using the tibble's method, then copy over all
           # attributes except for the ones that could have changed.
           if(is.list(i)) i <- unlist(i)
           bracket <- getS3method("[", "tbl_df")
           xt <- bracket(x,i,j,drop=FALSE)
           if(! ".alters"%in%names(xt)) xt <- cbind(xt, .alters=bracket(x,,".alters"))
           if(".alter_ties"%in%names(x) && ! ".alter_ties"%in%names(xt)) xt <- cbind(xt, .alter_ties=bracket(x,,".alter_ties"))
           for(a in setdiff(names(attributes(x)), c("ego.design", "names", "row.names")))
             attr(xt, a) <- attr(x, a)
           attr(xt, "ego.design") <- attr(x, "ego.design")[i,]
           xt
         },
         alters = {
           x$.alters <- lapply(seq_len(nrow(x)), function(k){
             x$.alters[[k]][i[[k]],j,drop=FALSE]
           })
           x
         },
         ties = {
           x$.alter_ties <- lapply(seq_len(nrow(x)), function(k){
             x$.alter_ties[[k]][i[[k]],j,drop=FALSE]
           })
           x
         })
}
