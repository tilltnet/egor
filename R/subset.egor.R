## FIXME: Filtering alters does not filter corresponding ties at this
## time.

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
#' @param subset an expression evaluated on the columns of [egor()]
#'   (as in the eponymous argument of [subset()]), specifying which
#'   egos, alters, or alter-alter ties to keep; output format depends
#'   on `aspect`: \describe{
#'
#' \item{`"egos"`}{either an integer vector of indices specifying
#' which egos to select or a logical vector of length `nrow(x)`
#' specifying which rows should be kept.}
#'
#' \item{`"alters"`}{*for each ego's row*, either an integer vector of
#' indices specifying which alters to select or a logical vector of
#' length `nrow(x)` specifying which alters should be kept.}
#'
#' \item{`"ties"`}{*for each ego's row*, either an integer vector of
#' indices specifying which alter-alter ties to select or a logical
#' vector of length `nrow(x)` specifying which alter-alter ties should
#' be kept.}
#' 
#' }
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
#' @export
subset.egor <- function(x, subset, aspect = c("egos","alters","ties"), ...){
  aspect <- match.arg(aspect)
  se <- substitute(subset)
  pf <- parent.frame() 
  f <- function(r) eval(se, r, pf)

  i <-
    switch(aspect,
           egos = {
             f(x)
           },
           alters = {
             lapply(seq_len(nrow(x)), function(k){
               f(x[k,,drop=FALSE])
             })
           },
           ties = {
             lapply(seq_len(nrow(x)), function(k){
               f(x[i,,drop=FALSE])
             })
           })

  x[i,,aspect=aspect]
}

#' @rdname subset.egor
#'
#' @param i depends on `aspect`: \describe{
#'
#' \item{`"egos"`}{either an integer vector of indices specifying
#' which egos to select or a logical vector of length `nrow(x)`
#' specifying which rows should be kept.}
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
#' @param j either an integer vector specifying which columns of the
#'   filtered structure (egos, alters, or ties) to select, or a
#'   logical vector specifying which columns to keep.
#' 
#' @export
`[.egor` <- function(x, i, j, aspect = c("egos","alters","ties"), ...){
  aspect <- match.arg(aspect)

  switch(aspect,
         egos = {
           x <- x[i,j,drop=FALSE]
           attr(x, "design") <- attr(x, "design")[i,]
           x
         },
         alters = {
           x$.alters <- lapply(seq_len(nrow(x)), function(k){
             r$.alters[i[[k]],j,drop=FALSE]
           })
           x
         },
         ties = {
           x$.ties <- lapply(seq_len(nrow(x)), function(k){
             r$.ties[i[[k]],j,drop=FALSE]
           })
           x
         })
}
