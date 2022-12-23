if (getRversion() >= "2.15.1")
  utils::globalVariables(c(".altRow"))

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
rowlist <- function(x) {
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
#'   via `ego$`, `alter$`, and `aatie$` as well as the following
#'   "virtual" columns to simplify indexing: \describe{
#'
#' \item{Ego index `.egoRow`}{ contains the index (counting from 1) of the row being
#' evaluated. (This can be used to access vector variables in the
#' calling environment.)}
#'
#' \item{Alter index `.altRow`}{ contains the index (counting from 1) of the row number in the alter table.}
#'
#' \item{Alter--alter indices `.srcRow` and `.tgtRow`}{ contain the
#' index (counting from 1) of the row of the alter being referenced by
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
#' # Using an external vector
#' # (though normally, we would use e[.keep,] here)
#' .keep <- rep(c(TRUE, FALSE), length.out=nrow(e$ego))
#' subset(e, .keep)
#' 
#'     # Filter egos
#' subset(x = egor32, subset = egor32$ego$variables$sex == "m", unit="ego")
#' subset(x = egor32, sex == "m")
#' 
#' # Filter alters
#' subset(x = egor32, sex == "m", unit = "alter")
#' 
#' # Filter aaties
#' subset(x = egor32, weight != 0, unit = "aatie")
#' 
#' # Filter egos by alter variables (keep only egos that have more than 13 alters)
#' subset(x = egor32, nrow(alter) > 13, unit = "alter")
#' 
#' # Filter alters by ego variables (keep only alters that have egos from Poland)
#' subset(x = egor32, ego$country == "Poland", unit = "ego")
#' 
#' # Filter edges by alter variables (keep only edges between alters where `sex == "m"`)
#' subset(x = egor32, all(alter$sex == "m"), unit = "aatie")
#' @importFrom methods is
#' @importFrom dplyr nest_join
#' @export
subset.egor <- function(x, subset, ..., unit = attr(x, "active")) {
  unit <- match.arg(unit, UNITS)
  
  f <- try(is.function(subset), silent = TRUE)
  if (is(f, "try-error") || !f) {
    # Does the `if` ever not fire?
    se <- substitute(subset)
    pf <- parent.frame()
    f <- function(r)
      eval(se, r, pf)
  } else
    f <- subset
  
  # The following works because we end up joining the same set of
  # tables to any of these, except that for unit=="aatie", we also
  # need to add row indices.
  
  add_egoRow <- function(x) {
    tmp <- as_tibble(x$ego)
    tmp$.egoRow <- seq_len(nrow(x$ego))
    tmp
  }
  
  add_altRow <- function(x) {
    tmp <- x$alter
    tmp$.altRow <-
      stats::ave(logical(nrow(tmp)), tmp$.egoID, FUN = seq_along)
    tmp
  }
  
  xa <- switch(
    unit,
    ego = add_egoRow(x),
    alter =
      # Within each ego, assign increasing alter
      # indices. Note that the first argument of ave() is a
      # dummy variable.
      add_altRow(x),
    aatie = {
      alters_with_altRow <-
        add_altRow(x) %>%
        select(.egoID, .altID, .altRow)
      xa <-
        left_join(x$aatie,
                  alters_with_altRow,
                  by = c(".egoID", ".srcID" = ".altID")) %>%
        rename(.srcRow = .altRow)
      
      left_join(xa,
                alters_with_altRow,
                by = c(".egoID", ".tgtID" = ".altID")) %>%
        rename(.tgtRow = .altRow)
    }
  )
  
  # Join nested ego column
  xa <-
    nest_join(xa,
              add_egoRow(x),
              ".egoID",
              keep = TRUE,
              name = "ego")
  
  # Join nested alter column
  by_alter <- switch(
    unit,
    ego = ".egoID",
    alter = c(".egoID", ".altID"),
    aatie = list(
      c(".egoID", ".srcID" = ".altID"),
      c(".egoID", ".tgtID" = ".altID")
    )
  )
  
  xa <-
    nest_join(xa,
              add_altRow(x),
              by_alter[[1]],
              keep = TRUE,
              name = "alter")
  
  if (unit == "aatie")  {
    xa <-
      nest_join(xa,
                add_altRow(x),
                by_alter[[2]],
                keep = TRUE,
                name = "alter2")
    xa$alter <- purrr::map2(xa$alter, xa$alter2, bind_rows)
    xa$alter2 <- NULL
  }
  
  # Join nested aatie column
  by_aatie <- switch(
    unit,
    ego = list(".egoID", ".egoID"),
    alter = list(
      c(".egoID", ".altID" = ".srcID"),
      c(".egoID", ".altID" = ".tgtID")
    ),
    aatie = list(
      c(".egoID", ".srcID" = ".srcID"),
      c(".egoID", ".tgtID" = ".tgtID")
    )
  )
  
  xa <-
    nest_join(xa, x$aatie, by_aatie[[1]], keep = TRUE, name = "aatie")
  
  if (unit != "ego") {
    xa <-
      nest_join(xa, x$aatie, by_aatie[[2]], keep = TRUE, name = "aatie2")
    xa$aatie <- purrr::map2(xa$aatie, xa$aatie2, bind_rows)
    xa$aatie2 <- NULL
  }

  # Call the function to perform indexing
  i <- sapply(rowlist(xa), f, ..., simplify = TRUE)

  # If `subset` is evaluated against the calling environment, we only need the lgl vector once:
  if (is.matrix(i))
    i <- i[, 1]
  
  # Subset x by i
  x[i, , unit = unit]
}

#' @rdname subset.egor
#'
#' @param i numeric or logical vector indexing the appropriate unit.
#'
#' @param j either an integer vector specifying which columns of the
#'   filtered structure (ego, alters, or ties) to select, or a logical
#'   vector specifying which columns to keep. Note that the special
#'   columns .egoID, .altID, .srcID, .tgtID are not indexed by `j`.
#   columns \Sexpr{sQuote(unlist(IDVARS))} are not indexed by `j`.
#'
#' @import tibble
#' @export
`[.egor` <- function(x, i, j, unit = attr(x, "active"), ...) {
  unit <- match.arg(unit, UNITS)
  if (missing(i))
    i <- TRUE
  if (missing(j))
    j <- TRUE
  
  switch(unit,
         ego = {
           eid <- as_tibble(x$ego)$.egoID
           if (is.numeric(i) && any(duplicated(i))) {
             warning(
               "Some ego indices have been selected multiple times. They will be duplicated, and ",
               sQuote(".egoID"),
               "s renumbered to preserve uniqueness."
             )
             x$alter <-
               map2(seq_along(i), alters_by_ego(x)[i], function(i, a) {
                 a[, ".egoID"] <- i
                 a
               }) %>% bind_rows
             
             if (!is.null(x$aatie))
               x$aatie <-
               map2(seq_along(i), aaties_by_ego(x)[i], function(i, aa) {
                 aa[, ".egoID"] <- i
                 aa
               }) %>% bind_rows
             
             # This guarantees that the ego ID column is always preserved.
             x$ego <- x$ego[i, j, ...]
             if (!".egoID" %in% names(as_tibble(x$ego))) {
               if (has_ego_design(x))
                 x$ego$variables$.egoID <- eid[i]
               else
                 x$ego$.egoID <- eid[i]
             }
             x$ego[, ".egoID"] <- seq_along(i)
             
           } else{
             # This guarantees that the ego ID column is always preserved.
             x$ego <- x$ego[i, j, ...]
             if (!".egoID" %in% names(as_tibble(x$ego))) {
               if (has_ego_design(x))
                 x$ego$variables$.egoID <- eid[i]
               else
                 x$ego$.egoID <- eid[i]
             }
             
             x$alter <-
               filter(x$alter, .egoID %in% as_tibble(x$ego)$.egoID)
             x$aatie <-
               filter(x$aatie, .egoID %in% as_tibble(x$ego)$.egoID)
           }
           x
         },
         alter = {
           if (!is.logical(i) && any(duplicated(i)))
             stop("Indexing duplicated alters is not implemented at this time.")
           
           eid <- x$alter$.egoID[i]
           aid <- x$alter$.altID[i]
           x$alter <- x$alter[i, j, ...]
           if (!".egoID" %in% names(x$alter))
             x$alter$.egoID <- eid
           if (!".altID" %in% names(x$alter))
             x$alter$.altID <- aid
           
           # Explanation: keep a row in aaties iff its (egoID,altID) tuple can (still) be found in the alters as well.
           trim_aaties(x)
         },
         aatie = {
           if (!utils::hasName(x, "aatie"))
             stop(
               "Attempted indexing of alter-alter ties on an object with no alter-alter ties observed."
             )
           eid <- x$aatie$.egoID[i]
           sid <- x$aatie$.srcID[i]
           tid <- x$aatie$.srcID[i]
           x$aatie <- x$aatie[i, j, ...]
           if (!".egoID" %in% names(x$aatie))
             x$aatie$.egoID <- eid
           if (!".srcID" %in% names(x$aatie))
             x$aatie$.srcID <- sid
           if (!".tgtID" %in% names(x$aatie))
             x$aatie$.tgtID <- tid
           
           x
         })
}
