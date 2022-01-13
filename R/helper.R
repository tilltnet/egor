if (getRversion() >= "2.15.1")
  utils::globalVariables(c(".egoID"))

#' General helper functions
#'
#' Helper functions for ego centered network analysis
#'
#' @template max_alters
#' @template directed
#' @param x \code{Numeric}.
#' @param y \code{Numeric}.
#' @param geometric `Logical.` Calculate possible dyads for geometric mean?
#' @name helper
NULL

#' @describeIn helper Converts an egor object to a "legacy" egor object with
#' nested .alts and .aaties columns.
#' @export
as_nested_egor <- function(x) {
  x$aatie <- select(x$aatie, .srcID, .tgtID, .egoID, everything())
  alters_l <- alters_by_ego(x)
  aaties_l <- aaties_by_ego(x)
  
  res <- x$ego
  
  if (has_ego_design(x)) {
    res$variables$.aaties <- aaties_l
    res$variables$.alts <- alters_l
  }
  else {
    res <- x$ego
    res$.aaties <- aaties_l
    res$.alts <- alters_l
  }
  
  class(res) <- c("nested_egor", class(res))
  res
}

#' Print method for nested_egor, making sure, that nested_egor objects with
#' ego_design get printed in a readable way
#' @noRd
#' @export
print.nested_egor <- function(x, ...) {
  if ("tbl_svy" %in% class(x)) {
    print(x$variables)
    NextMethod()
  }
  else
    NextMethod()
}

#' @describeIn helper Splits the alter table into a list of tables
#'   (possibly 0-row) of alters associated with each ego, in the same
#'   order as the ego table.
#' @export
alters_by_ego <- function(x)
  UseMethod("alters_by_ego")
#' @rdname helper
#' @export
alters_by_ego.egor <- function(x)
  split(x$alter, factor(x$alter$.egoID, levels = as_tibble(x$ego)$.egoID))
#' @rdname helper
#' @export
alters_by_ego.nested_egor <- function(x)
  as_tibble(x)$.alts

#' @describeIn helper Splits the alter--alter ties table into a list of
#'   tables (possibly 0-row) of alter--alter associated with each ego, in
#'   the same order as the ego table.
#' @export
aaties_by_ego <- function(x)
  UseMethod("aaties_by_ego")
#' @rdname helper
#' @export
aaties_by_ego.egor <- function(x)
  split(x$aatie, factor(x$aatie$.egoID, levels = as_tibble(x$ego)$.egoID))
#' @rdname helper
#' @export
aaties_by_ego.nested_egor <- function(x)
  as_tibble(x)$.aaties

#' @describeIn helper Returns the count of possible edges in an
#' un-directed or directed, ego-centered network, based on the number of alters.
#' @export
dyad.poss <- function(max.alters, directed = FALSE) {
  dp <- (max.alters ^ 2 - max.alters)
  if (!directed) {
    dp <- dp / 2
  }
  dp
}

#' @describeIn helper Generates a \code{data.frame} marking possible dyads in
#' a wide alter-alter relation \code{data.frame}. Row names corresponds to the
#' network size. This is useful for sanitizing alter-alter relations in the wide
#' format.
#' @export
sanitize.wide.edges <- function(max.alters) {
  x <- max.alters
  dp <- dyad.poss(x)
  
  names_ <- create_edge_names_wide(x)
  
  m <- matrix(0, nrow = x, ncol = x)
  m[lower.tri(m)] <- 99
  m1 <- m[, 1:(x - 1)]
  for (i in 2:x) {
    m1 <- cbind(m1, m[, i:(x - 1)])
  }
  m1 <- m1[, 1:dp]
  
  df <- data.frame(m1)
  names(df) <- names_
  df
}

#' @describeIn helper Creates a \code{vector} of names for variables
#' containing data on alter-alter relations/ dyads in ego-centered networks.
#' @export
create_edge_names_wide <- function(x) {
  leading.zeros.character <- paste("%0",
                                   nchar(as.character(x)),
                                   "d", sep = "")
  
  names_ <- c()
  for (i in 1:(x - 1)) {
    for (j in (i + 1):x) {
      i_ <- sprintf(leading.zeros.character, i)
      j_ <- sprintf(leading.zeros.character, j)
      names_ <- c(names_, paste(i_, j_, sep = " to "))
    }
  }
  names_
}

#' @describeIn helper Calculates the possible edges between members of
#' different groups in an ego-centered network.
#' @export
dyads_possible_between_groups <- function(x, y, geometric = TRUE) {
  if (geometric)
    sqrt(x * y)
  else
    x * y
}

#' @describeIn helper Calculates the optimal distribution of a number of
#' equally sized objects on a DIN-Norm DIN 476 (i.e. DIN A4) page in landscape
#' view.
#' @export
din_page_dist <- function(x) {
  for (yps in 2:x) {
    ix <- x / yps
    if (ix / yps <= sqrt(2)) {
      break()
    }
  }
  c(x = ceiling(ix), y = ceiling(yps))
}

#' Returns results inheriting `srvyr` design if the input egor object has a an
#' `ego_design` and global option "egor.return.results.with.design" is `TRUE` or
#' 'NULL'.
#' @param x Original `egor` object, as submitted in call to parent function.
#' @param results `data.frame` with `.egoID` column and a column that hold the
#' ego-level results.
return_results <-
  function(x, results) {
    
    join_results_with_design <-
      function(x, results) {
        if ("tbl_svy" %in% class(x$ego)) {
          a <- ego_design(x)
          a$variables <- dplyr::select(a$variables, .egoID)
          a$variables <- dplyr::left_join(a$variables, results, by = ".egoID")
          a
        } else
          results
      }
    
    errwd <- getOption("egor.results_with_design")
    
    if (is.null(errwd) | isTRUE(errwd)) {
      join_results_with_design(x, results)
    } else {
      results
    }
  }
