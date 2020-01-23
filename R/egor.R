if (getRversion() >= "2.15.1") utils::globalVariables(c(":="))

#' egor - a data class for ego-centered network data.
#'
#' The function [egor()] is used to create an egor object from
#' ego-centered network data.
#' @param alters either a \code{data.frame} containing the alters
#'   (whose nominator is identified by the column specified by `egoID`
#'   or a list of data frames with the same columns, one for each ego,
#'   with empty data frames or `NULL`s corresponding to egos with no
#'   nominees.
#' @param egos \code{data.frame} containing the egos.
#' @param aaties \code{data.frame} containing the alter-alter
#'   relations in the style of an edge list, or a list of data frames
#'   similar to `alters.df`.
#' @template ID.vars
#' @param ego_design A [`list`] of arguments to [survey::svydesign()]
#'   specifying the sampling design for the egos. If formulas, they
#'   can refer to columns of `egos.df`.
#' @param alter_design A [`list`] of arguments specifying nomination
#'   information. Currently, the following elements are supported:
#'   \describe{\item{\code{"max"}}{Maximum number of alters that an
#'   ego can nominate.}}
#' @template meth_dots
#' 
#' @details If parameters `alters.df`, `egos.df`, and `aaties.df` are
#'   data frames, they need to share a common ego ID variable, with
#'   corresponding values. If `alters.df` and `aaties.df` are lists of
#'   data frames, `egoID` is ignored and they are matched positionally
#'   with the rows of `egos.df`. Of the three parameters only
#'   `alters.df` is necessary to create an `egor` object, and
#'   `egos.df` and `aaties.df` are optional.
#' @note Column names `.alts`, `.aaties`, and `.egoRow` are reserved
#'   for internal use of `egor` and should not be used to store
#'   persistent data. Other `.`-led column names may be reserved in
#'   the future.
#' @return Returns an [`egor`] object. An [`egor`] object is a [`tibble`] whose
#'   top-level columns store the ego attributes, and which has two
#'   special nested columns: `.alts`, containing, for each row (ego) a
#'   table of that ego's alter attributes and `.aaties`, a table
#'   containing that ego's alter--alter ties, if observed.
#'
#'   If alter-alter ties are observed, `.alts` also has a column
#'   `.altID` giving a unique (within each ego) ID of the alter, by
#'   which the alter can be identified in the `.aaties` table for that
#'   ego. `.aaties`, in turn, has columns `.srcID` and `.tgtID` that
#'   contain the source and the target of the alter-alter relation.
#'
#'   In addition, `egor` has two attributes: `ego_design`, containing an
#'   object returned by [survey::svydesign()] specifying the sampling
#'   design by which the egos were selected and `alter_design`, a
#'   [`list`] containing specification of how the alters were
#'   nominated. See the argument above for currently implemented
#'   settings.
#' @keywords ego-centered network analysis
#' @examples 
#' data("egos32")
#' data("alters32")
#' data("aaties32") 
#' 
#' egor(alters32, 
#'      egos32, 
#'      aaties32,
#'      ID.vars = list(ego = ".EGOID", 
#'                     alter = ".ALTID", 
#'                     source = ".SRCID",
#'                     target =  ".TGTID"))
#' @export
egor <- function(alters,
                    egos = NULL,
                    aaties = NULL,
                    ID.vars = list(
                      ego = "egoID",
                      alter = "alterID",
                      source = "Source",
                      target = "Target"
                    ),
                    ego_design = list( ~ 1),
                    alter_design = list(max = Inf)) {
  
  # Check for reserved column names
  
  check_reserved_colnames <-
    function(x, unit_) {
      if (!is.null(x))
        if (any(names(x) %in% RESERVED_COLNAMES))
          warning(paste0(
            unit_,
            " dataset uses reserved column name(s): ",
            paste(RESERVED_COLNAMES[RESERVED_COLNAMES %in% names(x)], 
                  collapse = " ")
          ),
          call. = FALSE)
    }
  
  mapply(check_reserved_colnames,
         list(egos, alters, aaties),
         UNITS)
  
  # Modify ID name list
  
  IDv <- modifyList(eval(formals()$ID.vars), ID.vars)
  
  # Alters
  
  if (!is_tibble(alters)) {
    alters <- as_tibble(alters)
  }
  
  alters <- select(alters,
                   !!IDVARS$alter := !!IDv$alter,
                   !!IDVARS$ego := !!IDv$ego,
                   everything())
  
  # Egos
  
  if (is.null(egos)) {
    egos <- tibble(.egoID = unique(alters[[IDVARS$ego]]))
  } else {
    if (!is_tibble(egos)) {
      egos <- as_tibble(egos)
    }
    egos <- select(egos,!!IDVARS$ego := !!IDv$ego, everything())
  }
  
  # Alter-Alter
  
  if (is.null(aaties)) {
    aaties <- tibble(.srcID = 0, .tgtID = 0)[0,]
  } else {
    if (!is_tibble(aaties)) {
      aaties <- as_tibble(aaties)
    }
    aaties <- select(
      aaties,
      !!IDVARS$ego := !!IDv$ego,
      !!IDVARS$source := !!IDv$source,
      !!IDVARS$target := !!IDv$target,
      everything()
    )
  }
  
  # Check ID consistency
  
  if (any(duplicated(egos[[IDVARS$ego]])))
    warning("Duplicated ego IDs in ego data.", 
            call. = FALSE)
  
  if (!all(alters[[IDVARS$ego]] %in% egos[[IDVARS$ego]]))
    warning("There is at least one ego ID in the alter data with no
            corresponding entry in the ego data.", 
            call. = FALSE)
  
  if (!all(c(aaties[[IDVARS$ego]] %in% egos[[IDVARS$ego]])))
    warning("There is at least one ego ID in the alter-alter data with no
            corresponding entry in the alter data.", 
            call. = FALSE)
  
  # Return
  
  egor <- list(ego = egos,
               alter = alters,
               aatie = aaties
  )
  class(egor) <- c("egor", class(egor))
  activate(egor, "ego")
}

#' Methods to print and summarize [`egor`] objects
#'
#' @param object,x an [`egor`] object.
#' @param ... additional arguments, either unused or passed to lower-level functions.
#' @param n Number of rows to print.
#' @docType methods
#' @method summary egor
#' @export
summary.egor <- function(object, ...) {
  
  # TODO: return tibble instead of using cat for output?!? 
  
  # Network count
  nc <- nrow(object$ego)
  
  # Min, Max  & Average netsize
  min_nts <- min(table(object$alter$.egoID))
  max_nts <- max(table(object$alter$.egoID))
  avg_nts <- mean(table(object$alter$.egoID))
  
  # Total number of alters
  alts_count <- nrow(object$alter)
  
  # Average density
  if ("aatie" %in% names(object)) 
    dens <- mean(ego_density(object = object), na.rm = TRUE)
  else
    dens <- NULL
  
  cat(paste(nc, "Egos/ Ego Networks",
            paste0( "\n", alts_count, " Alters"),
            "\nMin. Netsize", min_nts,
            "\nAverage Netsize", avg_nts,
            "\nMax. Netsize", max_nts, "\n"))
  if (!is.null(dens)) cat(paste("Average Density", dens, "\n"))

  # Meta Data
  cat("\nEgo sampling design:\n")
#' @importFrom utils capture.output
  writeLines(paste("  ", capture.output(print(attr(object, "ego_design"))), sep = ""))

  cat("Alter survey design:\n")
  cat("  Maximum nominations:", attr(object, "alter_design")$max,"\n")
}

#' @rdname summary.egor
#' @export
#' @method print egor
#' @import tibble
#' @importFrom dplyr group_vars
print.egor <- function(x, ..., n = 3) {
  class(x) <- "list"
  active_lgl <- attr(x, "active") == names(x)
  y <- c(x[active_lgl],
         x[!active_lgl])
  
  purrr::pwalk(list(y, names(y), c(TRUE, FALSE, FALSE)), function(x, y, z) {
    if (z)
      cat(paste0("# ", toupper(y), " data (active)", "\n"))
    else
      cat(paste0("# ", toupper(y), " data \n"))
    print(tibble::trunc_mat(x, n = n))
  })
  invisible(x)
}

#' @rdname egor
#' @param x an object to be coerced to [`egor`].
#' @export
as.egor <- function(x, ...) UseMethod("as.egor")

#' @export
#' @noRd
as.egor.egor <- function(x, ...) x


#' @method as_tibble egor
#' @export
as_tibble.egor <- function(x, 
                           ..., 
                           include.ego.vars = FALSE, 
                           include.alter.vars = FALSE){
  res <- x[[attr(x, "active")]]
  
  if (include.ego.vars & attr(x, "active") != "ego") {
    
    names(x$ego)[names(x$ego) != ".egoID"] <- 
      paste0(names(x$ego)[names(x$ego) != ".egoID"] , "_ego")
    
    
    res <- full_join(res, x$ego,
                     by = ".egoID")
  }
  
  if (include.alter.vars & attr(x, "active") == "aatie") {
    res <- left_join(res, 
                     x$alter, 
                     by = c(".egoID", ".srcID" = ".altID"))
    res <- left_join(res, 
                     x$alter, 
                     by = c(".egoID", ".tgtID" = ".altID"),
                     suffix = c("_src","_tgt"))
  }
  
  res
}

#' @method as.tibble egor
#' @export
as.tibble.egor <- as_tibble.egor
